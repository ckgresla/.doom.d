;;; +android-pdf.el --- Android PDF rendering safety -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar my/android-pdf-momentum-enabled t
  "Whether an Android PDF flick continues briefly after finger release.")
(defvar-local my/android-pdf-motion-samples nil)
(defvar-local my/android-pdf-motion-pinched nil)
(defvar-local my/android-pdf-motion-window nil)
(defvar-local my/android-pdf-momentum-timer nil)
(defvar-local my/android-pdf-momentum-velocity nil)
(defvar-local my/android-pdf-momentum-time nil)

(defun my/android-pdf-stop-momentum ()
  "Stop this buffer's glide immediately, without changing its position."
  (when (timerp my/android-pdf-momentum-timer)
    (cancel-timer my/android-pdf-momentum-timer))
  (setq my/android-pdf-momentum-timer nil
        my/android-pdf-momentum-velocity nil))

(defun my/android-pdf-motion-sample (posn)
  "Record POSN using device event time, not delayed Lisp processing time."
  (let ((stamp (posn-timestamp posn))
        (xy (posn-x-y posn)))
    (when (and (numberp stamp) (consp xy)
               (numberp (car xy)) (numberp (cdr xy)))
      (push (list stamp (car xy) (cdr xy)) my/android-pdf-motion-samples)
      (setq my/android-pdf-motion-samples
            (cl-remove-if (lambda (sample) (> (- stamp (car sample)) 120))
                          my/android-pdf-motion-samples)))))

(defun my/android-pdf-motion-begin (event)
  "Cancel gliding and start measuring a fresh single-finger EVENT."
  (my/android-pdf-stop-momentum)
  (if (and (bound-and-true-p my/pdf-raw-touch-points)
           (<= (- (float-time) my/pdf-raw-touch-start-time) 2))
      (setq my/android-pdf-motion-pinched t)
    (setq my/android-pdf-motion-samples nil
          my/android-pdf-motion-pinched nil
          my/android-pdf-motion-window (posn-window (cdr (cadr event))))
    (my/android-pdf-motion-sample (cdr (cadr event)))))

(defun my/android-pdf-motion-update (event)
  "Measure the primary finger in EVENT; never derive a flick from a pinch."
  (when-let* ((point (assq my/pdf-raw-touch-primary (cadr event))))
    (my/android-pdf-motion-sample (cdr point))))

(defun my/android-pdf-release-velocity (release-time)
  "Return recent finger velocity as (DX . DY), or nil for a pause/tap.
RELEASE-TIME and recorded positions use the device's millisecond clock."
  (let* ((new (car my/android-pdf-motion-samples))
         (old (car (last my/android-pdf-motion-samples)))
         (dt (and old new (- (car new) (car old)))))
    (when (and dt (>= dt 16) (numberp release-time)
               (<= 0 (- release-time (car new)) 100))
      (let* ((vx (* 1000.0 (/ (- (nth 1 old) (nth 1 new)) (float dt))))
             (vy (* 1000.0 (/ (- (nth 2 old) (nth 2 new)) (float dt))))
             (speed (sqrt (+ (* vx vx) (* vy vy))))
             (scale (min 1.0 (/ 4000.0 (max speed 1.0)))))
        (when (> speed 180)
          (cons (* vx scale) (* vy scale)))))))

(defun my/android-pdf-momentum-step (buffer window)
  "Advance BUFFER in WINDOW by elapsed time, then exponentially slow down."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (not (and my/android-pdf-momentum-velocity
                    (window-live-p window) (eq window (selected-window))
                    (eq buffer (window-buffer window))
                    (bound-and-true-p pdf-view-roll-minor-mode)))
          (my/android-pdf-stop-momentum)
        (let* ((now (float-time))
               ;; A slow render must not turn into a huge catch-up jump.
               (dt (max 0.0 (min 0.05 (- now my/android-pdf-momentum-time))))
               (decay (exp (* -5.5 dt)))
               (vx (car my/android-pdf-momentum-velocity))
               (vy (cdr my/android-pdf-momentum-velocity))
               (before (list (image-mode-window-get 'page window)
                             (image-mode-window-get 'vscroll window)
                             (image-mode-window-get 'my/android-pdf-pan-state window))))
          (setq my/android-pdf-momentum-time now)
          (condition-case nil
              (progn
                (my/android-pdf-scroll-pixels (round (* vy dt)) window
                                              (* vx dt))
                (setq my/android-pdf-momentum-velocity
                      (cons (* vx decay) (* vy decay)))
                (when (or (< (max (abs vx) (abs vy)) 60)
                          (equal before
                                 (list (image-mode-window-get 'page window)
                                       (image-mode-window-get 'vscroll window)
                                       (image-mode-window-get 'my/android-pdf-pan-state window))))
                  (my/android-pdf-stop-momentum)))
            (error (my/android-pdf-stop-momentum)))
          ;; One-shot scheduling avoids a repeating timer's catch-up burst
          ;; after a page render blocks the command loop.
          (when my/android-pdf-momentum-velocity
            (setq my/android-pdf-momentum-timer
                  (run-at-time (/ 1.0 60) nil #'my/android-pdf-momentum-step
                               buffer window))))))))

(defun my/android-pdf-motion-end (original event)
  "Finish EVENT normally, then glide only after an uncanceled single flick."
  (let ((velocity (and my/android-pdf-momentum-enabled
                       (not my/android-pdf-motion-pinched)
                       (not (caddr event))
                       (bound-and-true-p my/pdf-raw-touch-moved)
                       (my/android-pdf-release-velocity
                        (posn-timestamp (cdr (cadr event))))))
        (window my/android-pdf-motion-window))
    (prog1 (funcall original event)
      (when (and velocity (not (bound-and-true-p my/pdf-raw-touch-points))
                 (window-live-p window)
                 (bound-and-true-p pdf-view-roll-minor-mode))
        (my/android-pdf-stop-momentum)
        (setq my/android-pdf-momentum-velocity velocity
              my/android-pdf-momentum-time (float-time)
              my/android-pdf-momentum-timer
              (run-at-time (/ 1.0 60) nil
                           #'my/android-pdf-momentum-step (current-buffer) window))
        (add-hook 'pre-command-hook #'my/android-pdf-stop-momentum nil t)
        (add-hook 'kill-buffer-hook #'my/android-pdf-stop-momentum nil t)
        (add-hook 'change-major-mode-hook #'my/android-pdf-stop-momentum nil t)))))

(defun my/android-pdf-retain-neighbors (original page &optional window force scrolling)
  "Keep already-rendered immediate neighbors; never eagerly render extra pages.
This bounds native memory while avoiding decode/render churn on reversals."
  (let* ((window (or window (selected-window)))
         (shown (funcall original page window force scrolling)))
    (unless force
      (dolist (neighbor (list (1- (apply #'min shown))
                             (1+ (apply #'max shown))))
        (when (<= 1 neighbor (pdf-cache-number-of-pages))
          (when (my/android-pdf-image-spec
                 (overlay-get (pdf-roll-page-overlay neighbor window) 'display))
            (push neighbor shown)))))
    shown))

(defun my/android-pdf-idle-image-gc ()
  "Collect retired image objects only after touch and momentum have settled."
  (setq my/pdf-native-image-gc-timer nil)
  (if (cl-some (lambda (buffer)
                 (with-current-buffer buffer
                   (or my/android-pdf-momentum-timer
                       (and (bound-and-true-p my/pdf-raw-touch-points)
                            (< (- (float-time) my/pdf-raw-touch-start-time) 2)))))
               (buffer-list))
      (setq my/pdf-native-image-gc-timer
            (run-with-timer 2 nil #'my/android-pdf-idle-image-gc))
    (if (and (current-idle-time)
             (>= (float-time (current-idle-time)) 2))
        (garbage-collect)
      (setq my/pdf-native-image-gc-timer
            (run-with-idle-timer 2 nil #'my/android-pdf-idle-image-gc)))))

(defconst my/android-pdf-max-render-pixels (* 8 1024 1024)
  "Maximum pixels in a pinch-rendered page (32 MiB of RGBA pixels).")

(defun my/android-pdf-limit-scale (scale page-size)
  "Limit SCALE for PAGE-SIZE in PDF points to the Android pixel budget."
  (min scale
       (sqrt (/ (float my/android-pdf-max-render-pixels)
                (* (float (car page-size)) (cdr page-size))))))

(defun my/android-pdf-enable-roll ()
  "Use stacked PDF pages where the installed PDF-tools supports them."
  (when (and (require 'pdf-roll nil t)
             (fboundp 'pdf-view-roll-minor-mode))
    (pdf-view-roll-minor-mode 1)))

(defun my/android-pdf-pan-pixels (dx window)
  "Pan PDF WINDOW horizontally by DX pixels, within the rendered page.
Emacs hscroll uses frame columns, so retain sub-column motion in window-local
image properties.  The saved hscroll must also change or roll redraw undoes it."
  (when (and (window-live-p window) (numberp dx))
    (with-selected-window window
      (let* ((char-width (max 1 (frame-char-width (window-frame window))))
             (current (window-hscroll window))
             (state (image-mode-window-get 'my/android-pdf-pan-state window))
             (origin (if (and (eq current (car state))
                              (equal char-width (nth 2 state)))
                         (nth 1 state)
                       (* current char-width)))
             (limit (max 0 (- (car (pdf-view-image-size t window))
                              (window-body-width window t))))
             (pixels (max 0 (min limit (+ origin dx))))
             ;; Reach the final partial column at the right edge, too.
             (columns (if (= pixels limit)
                          (ceiling pixels char-width)
                        (round pixels char-width))))
        (setq columns (set-window-hscroll window columns))
        (image-mode-window-put 'hscroll columns window)
        (image-mode-window-put 'my/android-pdf-pan-state
                               (list columns pixels char-width) window)
        (unless (= current columns)
          (force-window-update window))))))

(defun my/android-pdf-roll-clamp-pan (window)
  "Clamp WINDOW's horizontal pan after roll redraw, zoom, or resize."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (when (bound-and-true-p pdf-view-roll-minor-mode)
        (my/android-pdf-pan-pixels 0 window)))))

(defun my/android-pdf-scroll-pixels (dy window &optional dx)
  "Scroll PDF WINDOW by signed pixel distances DY and optional DX."
  (when (and (window-live-p window) (numberp dy) (not (zerop dy)))
    (with-selected-window window
      (if (bound-and-true-p pdf-view-roll-minor-mode)
          ;; Always pass positive distances: the two commands choose direction.
          (if (> dy 0)
              (pdf-roll-scroll-forward dy window t)
            (pdf-roll-scroll-backward (- dy) window t))
        (if (> dy 0)
            (pdf-view-next-line-or-next-page 1)
          (pdf-view-previous-line-or-previous-page 1)))))
  (when (numberp dx)
    (my/android-pdf-pan-pixels dx window)))

(defun my/android-pdf-roll-page-height (page window)
  "Return PAGE's full rendered height and track its image for retirement."
  (let ((height (max 1 (pdf-roll-display-page page window))))
    (image-mode-window-put
     'displayed-pages
     (cons page (remq page (image-mode-window-get 'displayed-pages window)))
     window)
    height))

(defun my/android-pdf-roll-scroll (distance window)
  "Move WINDOW by signed pixel DISTANCE through full PDF page heights.
Use the stored page/offset, not point or clipped redisplay geometry: a tall
image can extend below the viewport, and several touch events can arrive
before redisplay updates `window-start'.  Preserve the remainder when
crossing pages in either direction, including pages of different heights."
  (setq window (or window (selected-window)))
  (with-selected-window window
    (let* ((page (image-mode-window-get 'page window))
           (last-page (pdf-cache-number-of-pages))
           (offset (+ (or (image-mode-window-get 'vscroll window) 0)
                      distance))
           (height (my/android-pdf-roll-page-height page window)))
      (while (and (< offset 0) (> page 1))
        (cl-decf page)
        (setq height (my/android-pdf-roll-page-height page window))
        (cl-incf offset height))
      (while (and (>= offset height) (< page last-page))
        (cl-decf offset height)
        (cl-incf page)
        (setq height (my/android-pdf-roll-page-height page window)))
      ;; Stop at the bottom of the final page, without scrolling it away.
      (setq offset (max 0 (min offset
                               (if (= page last-page)
                                   (max 0 (- height (window-body-height window t)))
                                 (1- height)))))
      (image-mode-window-put 'page page window)
      (pdf-roll-set-vscroll offset window)
      (let ((start (pdf-roll-page-to-pos page)))
        (set-window-start window start t)
        (set-window-point window start))
      (force-window-update window))))

(defun my/android-pdf-roll-scroll-forward (&optional n window pixels)
  "Scroll N lines or PIXELS forward using full PDF page dimensions."
  (interactive "p")
  (my/android-pdf-roll-scroll
   (* (or n 1) (if pixels 1 (frame-char-height (window-frame window))))
   window))

(defun my/android-pdf-roll-scroll-backward (&optional n window pixels)
  "Scroll N lines or PIXELS backward using full PDF page dimensions."
  (interactive "p")
  (my/android-pdf-roll-scroll-forward (- (or n 1)) window pixels))

(defun my/android-pdf-image-spec (display)
  "Extract an image specification from a possibly sliced DISPLAY property."
  (cond ((eq (car-safe display) 'image) display)
        ((eq (car-safe (car-safe display)) 'slice) (cadr display))))

(defun my/android-pdf-flush-retired-images (images frame)
  "Flush IMAGES detached from this buffer's roll overlays on FRAME.
Keep images still used by another page/window, rather than clearing the whole
native cache and repeatedly decoding the visible neighboring pages."
  (let ((visible
         (delq nil
               (mapcar
                (lambda (overlay)
                  (let ((window (overlay-get overlay 'window)))
                    (when (and (eq (overlay-get overlay 'category) 'pdf-roll)
                               (window-live-p window)
                               (eq (window-frame window) frame))
                      (my/android-pdf-image-spec
                       (overlay-get overlay 'display)))))
                (overlays-in (point-min) (point-max)))))
        flushed)
    (dolist (image (delete-dups (delq nil images)))
      (unless (member image visible)
        (image-flush image frame)
        (setq flushed t)))
    (when flushed
      (when (timerp my/pdf-native-image-gc-timer)
        (cancel-timer my/pdf-native-image-gc-timer))
      (setq my/pdf-native-image-gc-timer
            (run-with-idle-timer 2 nil #'my/android-pdf-idle-image-gc)))))

(defvar my/pdf-native-image-gc-timer nil)

(defun my/android-pdf-roll-pos-overlay (pos window)
  "Find only a roll page/margin overlay at POS belonging to WINDOW.
Upstream also accepts unrelated window-local overlays (e.g. a temporary
hl-line highlight), then overwrites their display properties with a PDF."
  (cl-find-if (lambda (overlay)
                (and (eq (overlay-get overlay 'window) window)
                     (memq (overlay-get overlay 'category)
                           '(pdf-roll pdf-roll-margin))))
              (overlays-at pos)))

(defun my/android-pdf-roll-redisplay (orig-fn &optional window)
  "Preserve PDF-tools' all-windows redisplay convention for roll mode.
Midnight/theme changes pass t, including from a different selected buffer."
  (if (eq window t)
      (dolist (win (get-buffer-window-list nil nil t))
        (funcall orig-fn win))
    (funcall orig-fn window)))

(defun my/android-pdf-release-roll-pages (orig-fn pages &optional window)
  "Release native images after ORIG-FN removes PAGES from WINDOW."
  (let* ((window (or window (selected-window)))
         (images (mapcar
                  (lambda (page)
                    (my/android-pdf-image-spec
                     (overlay-get (pdf-roll-page-overlay page window) 'display)))
                  pages)))
    (prog1 (funcall orig-fn pages window)
      (when images
        (my/android-pdf-flush-retired-images images (window-frame window))))))

(defun my/android-pdf-replace-roll-image (orig-fn image page &optional window inhibit)
  "Release the old image after ORIG-FN replaces PAGE in WINDOW."
  (let* ((window (or window (selected-window)))
         (old (my/android-pdf-image-spec
               (overlay-get (pdf-roll-page-overlay page window) 'display))))
    (prog1 (funcall orig-fn image page window inhibit)
      (when old
        (my/android-pdf-flush-retired-images (list old) (window-frame window))))))

(when (eq system-type 'android)
  (advice-add 'my/pdf-raw-touch-begin :before #'my/android-pdf-motion-begin)
  (advice-add 'my/pdf-raw-touch-update :before #'my/android-pdf-motion-update)
  (advice-add 'my/pdf-raw-touch-end :around #'my/android-pdf-motion-end)
  (with-eval-after-load 'pdf-view
    ;; pdf-tools' image hotspots feed raw touchscreen events to desktop mouse
    ;; proxies.  Their event layouts differ; on Emacs 31.1 this produces a
    ;; key-lookup/replay loop before our PDF gesture handlers can run.
    ;; Set the default before the first image is created, not just in the
    ;; mode hook.  Keep pdf-links/pdf-annot modes and their keyboard commands;
    ;; the canvas is for tap-to-keyboard, swiping, and pinching on Android.
    (setq-default pdf-view-inhibit-hotspots t)
    (add-hook 'pdf-view-mode-hook #'my/android-pdf-enable-roll t))
  (with-eval-after-load 'pdf-roll
    (advice-add 'pdf-roll-display-pages :around #'my/android-pdf-retain-neighbors)
    (advice-add 'pdf-roll-scroll-forward :override
                #'my/android-pdf-roll-scroll-forward)
    (advice-add 'pdf-roll-scroll-backward :override
                #'my/android-pdf-roll-scroll-backward)
    (advice-add 'pdf-roll-pre-redisplay :after
                #'my/android-pdf-roll-clamp-pan)
    (advice-add 'pdf-roll-redisplay :around
                #'my/android-pdf-roll-redisplay)
    (advice-add 'pdf-roll--pos-overlay :override
                #'my/android-pdf-roll-pos-overlay)
    (advice-add 'pdf-roll-undisplay-pages :around
                #'my/android-pdf-release-roll-pages)
    (advice-add 'pdf-roll-display-image :around
                #'my/android-pdf-replace-roll-image)))

(provide '+android-pdf)
;;; +android-pdf.el ends here
