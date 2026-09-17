;;; +android-pdf.el --- Android PDF rendering safety -*- lexical-binding: t; -*-

(require 'cl-lib)

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
            (run-with-idle-timer 0.35 nil #'garbage-collect)))))

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
