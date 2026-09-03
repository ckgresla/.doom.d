;;; +android.el --- Android-only config (Po Lu Emacs port) -*- lexical-binding: t; -*-
;;
;; Loaded from config.el only when running on the Android Emacs port, so the
;; same .doom.d works unchanged on macOS and Linux desktops.

;; ----- Termux <-> Android Emacs bridge -----------------------------
;; com.termux and org.gnu.emacs share a UID on this device, so Emacs can use
;; Termux binaries directly.  Do not export Termux's library directory:
;; Android commands then load incompatible libraries and `doom/reload' breaks.
(let* ((termux-prefix "/data/data/com.termux/files/usr")
       (termux-bin (concat termux-prefix "/bin")))
  (when (file-directory-p termux-bin)
    (add-to-list 'exec-path termux-bin)
    (setenv "PATH" (concat termux-bin ":" (or (getenv "PATH") "")))
    (setenv "LD_LIBRARY_PATH" nil)
    (unless (getenv "TMPDIR")
      (setenv "TMPDIR" (concat termux-prefix "/tmp")))))

;; Gboard accessory controls live in a separate Android-only module so their
;; implementation and image assets can be versioned with this Doom config.
(load! "+android-keybar")

;; A touchscreen press on the modeline can enter `mouse-drag-mode-line', but
;; Android occasionally omits the matching release event.  Emacs then waits in
;; the drag command forever and appears frozen, with "mode-line down-mouse-1-"
;; in the echo area.  Disable only drag-start on these narrow separators;
;; ordinary release clicks and all buffer-area touch gestures remain intact.
(global-set-key [mode-line down-mouse-1] #'ignore)
(global-set-key [bottom-divider down-mouse-1] #'ignore)

;; Emacs 31 receives Android's resolved UI_MODE_NIGHT value directly from the
;; Activity configuration.  Follow it at startup and on system theme changes.
(when (and (boundp 'toolkit-theme-set-functions)
           (fboundp 'my/apply-theme))
  (add-hook 'toolkit-theme-set-functions #'my/apply-theme)
  (when (memq toolkit-theme '(light dark))
    (my/apply-theme toolkit-theme)))

;; Android's child Emacs resets its working directory.  Pass Doom's launcher
;; as an absolute path so `doom/reload' still works.
(setq doom-reload-command
      (format "sh %s sync -B -e # %%s"
              (shell-quote-argument
               (expand-file-name "bin/doom" doom-emacs-dir))))

;; Doom's `+default/dired' can call this before dired.el is otherwise loaded on
;; this Emacs 31 build.  Register the upstream autoload explicitly.
(autoload 'dired-read-dir-and-switches "dired")

(after! dired
  (add-hook! 'dired-mode-hook
    (dired-hide-details-mode +1)
    ;; Keep the listing switches but do not spend the phone-width modeline on
    ;; displaying all of them.
    (setq-local mode-name "Dired")))

(after! dirvish
  (setq dirvish-hide-details t))

;; ----- Android notebook mode --------------------------------------
;; Keep Doom's stock `SPC t z' zen mode untouched.  This second, deliberately
;; independent mode is an easy place to iterate on phone-specific writing UI.
(defgroup my/android-notebook nil
  "A quiet, centered writing view for Android."
  :group 'convenience)

(defcustom my/android-notebook-width 80
  "Maximum body width, in columns, for Android notebook mode."
  :type 'integer)

(defcustom my/android-notebook-height 0.94
  "Relative text height for Android notebook mode."
  :type 'number)

(defcustom my/android-notebook-line-spacing 0.18
  "Extra line spacing used by Android notebook mode."
  :type 'number)

(defvar-local my/android-notebook--saved-state nil)
(defvar-local my/android-notebook--face-cookie nil)

(define-minor-mode my/android-notebook-mode
  "Toggle a centered, low-chrome writing view for the current buffer."
  :init-value nil
  :lighter " Note"
  (if my/android-notebook-mode
      (progn
        (require 'visual-fill-column)
        (setq my/android-notebook--saved-state
              (list :display-line-numbers display-line-numbers
                    :line-spacing line-spacing
                    :cursor-type cursor-type
                    :visual-fill-column-mode
                    (bound-and-true-p visual-fill-column-mode)
                    :mixed-pitch-mode
                    (bound-and-true-p mixed-pitch-mode)))
        (setq-local display-line-numbers nil
                    line-spacing my/android-notebook-line-spacing
                    cursor-type 'bar
                    visual-fill-column-width my/android-notebook-width
                    visual-fill-column-center-text t)
        (setq my/android-notebook--face-cookie
              (face-remap-add-relative 'default
                                       `(:height ,my/android-notebook-height)))
        ;; `mixed-pitch-mode' gives prose Inter while preserving JetBrains Mono
        ;; for code, tables, and other fixed-pitch faces.
        (when (fboundp 'mixed-pitch-mode)
          (mixed-pitch-mode 1))
        (visual-fill-column-mode 1))
    (when my/android-notebook--face-cookie
      (face-remap-remove-relative my/android-notebook--face-cookie)
      (setq my/android-notebook--face-cookie nil))
    (when my/android-notebook--saved-state
      (setq-local display-line-numbers
                  (plist-get my/android-notebook--saved-state
                             :display-line-numbers)
                  line-spacing
                  (plist-get my/android-notebook--saved-state :line-spacing)
                  cursor-type
                  (plist-get my/android-notebook--saved-state :cursor-type))
      (unless (plist-get my/android-notebook--saved-state
                         :visual-fill-column-mode)
        (visual-fill-column-mode -1))
      (when (and (fboundp 'mixed-pitch-mode)
                 (not (plist-get my/android-notebook--saved-state
                                 :mixed-pitch-mode)))
        (mixed-pitch-mode -1))
      (setq my/android-notebook--saved-state nil))))

(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Android notebook mode" "n" #'my/android-notebook-mode))

;; `recentf-cleanup' replaces Projectile's own status text, which makes
;; `SPC p i' look like a no-op on the phone.  Restore durable confirmation.
(after! projectile
  (defun my/android-projectile-invalidate-cache-feedback (&rest _)
    (message "Projectile cache invalidated."))
  (unless (advice-member-p #'my/android-projectile-invalidate-cache-feedback
                           #'projectile-invalidate-cache)
    (advice-add #'projectile-invalidate-cache :after
                #'my/android-projectile-invalidate-cache-feedback)))

;; ----- Android content:// materialization -------------------------
;; Android Intents appear as /content/... virtual paths readable only through
;; Emacs's file-name handler.  External renderers need a real private file.
(defvar-local my/android-document-cache-file nil
  "Materialized private path for an Android content URI, if any.")

(defun my/android-materialize-content-uri ()
  "Copy an Android /content/ buffer to a real file for subprocesses."
  (when (and (stringp buffer-file-name)
             (string-prefix-p "/content/" buffer-file-name))
    (let* ((src buffer-file-name)
           (cache-dir (expand-file-name "~/.cache/pdf-tools-content/"))
           (basename (or (file-name-nondirectory src) "untitled.pdf"))
           (dst (expand-file-name
                 (concat (format-time-string "%s-") basename)
                 cache-dir)))
      (make-directory cache-dir t)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (let ((coding-system-for-read 'no-conversion))
          (insert-file-contents-literally src))
        (let ((coding-system-for-write 'no-conversion))
          (write-region (point-min) (point-max) dst nil 'silent)))
      (set-visited-file-name dst t t)
      (rename-buffer (file-name-nondirectory dst) t)
      (set-buffer-modified-p nil)
      (setq my/android-document-cache-file dst)
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when (and my/android-document-cache-file
                             (file-exists-p my/android-document-cache-file))
                    (ignore-errors
                      (delete-file my/android-document-cache-file))))
                nil 'local))))

;; ----- pdf-tools on Android ---------------------------------------
;; Keep Doom's full PDF workflow (midnight mode, search, selection, continuous
;; scrolling, and its keymaps), while bounding the Android image allocations
;; that previously accumulated on every page render.
(setq pdf-info-epdfinfo-program (expand-file-name "~/.local/bin/epdfinfo")
      pdf-tools-handle-upgrades nil
      image-cache-eviction-delay 1)

(defvar my/pdf-native-image-gc-timer nil)

(defun my/pdf-release-old-native-image (orig-fn image &rest args)
  "Evict the previous Android image before displaying PDF IMAGE."
  (let* ((candidate (car args))
         (window (if (windowp candidate) candidate (selected-window)))
         (frame (and (window-live-p window) (window-frame window))))
    (when frame
      (clear-image-cache frame))
    (prog1 (apply orig-fn image args)
      (when (timerp my/pdf-native-image-gc-timer)
        (cancel-timer my/pdf-native-image-gc-timer))
      (setq my/pdf-native-image-gc-timer
            (run-with-idle-timer 0.35 nil #'garbage-collect)))))

(defvar my/pdf-wheel-last 0)
(defconst my/pdf-wheel-interval 0.18)

(defvar my/pdf-touch-stale-timer nil)

(defun my/pdf-touch-cancel-stale-timer ()
  "Cancel the watchdog for an incomplete Android PDF touch."
  (when (timerp my/pdf-touch-stale-timer)
    (cancel-timer my/pdf-touch-stale-timer)
    (setq my/pdf-touch-stale-timer nil)))

(defun my/pdf-touch-clear-stale-state ()
  "Clear an Android touch whose release event never arrived."
  (setq my/pdf-touch-stale-timer nil
        touch-screen-current-tool nil
        touch-screen-aux-tool nil)
  (when (timerp touch-screen-current-timer)
    (cancel-timer touch-screen-current-timer)
    (setq touch-screen-current-timer nil)))

(defun my/pdf-touch-begin (&rest _)
  "Return immediately from PDF touch-down and enable gesture detection."
  (interactive)
  ;; The touchscreen translator initially labels this a desktop mouse drag in
  ;; order to emit down-mouse-1 without waiting.  Reclassify the retained tool
  ;; so later motion becomes touchscreen-scroll and a clean release becomes
  ;; mouse-1.  Crucially, a missing Android release cannot block command_loop.
  (when touch-screen-current-tool
    (setcar (nthcdr 3 touch-screen-current-tool) nil))
  (my/pdf-touch-cancel-stale-timer)
  (setq my/pdf-touch-stale-timer
        (run-at-time 2 nil #'my/pdf-touch-clear-stale-state)))

(defun my/pdf-touch-tap (&rest _)
  "Turn a stationary PDF touch into a request to show Gboard."
  (interactive)
  (my/pdf-touch-cancel-stale-timer)
  (frame-toggle-on-screen-keyboard (selected-frame) nil))

(defun my/pdf-touch-scroll (event)
  "Navigate pdf-view in response to touchscreen scroll EVENT."
  (interactive "e")
  (my/pdf-touch-cancel-stale-timer)
  (let ((window (nth 1 event))
        (dy (nth 3 event)))
    (when (and (window-live-p window) (numberp dy) (not (zerop dy)))
      (with-selected-window window
        (if (> dy 0)
            (pdf-view-next-line-or-next-page 1)
          (pdf-view-previous-line-or-previous-page 1))))))

;; pdf-tools' desktop mouse bindings make Emacs's standard touch translator
;; synchronous: it reads ahead until release, and a release occasionally goes
;; missing on Android.  Handle raw events as separate commands in PDF buffers
;; instead, so the command loop never waits for the rest of a gesture.
(defvar-local my/pdf-raw-touch-points nil)
(defvar-local my/pdf-raw-touch-primary nil)
(defvar-local my/pdf-raw-touch-last-xy nil)
(defvar-local my/pdf-raw-touch-moved nil)
(defvar-local my/pdf-raw-touch-pinch-distance nil)
(defvar-local my/pdf-raw-touch-start-time 0)

(defun my/pdf-raw-touch-reset ()
  "Forget the current nonblocking PDF touch gesture."
  (setq my/pdf-raw-touch-points nil
        my/pdf-raw-touch-primary nil
        my/pdf-raw-touch-last-xy nil
        my/pdf-raw-touch-moved nil
        my/pdf-raw-touch-pinch-distance nil
        my/pdf-raw-touch-start-time 0))

(defun my/pdf-raw-touch-distance (posn-a posn-b)
  "Return the pixel distance between POSN-A and POSN-B."
  (pcase-let ((`(,ax . ,ay) (posn-x-y posn-a))
              (`(,bx . ,by) (posn-x-y posn-b)))
    (sqrt (+ (expt (- ax bx) 2) (expt (- ay by) 2)))))

(defun my/pdf-raw-touch-begin (event)
  "Record raw touchscreen-begin EVENT without reading ahead."
  (interactive "e")
  (let* ((point (cadr event))
         (id (car-safe point))
         (posn (cdr-safe point)))
    (when (and id posn)
      ;; Treat an old point with no release as stale when the next gesture
      ;; begins; it must not turn a future tap into an accidental pinch.
      (when (> (- (float-time) my/pdf-raw-touch-start-time) 2)
        (my/pdf-raw-touch-reset))
      (push (cons id posn) my/pdf-raw-touch-points)
      (if my/pdf-raw-touch-primary
          (let ((first (cdr (assq my/pdf-raw-touch-primary
                                  my/pdf-raw-touch-points))))
            (when first
              (setq my/pdf-raw-touch-pinch-distance
                    (my/pdf-raw-touch-distance first posn)
                    my/pdf-raw-touch-moved t)))
        (setq my/pdf-raw-touch-primary id
              my/pdf-raw-touch-last-xy (posn-x-y posn)
              my/pdf-raw-touch-moved nil
              my/pdf-raw-touch-start-time (float-time))))))

(defun my/pdf-raw-touch-update (event)
  "Convert raw touchscreen-update EVENT to PDF scroll or pinch."
  (interactive "e")
  (let ((updates (cadr event)))
    (dolist (point updates)
      (when-let ((cell (assq (car point) my/pdf-raw-touch-points)))
        (setcdr cell (cdr point))))
    (let ((active (delq nil
                        (mapcar (lambda (point)
                                  (assq (car point) my/pdf-raw-touch-points))
                                updates))))
      (if (and (cdr active) my/pdf-raw-touch-pinch-distance
               (> my/pdf-raw-touch-pinch-distance 0))
          (let* ((a (cdar active))
                 (b (cdr (cadr active)))
                 (window (posn-window a))
                 (ratio (/ (my/pdf-raw-touch-distance a b)
                           my/pdf-raw-touch-pinch-distance)))
            (setq my/pdf-raw-touch-moved t)
            (my/pdf-touch-pinch
             (list 'touchscreen-pinch window ratio)))
        (when-let* ((primary (assq my/pdf-raw-touch-primary active))
                    (posn (cdr primary))
                    (xy (posn-x-y posn)))
          (let ((dx (- (car my/pdf-raw-touch-last-xy) (car xy)))
                (dy (- (cdr my/pdf-raw-touch-last-xy) (cdr xy))))
            (setq my/pdf-raw-touch-last-xy xy)
            (when (or (> (abs dy) 10) (> (abs dx) (frame-char-width)))
              (setq my/pdf-raw-touch-moved t)
              (my/pdf-touch-scroll
               (list 'touchscreen-scroll (posn-window posn) dx dy)))))))))

(defun my/pdf-raw-touch-end (event)
  "Finish raw touchscreen-end EVENT; a stationary tap opens Gboard."
  (interactive "e")
  (let* ((point (cadr event))
         (id (car-safe point))
         (canceled (caddr event))
         (primary (eq id my/pdf-raw-touch-primary)))
    (setq my/pdf-raw-touch-points
          (assq-delete-all id my/pdf-raw-touch-points))
    (when (and primary (not canceled) (not my/pdf-raw-touch-moved))
      (my/pdf-touch-tap))
    (if my/pdf-raw-touch-points
        (when primary
          (setq my/pdf-raw-touch-primary (caar my/pdf-raw-touch-points)
                my/pdf-raw-touch-last-xy
                (posn-x-y (cdar my/pdf-raw-touch-points))
                my/pdf-raw-touch-moved t))
      (my/pdf-raw-touch-reset))))

(defun my/android-touch-event-window (event)
  "Return the window associated with raw touchscreen EVENT."
  (let ((posn (pcase (car-safe event)
                ((or 'touchscreen-begin 'touchscreen-end)
                 (cdr-safe (cadr event)))
                ('touchscreen-update
                 (cdr-safe (car-safe (cadr event)))))))
    (and posn (posn-window posn))))

(defun my/android-pdf-touch-event-p (event)
  "Return non-nil when raw touchscreen EVENT belongs to a PDF window."
  (let ((window (my/android-touch-event-window event)))
    (and (windowp window)
         (with-current-buffer (window-buffer window)
           (derived-mode-p 'pdf-view-mode)))))

(defun my/android-touch-translate (prompt)
  "Dispatch PDF touches without blocking; translate all others normally."
  (let* ((index (1- (length current-key-remap-sequence)))
         (event (aref current-key-remap-sequence index)))
    (if (my/android-pdf-touch-event-p event)
        (vector
         (cons (pcase (car event)
                 ('touchscreen-begin 'my/pdf-touchscreen-begin)
                 ('touchscreen-update 'my/pdf-touchscreen-update)
                 ('touchscreen-end 'my/pdf-touchscreen-end))
               (cdr event)))
      (touch-screen-translate-touch prompt))))

;; The ordinary mode maps are consulted only after function-key translation,
;; so dispatch at that layer.  Prefix-specific mappings for the mode line,
;; echo area, dividers, and toolbars remain stock Emacs mappings.
(require 'touch-screen)
(define-key function-key-map [touchscreen-begin]
            #'my/android-touch-translate)
(define-key function-key-map [touchscreen-update]
            #'my/android-touch-translate)
(define-key function-key-map [touchscreen-end]
            #'my/android-touch-translate)

(defun my/pdf-wheel-event-p (event)
  "Return non-nil when EVENT is wheel or touchscreen scrolling."
  (and (consp event)
       (memq (car event)
             '(wheel-up wheel-down wheel-left wheel-right mouse-wheel
               touchscreen-update touchscreen-end touchscreen-scroll))))

(defun my/pdf-wheel-throttle (orig-fn &rest args)
  "Coalesce dense touch-scroll calls to ORIG-FN without changing key input."
  (if (my/pdf-wheel-event-p last-input-event)
      (let ((now (float-time)))
        (when (> (- now my/pdf-wheel-last) my/pdf-wheel-interval)
          (setq my/pdf-wheel-last now)
          (apply orig-fn args)))
    (apply orig-fn args)))

(defun my/android-sync-pdf-midnight (&optional appearance)
  "Match open PDF buffers to Android APPEARANCE."
  (let ((dark (eq (or appearance toolkit-theme) 'dark)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'pdf-view-mode)
          (pdf-view-midnight-minor-mode (if dark 1 -1)))))))

(when (boundp 'toolkit-theme-set-functions)
  (add-hook 'toolkit-theme-set-functions #'my/android-sync-pdf-midnight))

(after! pdf-tools
  ;; Doom registers the same handlers, but keep them first even if DocView was
  ;; autoloaded earlier in the session.
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  (add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))
  ;; A limit of 1 is a pdf-tools special case that retains nothing and forces
  ;; needless rerenders; 2 keeps the current and adjacent compressed page.
  (setq pdf-cache-image-limit 2
        pdf-tools-enabled-modes
        (delq 'pdf-cache-prefetch-minor-mode pdf-tools-enabled-modes)))

(after! pdf-view
  ;; HiDPI scaling would render this 1344 px frame at roughly 2688 px wide.
  (setq-default pdf-view-use-scaling nil)
  (when (boundp 'pdf-cache-prefetch-pages-front-limit)
    (setq pdf-cache-prefetch-pages-front-limit 0))
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (setq-local pdf-view-use-scaling nil)
              ;; A stationary tap should summon Gboard.  The PDF-local
              ;; mouse-1 no-op below prevents that tap from selecting text.
              (setq-local touch-screen-display-keyboard t)
              (when (fboundp 'pdf-cache-prefetch-minor-mode)
                (pdf-cache-prefetch-minor-mode -1))
              (my/android-sync-pdf-midnight)))
  (unless (advice-member-p #'my/android-materialize-content-uri
                           #'pdf-view-mode)
    (advice-add 'pdf-view-mode :before
                #'my/android-materialize-content-uri))
  (unless (advice-member-p #'my/pdf-wheel-throttle
                           #'pdf-view-next-line-or-next-page)
    (advice-add 'pdf-view-next-line-or-next-page :around
                #'my/pdf-wheel-throttle))
  (unless (advice-member-p #'my/pdf-wheel-throttle
                           #'pdf-view-previous-line-or-previous-page)
    (advice-add 'pdf-view-previous-line-or-previous-page :around
                #'my/pdf-wheel-throttle))
  (unless (advice-member-p #'my/pdf-release-old-native-image
                           #'pdf-view-display-image)
    (advice-add 'pdf-view-display-image :around
                #'my/pdf-release-old-native-image)))

(defun my/android-pinch-event-ratio (event)
  "Extract the cumulative scale ratio from EVENT."
  (let ((ratio (pcase (car-safe event)
                 ('pinch (nth 4 event))
                 ((or 'touchscreen-pinch 'touch-screen-pinch)
                  (nth 2 event))
                 (_ 1.0))))
    (if (and (numberp ratio) (> ratio 0)) (float ratio) 1.0)))

;; Pinch is the only overridden pdf-view touch gesture.  Motion events update
;; state only; one bounded render happens after the gesture becomes idle.
(defconst my/pdf-pinch-idle-delay 0.22)
(defconst my/pdf-pinch-min-scale 0.6)
(defconst my/pdf-pinch-max-scale 2.5)
(defvar-local my/pdf-pinch-base-scale nil)
(defvar-local my/pdf-pinch-ratio 1.0)
(defvar-local my/pdf-pinch-timer nil)

(defun my/pdf-current-scale ()
  "Return the current pdf-view scale relative to the page width."
  (if (numberp pdf-view-display-size)
      (float pdf-view-display-size)
    (let ((image-size (pdf-view-image-size))
          (page-size (pdf-cache-pagesize (pdf-view-current-page))))
      (/ (float (car image-size)) (float (car page-size))))))

(defun my/pdf-apply-pinch (buffer)
  "Apply the last debounced pinch to pdf-view BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'pdf-view-mode)
        (let ((target (max my/pdf-pinch-min-scale
                           (min my/pdf-pinch-max-scale
                                (* my/pdf-pinch-base-scale
                                   my/pdf-pinch-ratio)))))
          (setq pdf-view-display-size target
                my/pdf-pinch-base-scale nil
                my/pdf-pinch-ratio 1.0
                my/pdf-pinch-timer nil)
          (pdf-view-redisplay t))))))

(defun my/pdf-touch-pinch (event)
  "Debounce touchscreen EVENT into one bounded pdf-view resize."
  (interactive "e")
  (unless my/pdf-pinch-base-scale
    (setq my/pdf-pinch-base-scale (my/pdf-current-scale)))
  (setq my/pdf-pinch-ratio (my/android-pinch-event-ratio event))
  (when (timerp my/pdf-pinch-timer)
    (cancel-timer my/pdf-pinch-timer))
  (setq my/pdf-pinch-timer
        (run-with-idle-timer my/pdf-pinch-idle-delay nil
                             #'my/pdf-apply-pinch (current-buffer))))

;; Restore Emacs's standard pinch-to-text-scale gesture outside image buffers.
(setq touch-screen-display-pinch-to-zoom t)
(when (fboundp 'text-scale-pinch)
  (define-key global-map [pinch] #'text-scale-pinch))
(when (fboundp 'touch-screen-pinch)
  ;; Emacs 30/31's native translator emits `touchscreen-pinch'.  Retain the
  ;; legacy spelling as an alias for Android builds that used it previously.
  (define-key global-map [touchscreen-pinch] #'touch-screen-pinch)
  (define-key global-map [touch-screen-pinch] #'touch-screen-pinch))

(after! pdf-view
  (map! :map pdf-view-mode-map
        [down-mouse-1] #'my/pdf-touch-begin
        [mouse-1] #'my/pdf-touch-tap
        [drag-mouse-1] #'ignore
        [double-mouse-1] #'my/pdf-touch-tap
        [triple-mouse-1] #'my/pdf-touch-tap
        [my/pdf-touchscreen-begin] #'my/pdf-raw-touch-begin
        [my/pdf-touchscreen-update] #'my/pdf-raw-touch-update
        [my/pdf-touchscreen-end] #'my/pdf-raw-touch-end
        [touchscreen-scroll] #'my/pdf-touch-scroll
        [mode-line down-mouse-1] #'ignore
        [bottom-divider down-mouse-1] #'ignore
        [pinch] #'my/pdf-touch-pinch
        [touchscreen-pinch] #'my/pdf-touch-pinch
        [touch-screen-pinch] #'my/pdf-touch-pinch
        ;; Evil's normal-state auxiliary map outranks the major-mode map and
        ;; pdf-tools/evil-collection repeats the desktop selection bindings
        ;; there.  Override that layer too so the touch translator actually
        ;; sees the nonblocking Android handler on the rendered PDF canvas.
        :n [down-mouse-1] #'my/pdf-touch-begin
        :n [mouse-1] #'my/pdf-touch-tap
        :n [drag-mouse-1] #'ignore
        :n [double-mouse-1] #'my/pdf-touch-tap
        :n [triple-mouse-1] #'my/pdf-touch-tap
        :n [my/pdf-touchscreen-begin] #'my/pdf-raw-touch-begin
        :n [my/pdf-touchscreen-update] #'my/pdf-raw-touch-update
        :n [my/pdf-touchscreen-end] #'my/pdf-raw-touch-end
        :n [touchscreen-scroll] #'my/pdf-touch-scroll
        :n [pinch] #'my/pdf-touch-pinch
        :n [touchscreen-pinch] #'my/pdf-touch-pinch
        :n [touch-screen-pinch] #'my/pdf-touch-pinch))

;; ----- native image viewing + touch zoom --------------------------
(defconst my/image-pinch-idle-delay 0.12)
(defconst my/image-pinch-min-scale 0.1)
(defconst my/image-pinch-max-scale 4.0)
(defvar-local my/image-pinch-base-scale nil)
(defvar-local my/image-pinch-ratio 1.0)
(defvar-local my/image-pinch-timer nil)

(defun my/image-apply-pinch (buffer)
  "Apply the last debounced pinch to image BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'image-mode)
        (let ((target (max my/image-pinch-min-scale
                           (min my/image-pinch-max-scale
                                (* my/image-pinch-base-scale
                                   my/image-pinch-ratio)))))
          (setq my/image-pinch-base-scale nil
                my/image-pinch-ratio 1.0
                my/image-pinch-timer nil)
          (image-transform-set-scale target))))))

(defun my/image-touch-pinch (event)
  "Debounce touchscreen EVENT into one bounded image resize operation."
  (interactive "e")
  (unless my/image-pinch-base-scale
    (setq my/image-pinch-base-scale (float image-transform-scale)))
  (setq my/image-pinch-ratio (my/android-pinch-event-ratio event))
  (when (timerp my/image-pinch-timer)
    (cancel-timer my/image-pinch-timer))
  (setq my/image-pinch-timer
        (run-with-idle-timer my/image-pinch-idle-delay nil
                             #'my/image-apply-pinch (current-buffer))))

(after! image-mode
  (map! :map image-mode-map
        [pinch] #'my/image-touch-pinch
        [touchscreen-pinch] #'my/image-touch-pinch
        [touch-screen-pinch] #'my/image-touch-pinch
        :n [pinch] #'my/image-touch-pinch
        :n [touchscreen-pinch] #'my/image-touch-pinch
        :n [touch-screen-pinch] #'my/image-touch-pinch))

;; Keep the line-number menu entry out of the compact Android menu.
(when (fboundp 'menu-bar--display-line-numbers-mode-none)
  (menu-bar--display-line-numbers-mode-none))
