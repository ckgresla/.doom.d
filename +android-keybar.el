;;; +android-keybar.el --- Gboard accessory keys for Android -*- lexical-binding: t; -*-

;; This file is loaded only by +android.el, but retain the guard so it is safe
;; to byte-compile or load the same Doom config on desktop systems.
(when (eq system-type 'android)
  (require 'tool-bar)

  (defconst my/android-keybar-image-root
    (expand-file-name "assets/android-keybar/" doom-user-dir))

  (defconst my/android-keybar-buttons
    '((escape "Esc" nil "esc")
      (tab "Tab" nil "tab")
      (control "Ctrl" control "ctrl")
      (shift "Shift" shift "shift")
      (meta "Meta" meta "meta")
      (alt "Alt" alt "alt")
      (super "Super" super "super")
      (movement "Mvmt" movement "mvmt")))

  (defvar my/android-keybar-armed-modifiers nil
    "Modifiers selected for the next input event.")

  (defvar my/android-keybar-locked-modifiers nil
    "Modifiers kept active until their toolbar button is tapped again.")

  (defvar my/android-keybar-movement-active nil
    "Non-nil when Mvmt, rather than Ctrl itself, owns the Control lock.")

  (defvar my/android-keybar-hidden nil
    "Non-nil when the Android modifier bar is completely hidden.")

  (defvar my/android-keybar-manually-hidden nil
    "Keep the bar hidden until the next detected keyboard opening.")
  (defvar my/android-keybar-keyboard-visible nil)
  (defvar my/android-keybar-visibility-timer nil)

  (defvar my/android-keybar-flashed-buttons nil
    "Literal key buttons currently showing momentary press feedback.")

  (defvar my/android-keybar-flash-timer nil)

  (defvar my/android-keybar-saved-translation nil)
  (defvar my/android-keybar-saved-conversion-override nil)
  (defvar my/android-keybar-scale 1.0)
  (defvar my/android-keybar-vertical-padding 55
    "Extra pixels above and below each key's clickable image.
Together with the fitted 80px images this keeps 190px targets on the Galaxy.
The source images themselves are now 1.5 times taller, without stretching
the key labels; padding alone would enlarge only the invisible touch target.")

  (defun my/android-keybar-image-scale ()
    "Fit the accessory keys on one row at the current frame width."
    (let ((width 0))
      (dolist (asset (append '("spacer.png" "light/collapse.png")
                             (mapcar (lambda (button)
                                       (format "light/idle/%s.png" (nth 3 button)))
                                     my/android-keybar-buttons)))
        (cl-incf width
                 (car (image-size
                       (create-image (expand-file-name asset my/android-keybar-image-root)
                                     'png nil :scale 1.0) t))))
      (min 1.0 (/ (float (max 1 (- (frame-pixel-width)
                                   (* 2 (frame-char-width)))))
                  (max 1 width)))))

  (defun my/android-keybar-dark-p ()
    (or (eq (frame-parameter nil 'background-mode) 'dark)
        (eq frame-background-mode 'dark)))

  (defun my/android-keybar-state (modifier name)
    (when (eq modifier 'movement)
      (setq modifier 'control))
    (cond ((member name my/android-keybar-flashed-buttons) 'armed)
          ((memq modifier my/android-keybar-locked-modifiers) 'locked)
          ((memq modifier my/android-keybar-armed-modifiers) 'armed)
          (t 'idle)))

  (defun my/android-keybar-image (name modifier)
    (create-image
     (expand-file-name
      (format "%s/%s/%s.png"
              (if (my/android-keybar-dark-p) "dark" "light")
              (my/android-keybar-state modifier name)
              name)
      my/android-keybar-image-root)
     'png nil :ascent 'center :scale my/android-keybar-scale))

  (defun my/android-keybar-hide-image ()
    (create-image
     (expand-file-name
      (format "%s/collapse.png"
              (if (my/android-keybar-dark-p) "dark" "light"))
      my/android-keybar-image-root)
     'png nil :ascent 'center :scale my/android-keybar-scale))

  (defun my/android-keybar-build-map ()
    (let ((map (make-sparse-keymap)))
      (define-key-after
        map [keybar-spacer]
        `(menu-item "" ignore
                    :enable nil
                    :image ,(create-image
                             (expand-file-name "spacer.png"
                                               my/android-keybar-image-root)
                             'png nil :ascent 'center
                             :scale my/android-keybar-scale)))
      (dolist (button my/android-keybar-buttons)
        (pcase-let ((`(,name ,label ,modifier ,asset) button))
          (define-key-after
            map (vector name)
            `(menu-item ,label ignore
                        :help ,(cond ((eq modifier 'movement)
                                     "Escape, then toggle persistent Control")
                                    (modifier
                                     (format "%s: tap once for the next key; twice to lock" label))
                                    (t "Escape: quit or cancel"))
                        :image ,(my/android-keybar-image
                                 asset modifier)))))
      (define-key-after
        map [collapse]
        `(menu-item "Hide key buttons"
                    ignore
                    :help "Hide key buttons until the keyboard is reopened"
                    :image ,(my/android-keybar-hide-image)))
      map))

  (defun my/android-keybar-refresh ()
    (set-frame-parameter nil 'my/android-keybar-width (frame-pixel-width))
    (setq my/android-keybar-scale (my/android-keybar-image-scale))
    (setq secondary-tool-bar-map (my/android-keybar-build-map))
    (force-mode-line-update t)
    (redisplay))

  (defun my/android-keybar-resize (frame)
    "Refit the bar after a resolution or orientation change on FRAME."
    (when (and (frame-live-p frame)
               (not (equal (frame-parameter frame 'my/android-keybar-width)
                           (frame-pixel-width frame))))
      (with-selected-frame frame (my/android-keybar-refresh))))
  (add-hook 'window-size-change-functions #'my/android-keybar-resize)

  (defun my/android-keybar-flash-button (name)
    "Briefly show literal button NAME in the blue pressed state."
    (when (timerp my/android-keybar-flash-timer)
      (cancel-timer my/android-keybar-flash-timer))
    (setq my/android-keybar-flashed-buttons (list name))
    (my/android-keybar-refresh)
    (setq my/android-keybar-flash-timer
          (run-at-time
           0.10 nil
           (lambda ()
             (setq my/android-keybar-flashed-buttons nil
                   my/android-keybar-flash-timer nil)
             (when (and (bound-and-true-p modifier-bar-mode)
                        (not my/android-keybar-hidden))
               (my/android-keybar-refresh))))))

  (defun my/android-keybar-apply-theme ()
    "Blend the native toolbar strip into the active Doom theme."
    (set-face-attribute 'tool-bar nil
                        :background (face-background 'default nil t)
                        :foreground (face-foreground 'default nil t)
                        :box nil)
    (my/android-keybar-refresh))

  (defun my/android-keybar-keyboard-event-p (event)
    (and event
         (not (memq (event-basic-type event)
                    '(tool-bar menu-bar scroll-bar mouse-movement
                      escape ignore return linefeed newline 10 13)))))

  (defun my/android-keybar-return-event-p (event)
    "Return non-nil when EVENT is one of Android's Return spellings."
    (or (memq event '(10 13 return linefeed newline))
        (memq (event-basic-type event)
              '(10 13 return linefeed newline))))

  (defun my/android-keybar-apply-modifiers (event modifiers)
    "Apply MODIFIERS to EVENT, leaving Return and UI events unchanged."
    (cond
     ;; Android may send the GUI event <return>, while Doom/Evil generally
     ;; bind RET (character 13).  Canonicalize every Return spelling to RET,
     ;; and never apply a held keybar modifier to it.  In particular, Enter
     ;; remains usable while the Mvmt layer keeps Control locked.
     ((my/android-keybar-return-event-p event)
      (my/android-keybar-exit-movement)
      13)
     ;; Unexpected Keyboard reports Shift+Tab as the canonical `backtab'
     ;; event.  Applying Shift as a character bit to ASCII TAB (9) would
     ;; incorrectly produce the letter I instead.
     ((or (eq event 9)
          (eq (event-basic-type event) 'tab))
      (if (memq 'shift modifiers) 'backtab event))
     ((my/android-keybar-keyboard-event-p event)
      (tool-bar-apply-modifiers event modifiers))
     (t event)))

  (defun my/android-keybar-translate-locked (_prompt)
    "Apply locked modifiers to the event currently being translated."
    (let ((event last-input-event))
      (vector
       (my/android-keybar-apply-modifiers
        event my/android-keybar-locked-modifiers))))

  (defun my/android-keybar-enable-lock-translation ()
    (unless my/android-keybar-saved-translation
      (setq my/android-keybar-saved-translation
            (cons t (lookup-key key-translation-map [t])))
      (define-key key-translation-map [t]
                  #'my/android-keybar-translate-locked)))

  (defun my/android-keybar-disable-lock-translation ()
    (when my/android-keybar-saved-translation
      (define-key key-translation-map [t]
                  (cdr my/android-keybar-saved-translation))
      (setq my/android-keybar-saved-translation nil)))

  (defun my/android-keybar-sync-lock ()
    (if my/android-keybar-locked-modifiers
        (progn
          (my/android-keybar-enable-lock-translation)
          (when (fboundp 'set-text-conversion-style)
            ;; Locks are global, but text-conversion-style belongs to a buffer.
            ;; Never restore one buffer's insertion style into another (Dired
            ;; must keep receiving command keys even after a buffer switch).
            (unless my/android-keybar-saved-conversion-override
              (setq my/android-keybar-saved-conversion-override
                    (cons t overriding-text-conversion-style)))
            (setq overriding-text-conversion-style nil)
            (set-text-conversion-style text-conversion-style t)))
      (my/android-keybar-disable-lock-translation)
      (when (and (fboundp 'set-text-conversion-style)
                 my/android-keybar-saved-conversion-override)
        (setq overriding-text-conversion-style
              (cdr my/android-keybar-saved-conversion-override)
              my/android-keybar-saved-conversion-override nil)
        (set-text-conversion-style text-conversion-style t))))

  (defun my/android-keybar-toggle-lock (modifier)
    (if (memq modifier my/android-keybar-locked-modifiers)
        (progn
          (setq my/android-keybar-locked-modifiers
                (delq modifier my/android-keybar-locked-modifiers))
          (when (eq modifier 'control)
            (setq my/android-keybar-movement-active nil)))
      (push modifier my/android-keybar-locked-modifiers))
    (setq my/android-keybar-armed-modifiers
          (delq modifier my/android-keybar-armed-modifiers))
    (my/android-keybar-sync-lock)
    (my/android-keybar-refresh))

  (defun my/android-keybar-read-modified-event (first-modifier)
    "Read one event after FIRST-MODIFIER, supporting chords and locks."
    (if (memq first-modifier my/android-keybar-locked-modifiers)
        (progn
          (my/android-keybar-toggle-lock first-modifier)
          [ignore])
      (let ((old-text-conversion-style text-conversion-style)
            (my/android-keybar-armed-modifiers (list first-modifier))
            (last-modifier first-modifier)
            event event-modifier)
        (when (fboundp 'set-text-conversion-style)
          (set-text-conversion-style nil))
        (unwind-protect
            (progn
              (frame-toggle-on-screen-keyboard nil nil)
              (my/android-keybar-refresh)
              (setq event (read-event))
              (while (eq event 'tool-bar)
                (setq event-modifier (event-basic-type (read-event)))
                (cond
                 ;; Allow literal toolbar keys to complete a modifier chord.
                 ;; The modifier application below converts Shift+9 to
                 ;; `backtab', matching Unexpected Keyboard.
                 ((eq event-modifier 'tab)
                  (my/android-keybar-flash-button "tab")
                  (setq event 9))
                 ((eq event-modifier 'escape)
                  (my/android-keybar-flash-button "esc")
                  (setq event 'escape))
                 ((memq event-modifier '(return linefeed newline 10 13))
                  (setq event 13))
                 ((eq event-modifier 'collapse)
                  (my/android-keybar-hide)
                  (setq event 'ignore))
                 ((not (memq event-modifier
                             '(alt super hyper shift control meta)))
                  (user-error "Unknown keybar event %s" event-modifier))
                 ((eq event-modifier last-modifier)
                  (my/android-keybar-toggle-lock event-modifier)
                  (setq event 'ignore))
                 (t
                  (unless (or (memq event-modifier
                                    my/android-keybar-armed-modifiers)
                              (memq event-modifier
                                    my/android-keybar-locked-modifiers))
                    (push event-modifier my/android-keybar-armed-modifiers))
                  (setq last-modifier event-modifier)
                  (my/android-keybar-refresh)
                  (setq event (read-event)))))
              (if (eq event 'ignore)
                  [ignore]
                (vector
                 (my/android-keybar-apply-modifiers
                  event my/android-keybar-armed-modifiers))))
          (setq my/android-keybar-armed-modifiers nil)
          (progn
            (when (and (fboundp 'set-text-conversion-style)
                       (not (eq old-text-conversion-style
                                text-conversion-style)))
              (set-text-conversion-style old-text-conversion-style t)))
          (my/android-keybar-refresh)))))

  (defun my/android-keybar-input-decoder (modifier)
    (lambda (_prompt)
      (my/android-keybar-read-modified-event modifier)))

  (defun my/android-keybar-literal-decoder (name event)
    "Return a decoder that flashes NAME and emits literal EVENT."
    (lambda (_prompt)
      (my/android-keybar-flash-button name)
      ;; Match the hardware key; context-specific behavior belongs in keymaps.
      (vector event)))

  (defun my/android-keybar-movement-decoder (_prompt)
    "Send Escape and toggle the persistent Control movement layer."
    (if my/android-keybar-movement-active
        (my/android-keybar-exit-movement)
      (unless (memq 'control my/android-keybar-locked-modifiers)
        (my/android-keybar-toggle-lock 'control))
      (setq my/android-keybar-movement-active t)
      (my/android-keybar-refresh))
    [escape])

  (defun my/android-keybar-exit-movement ()
    "Release Mvmt's Control lock, preserving every other locked modifier."
    (when my/android-keybar-movement-active
      (setq my/android-keybar-movement-active nil
            my/android-keybar-locked-modifiers
            (delq 'control my/android-keybar-locked-modifiers))
      (my/android-keybar-sync-lock)
      (my/android-keybar-refresh)))

  (defun my/android-keybar-hide (&optional automatic)
    "Hide the entire row.  AUTOMATIC does not set the manual Hide latch."
    (unless automatic (setq my/android-keybar-manually-hidden t))
    (setq my/android-keybar-hidden t)
    (modifier-bar-mode -1)
    ;; The secondary modifier bar is hosted by the native tool-bar container.
    ;; Disabling only `modifier-bar-mode' removes the buttons but leaves that
    ;; container as an empty full-height row.
    (tool-bar-mode -1)
    (force-mode-line-update t)
    (redisplay))

  (defun my/android-keybar-update-visibility (visible)
    "Apply detected keyboard VISIBLE state, preserving a manual Hide latch."
    (when (and visible (not my/android-keybar-keyboard-visible))
      (setq my/android-keybar-manually-hidden nil))
    (setq my/android-keybar-keyboard-visible visible)
    (if (and visible (not my/android-keybar-manually-hidden))
        (when my/android-keybar-hidden (my/android-keybar-install))
      (unless my/android-keybar-hidden (my/android-keybar-hide t))))

  (defun my/android-keybar-keyboard-geometry (height baseline visible)
    "Detect a docked keyboard from outer HEIGHT and closed BASELINE.
Use hysteresis to ignore system-bar changes and intermediate resize events.
VISIBLE is the previous state.  No inner window or toolbar sizes are used."
    (< height (- baseline (if visible
                              (max 48 (* 0.03 baseline))
                            (max 96 (* 0.15 baseline))))))

  (defun my/android-keybar-sync-visibility (&optional frame)
    "Follow docked keyboard visibility using FRAME's native resize events.
Emacs 31 does not expose Android's IME insets to Lisp.  This therefore targets
the normal full-screen, docked-keyboard layout, not floating IMEs or split-screen."
    (setq frame (or frame (selected-frame)))
    (when (and (frame-live-p frame) (frame-visible-p frame))
      (with-selected-frame frame
        (let* ((height (frame-pixel-height frame))
               (geometry (alist-get 'geometry (frame-monitor-attributes frame)))
               (layout (list (frame-pixel-width frame) geometry))
               (baselines (frame-parameter frame 'my/android-keybar-baselines))
               (baseline (alist-get layout baselines nil nil #'equal))
               (known-baseline baseline)
               (previous-visible (and baseline my/android-keybar-keyboard-visible)))
          ;; A new orientation has its own baseline.  Monitor height includes
          ;; system bars, but the opening threshold comfortably excludes them.
          (unless baseline
            (setq baseline (or (nth 3 geometry) height)))
          (let ((visible (my/android-keybar-keyboard-geometry
                          height baseline previous-visible)))
            ;; Never chase an opening animation downwards: a succession of
            ;; small height decreases must still cross the original baseline.
            (unless visible
              (setq baseline (if known-baseline (max baseline height) height)))
            ;; A physical-monitor estimate is not yet a measured closed frame.
            ;; Keep the wider threshold until we actually observe it closed.
            (when (or known-baseline (not visible))
              (setf (alist-get layout baselines nil nil #'equal) baseline))
            (set-frame-parameter frame 'my/android-keybar-baselines baselines)
            (my/android-keybar-update-visibility visible))))))

  (defun my/android-keybar-schedule-visibility (&optional frame)
    "Coalesce visibility changes outside redisplay's window-size hooks."
    (when (timerp my/android-keybar-visibility-timer)
      (cancel-timer my/android-keybar-visibility-timer))
    (setq my/android-keybar-visibility-timer
          (run-at-time 0 nil
                       (lambda (frame)
                         (setq my/android-keybar-visibility-timer nil)
                         (my/android-keybar-sync-visibility frame))
                       (or frame (selected-frame)))))

  (defun my/android-keybar-keyboard-request (frame _hide)
    "Sample FRAME before an IME request; the subsequent resize confirms it."
    (my/android-keybar-schedule-visibility frame))

  (defun my/android-keybar-hide-decoder (_prompt)
    "Hide the accessory row until the keyboard closes and reopens."
    (my/android-keybar-hide)
    [ignore])

  (defun my/android-keybar-clear-main-toolbar ()
    "Remove the ordinary toolbar globally and from existing buffers."
    (let ((empty-map (make-sparse-keymap)))
      (setq-default tool-bar-map empty-map)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (setq tool-bar-map empty-map)))))

  (defun my/android-keybar-install ()
    (setq my/android-keybar-hidden
          (not (and my/android-keybar-keyboard-visible
                    (not my/android-keybar-manually-hidden))))
    (setq tool-bar-button-margin (cons 0 my/android-keybar-vertical-padding)
          tool-bar-button-relief 0
          tool-bar-always-show-default t
          android-intercept-control-space t)
    (when (boundp 'tool-bar-position)
      (customize-set-variable 'tool-bar-position 'bottom))
    (tool-bar-mode (if my/android-keybar-hidden -1 1))
    (modifier-bar-mode (if my/android-keybar-hidden -1 1))
    ;; Decoder commands return this synthetic event when a toolbar action has
    ;; already done all its work.  Bind it explicitly so Doom consumes it
    ;; silently instead of reporting "<ignore> is undefined".
    (global-set-key [ignore] #'ignore)
    (my/android-keybar-clear-main-toolbar)
    (dolist (button my/android-keybar-buttons)
      (when-let ((modifier (nth 2 button)))
        (define-key input-decode-map
                    (vector 'tool-bar (car button))
                    (if (eq modifier 'movement)
                        #'my/android-keybar-movement-decoder
                      (my/android-keybar-input-decoder modifier)))))
    (define-key input-decode-map [tool-bar escape]
                (my/android-keybar-literal-decoder "esc" 'escape))
    ;; Match the terminal-style event sent by Unexpected Keyboard.  `[tab]'
    ;; is the distinct GUI <tab> event; character 9 is literal TAB / C-i.
    (define-key input-decode-map [tool-bar tab]
                (my/android-keybar-literal-decoder "tab" 9))
    (define-key input-decode-map [tool-bar collapse]
                #'my/android-keybar-hide-decoder)
    (my/android-keybar-apply-theme))

  (defun my/android-keybar-install-after-reload ()
    "Restore the keybar after late toolbar setup performed by Doom packages."
    (run-at-time 0.1 nil #'my/android-keybar-install))

  (my/android-keybar-install)
  ;; Remove the old request-only behavior when hot-reloading this file.
  (advice-remove #'frame-toggle-on-screen-keyboard
                 #'my/android-keybar-show-with-keyboard)
  (advice-add #'frame-toggle-on-screen-keyboard :before
              #'my/android-keybar-keyboard-request)
  (add-hook 'window-size-change-functions #'my/android-keybar-schedule-visibility)
  (add-function :after after-focus-change-function
                #'my/android-keybar-schedule-visibility)
  (my/android-keybar-schedule-visibility)
  (add-hook 'doom-load-theme-hook #'my/android-keybar-apply-theme)
  ;; Some packages repopulate `tool-bar-map' while Doom is reloading.  Run the
  ;; full installer once more at the end so only the Gboard accessory row
  ;; remains.
  (add-hook 'doom-after-reload-hook
            #'my/android-keybar-install-after-reload))

(provide '+android-keybar)
;;; +android-keybar.el ends here
