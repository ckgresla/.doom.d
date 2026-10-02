;;; +android-dashboard.el --- Quiet Android landing screen -*- lexical-binding: t; -*-

(when (eq system-type 'android)
  (require 'bookmark)
  (require 'button)
  (require 'color)
  (require 'seq)

  (defvar my/android-dashboard-bookmark-gap 3.0
    "Blank lines between the wordmark and bookmark blocks, rounded to an integer.")

  (defun my/android-dashboard-push-button (&optional event)
    "Activate the touched button using EVENT, not the dashboard's old point."
    ;; last-input-event can still be the raw touchscreen-end, while the
    ;; translated command event contains the correct clicked text position.
    (interactive (list (if (integerp last-command-event)
                           (point) last-command-event)))
    (push-button event))

  (defun my/android-dashboard-background-tap (event)
    "Activate a block at EVENT, or open the keyboard after a background tap."
    (interactive "e")
    (unless (my/android-dashboard-push-button event)
      (frame-toggle-on-screen-keyboard nil nil)))

  (defvar my/android-dashboard-button-map
    (let ((map (copy-keymap button-map)))
      ;; Explicit touch/mouse handling avoids Evil's drag command and Doom's
      ;; generic remap of push-button.  Keyboard activation remains standard.
      (define-key map [down-mouse-1] #'ignore)
      (define-key map [mouse-1] #'my/android-dashboard-push-button)
      (define-key map [mouse-2] #'my/android-dashboard-push-button)
      (define-key map [touchscreen-down] #'my/android-dashboard-push-button)
      map))

  (defun my/android-dashboard-block-background ()
    "Choose a theme-native block color distinct from the canvas.
Minimal themes may use the same background for every UI face.  In that
case, mix ten percent of the theme foreground into its background."
    (let* ((background (face-background 'default nil t))
           (base (color-name-to-rgb background)))
      (or (seq-some
           (lambda (face)
             (when-let* ((color (and (facep face) (face-background face nil t)))
                         (rgb (color-name-to-rgb color)))
               (unless (equal rgb base) color)))
           '(hl-line mode-line-inactive))
          (when-let* ((foreground (color-name-to-rgb
                                   (face-foreground 'default nil t)))
                      (_ base))
            (apply #'color-rgb-to-hex
                   (append (seq-mapn (lambda (bg fg) (+ (* 0.9 bg) (* 0.1 fg)))
                                     base foreground)
                           '(2))))
          background)))

  (defun my/android-dashboard-recent-bookmarks ()
    "Return at most five bookmark names, newest saved/edited first.
Use Emacs's built-in `last-modified' ordering, not access history.  Older
bookmarks without timestamps retain their creation order at the end.
This only reads bookmarks; it neither changes nor saves their records."
    (bookmark-maybe-load-default-file)
    (let ((bookmark-sort-flag 'last-modified))
      (seq-take (delete-dups (mapcar #'car (bookmark-maybe-sort-alist))) 5)))

  (defun my/android-dashboard-bookmark-action (button)
    "Visit the bookmark represented by BUTTON using its normal handler."
    (bookmark-jump (button-get button 'my/android-dashboard-bookmark)))

  (defun my/android-dashboard-bookmark-button (name width)
    "Return a flat text button for NAME with WIDTH character cells.
Truncate only its display label; activation always uses the full name."
    (let* ((label (truncate-string-to-width
                   (replace-regexp-in-string "[\n\r\t]" " " name)
                   (- width 4) nil nil "…"))
           (background (my/android-dashboard-block-background))
           ;; Same-colored vertical padding makes a flat block, not an outline.
           (face `(:inherit default :foreground ,(face-foreground 'default nil t)
                   :background ,background :underline nil
                   :box (:line-width (0 . 8) :color ,background))))
      (with-temp-buffer
        (insert-text-button
         (concat "  " label (make-string (- width 2 (string-width label)) ?\s))
         'action #'my/android-dashboard-bookmark-action
         'keymap my/android-dashboard-button-map
         'my/android-dashboard-bookmark name
         'face face 'mouse-face 'highlight 'follow-link t
         'help-echo (concat "Open bookmark: " name))
        (buffer-string))))

  (defun my/android-dashboard-bookmarks ()
    "Insert a quiet, centered stack of recent bookmark links, when available."
    (when-let* ((names (my/android-dashboard-recent-bookmarks)))
      (let* ((window (or (get-buffer-window (current-buffer)) (selected-window)))
             (available (max 8 (- (floor (/ (window-body-width window t)
                                             (float (window-font-width window))))
                                  4)))
             (width (min available 32
                         (max 20 (+ 4 (apply #'max (mapcar #'string-width names)))))))
        ;; Real lines keep Android's touch positions aligned with the rendered
        ;; glyphs; oversized display spaces can disagree with hit testing.
        (insert (make-string (max 0 (round my/android-dashboard-bookmark-gap)) ?\n))
        (dolist (name names)
          (let ((button (my/android-dashboard-bookmark-button name width)))
            (if (fboundp '+dashboard-insert)
                (+dashboard-insert button)
              (insert button "\n")))
          (insert (propertize "\n" 'line-height 0.3))))))

  (defun my/android-dashboard-wordmark ()
    "Draw a theme-native wordmark and up to five recent bookmark links."
    ;; Doom can install its dashboard modeline after the mode-creation hook,
    ;; and this buffer may already exist when our Android config is loaded.
    ;; Reapply the buffer-local preference whenever its contents are rebuilt.
    (my/android-dashboard-quiet-mode)
    ;; Keep the blank padding and text on the same theme background.
    (when (bound-and-true-p solaire-mode) (solaire-mode -1))
    ;; A space rather than an empty line survives Doom's padding cleanup.
    (insert (propertize " " 'my/android-dashboard-spacer t) "\n")
    (dolist (line (list (propertize "D O O M"
                                    'face '(:inherit default :weight bold :height 1.4))
                       (propertize "E M A C S"
                                   'face '(:inherit default :weight normal :height 1.0))))
      (if (fboundp '+dashboard-insert)
          (+dashboard-insert line)
        (insert line "\n")))
    (my/android-dashboard-bookmarks))

  (defun my/android-dashboard-quiet-mode ()
    "Hide dashboard chrome without changing the global modeline preference."
    ;; Showing the keyboard on touch-down resizes/recenters the dashboard
    ;; before touch-up, so the same finger no longer points at its button.
    ;; Background taps open it on release instead; button taps navigate.
    (setq-local touch-screen-display-keyboard nil)
    (local-set-key [mouse-1] #'my/android-dashboard-background-tap)
    (when (require 'hide-mode-line nil t)
      (hide-mode-line-mode 1)))
  (add-hook '+dashboard-mode-hook #'my/android-dashboard-quiet-mode)
  (add-hook '+doom-dashboard-mode-hook #'my/android-dashboard-quiet-mode)

  (defun my/android-dashboard-resize-in-buffer (original &rest args)
    "Keep ORIGINAL's dashboard point restoration out of the editing buffer.
Doom's resize hook restores `+dashboard-last-position' with `goto-char'
before switching into the dashboard buffer.  Keyboard/keybar resizes can
therefore move an unrelated buffer's insertion point when a dashboard is
visible in another window.  Give that hook the buffer it intends to edit;
do not undo legitimate cursor movements in ordinary editing commands."
    (if-let* ((buffer (get-buffer (if (boundp '+dashboard-name)
                                    +dashboard-name "*doom*"))))
        (with-current-buffer buffer
          (apply original args))
      (apply original args)))

  (defun my/android-dashboard-center-wordmark (&rest _)
    "Center the dashboard with ordinary, touch-safe blank lines.
Measure both content and a plain line using the buffer's current font
remapping.  Whole-line padding keeps Android hit testing aligned with the
rendered buttons, unlike one oversized display-space glyph."
    (when-let* ((buffer (get-buffer (if (boundp '+dashboard-name)
                                      +dashboard-name "*doom*")))
                (window (get-buffer-window buffer)))
      (with-current-buffer buffer
        (when (get-text-property (point-min) 'my/android-dashboard-spacer)
          (save-excursion
            (with-silent-modifications
              ;; Also migrate a dashboard that predates this padding scheme
              ;; when the config is hot-reloaded without rebuilding its text.
              (remove-text-properties (point-min) (1+ (point-min)) '(display nil))
              (goto-char (point-min))
              (forward-line 1)
              (let* ((padding-start (point))
                     (content-start
                      (if (get-text-property padding-start 'my/android-dashboard-padding)
                          (next-single-property-change
                           padding-start 'my/android-dashboard-padding nil (point-max))
                        padding-start))
                     (line-height
                      (max 1 (cdr (window-text-pixel-size
                                   window (point-min) padding-start nil nil nil t))))
                     (content-height
                      (cdr (window-text-pixel-size
                            window content-start (point-max) nil nil nil t)))
                     ;; The permanent space/newline anchor already takes one
                     ;; line and prevents Doom from stripping our top padding.
                     (padding-lines
                      (max 0 (1- (round (/ (- (window-body-height window t)
                                               content-height)
                                            (* 2.0 line-height)))))))
                (unless (= padding-lines (- content-start padding-start))
                  (delete-region padding-start content-start)
                  (insert-before-markers
                   (propertize (make-string padding-lines ?\n)
                               'my/android-dashboard-padding t
                               'rear-nonsticky t))))))))))

  (when (boundp '+dashboard-functions)
    (setq +dashboard-functions '(my/android-dashboard-wordmark)
          +dashboard-anchor '(top . center))
    (advice-add '+dashboard-resize-h :around #'my/android-dashboard-resize-in-buffer)
    (advice-add '+dashboard-resize-h :after #'my/android-dashboard-center-wordmark))
  ;; Older Doom installations use the previous module name.
  (when (boundp '+doom-dashboard-functions)
    (setq +doom-dashboard-functions '(my/android-dashboard-wordmark))))

(provide '+android-dashboard)
