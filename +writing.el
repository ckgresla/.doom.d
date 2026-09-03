;;; +writing.el --- Shared writing views -*- lexical-binding: t; -*-

(require 'face-remap)

(defgroup my/writing nil "Shared writing views." :group 'convenience)
(defcustom my/writing-hide-modeline
  (not (member (downcase (or (getenv "DOOM_WRITING_HIDE_MODELINE") "1"))
               '("0" "false" "no")))
  "Hide the status bar in writing views.  Environment default is on."
  :type 'boolean :group 'my/writing)
(defcustom my/writing-hide-hl-line
  (not (member (downcase (or (getenv "DOOM_WRITING_HIDE_HL_LINE") "1"))
               '("0" "false" "no")))
  "Disable current-line highlighting in writing views."
  :type 'boolean :group 'my/writing)

(defvar-local my/writing-style nil)
(defvar-local my/writing--saved nil)
(defvar-local my/writing--font-cookie nil)

(defconst my/writing--variables
  '(display-line-numbers indicate-empty-lines truncate-lines word-wrap
    mode-line-format global-hl-line-mode
    visual-fill-column-width visual-fill-column-center-text
    visual-fill-column-extra-text-width))
(defconst my/writing--modes
  '(visual-line-mode visual-fill-column-mode vi-tilde-fringe-mode
    variable-pitch-mode mixed-pitch-mode hl-line-mode))

(defun my/writing--set-margins (original window)
  "Center writing views using rendered font pixels, not unscaled columns."
  (with-current-buffer (window-buffer window)
    (if (not my/writing-style)
        (funcall original window)
      (let* ((cell (frame-char-width (window-frame window)))
             (old (window-margins window))
             (available (+ (window-body-width window t)
                           (* cell (+ (or (car old) 0) (or (cdr old) 0)))))
             (target (* 80 (window-font-width window)))
             (margin (max 0 (ceiling (/ (- available target) (* 2.0 cell))))))
        (set-window-margins window margin margin)))))

(defun my/writing--resize (&rest _)
  "Recompute all visible writing columns after changing text size."
  (when my/writing-style
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (visual-fill-column--adjust-window window))))

(with-eval-after-load 'visual-fill-column
  (advice-add 'visual-fill-column--set-margins :around #'my/writing--set-margins))
;; Interactive increase/decrease and pinch call `text-scale-mode' directly,
;; bypassing `text-scale-set'.  Its hook covers all of these zoom paths.
(advice-remove 'text-scale-set #'my/writing--resize)
(add-hook 'text-scale-mode-hook #'my/writing--resize)

(defun my/writing--resize-all ()
  "Refresh writing columns after a frame-wide font change."
  (dolist (frame (frame-list))
    (dolist (window (window-list frame 'no-minibuffer))
      (with-current-buffer (window-buffer window)
        (when my/writing-style
          (visual-fill-column--adjust-window window))))))
(add-hook 'after-setting-font-hook #'my/writing--resize-all)

(defun my/writing--before-zen (&optional arg)
  "Leave the custom view before entering Doom's ordinary zen mode."
  (when (and my/writing-style
             (or (null arg) (> (prefix-numeric-value arg) 0)))
    (my/writing-restore)))

(with-eval-after-load 'writeroom-mode
  (advice-add 'writeroom-mode :before #'my/writing--before-zen))

(defun my/writing-restore ()
  "Restore the buffer's presentation from before its writing view."
  (when my/writing--saved
    (when my/writing--font-cookie
      (face-remap-remove-relative my/writing--font-cookie)
      (setq my/writing--font-cookie nil))
    (dolist (entry (plist-get my/writing--saved :modes))
      (when (fboundp (car entry))
        (funcall (car entry) (if (cdr entry) 1 -1))))
    (dolist (entry (plist-get my/writing--saved :variables))
      (if (nth 1 entry)
          (set (make-local-variable (car entry)) (nth 2 entry))
        (kill-local-variable (car entry))))
    (setq my/writing--saved nil my/writing-style nil)
    (when (bound-and-true-p visual-fill-column-mode)
      (visual-fill-column-adjust))
    (force-window-update (current-buffer))))

(defun my/writing-toggle (style)
  "Toggle STYLE, either `mono' or `variable', without changing text scale."
  (if (eq my/writing-style style)
      (my/writing-restore)
    (require 'visual-fill-column)
    ;; Each view starts from the original presentation, never another view's
    ;; temporary settings.  Disabling Doom zen normally resets text scale;
    ;; retain the user's current zoom when entering our custom views.
    (let ((scale text-scale-mode-amount))
      (my/writing-restore)
      (when (bound-and-true-p writeroom-mode) (writeroom-mode -1))
      (when (bound-and-true-p olivetti-mode) (olivetti-mode -1))
      (text-scale-set scale))
    (unless my/writing--saved
      (setq my/writing--saved
            (list :variables
                  (mapcar (lambda (var)
                            (list var (local-variable-p var)
                                  (and (boundp var) (symbol-value var))))
                          my/writing--variables)
                  :modes
                  (mapcar (lambda (mode)
                            (cons mode (and (boundp mode) (symbol-value mode))))
                          my/writing--modes))))
    ;; Remove pitch modes' family/height remappings; preserve text-scale-mode
    ;; and all unrelated face remappings (including the previous zen scale).
    (dolist (mode '(mixed-pitch-mode variable-pitch-mode vi-tilde-fringe-mode))
      (when (and (fboundp mode) (boundp mode) (symbol-value mode))
        (funcall mode -1)))
    (when my/writing--font-cookie
      (face-remap-remove-relative my/writing--font-cookie))
    (setq my/writing--font-cookie
          (face-remap-add-relative
           'default :family
           (face-attribute (if (eq style 'mono) 'fixed-pitch 'variable-pitch)
                           :family nil t)))
    (setq-local display-line-numbers nil
                indicate-empty-lines nil
                visual-fill-column-width 80
                visual-fill-column-center-text t
                visual-fill-column-extra-text-width '(0 . 0))
    (when my/writing-hide-modeline
      (setq-local mode-line-format nil))
    (when my/writing-hide-hl-line
      (setq-local global-hl-line-mode nil)
      (when (fboundp 'hl-line-mode) (hl-line-mode -1))
      (when (fboundp 'global-hl-line-unhighlight)
        (global-hl-line-unhighlight)))
    (setq my/writing-style style)
    (visual-line-mode 1)
    (visual-fill-column-mode 1)
    (visual-fill-column-adjust)
    (force-window-update (current-buffer))))

(defun my/writing-monospace ()
  "Toggle the centered, 80-column monospace writing view."
  (interactive)
  (my/writing-toggle 'mono))

(defun my/writing-variable-pitch ()
  "Toggle the centered writing view using the configured variable font."
  (interactive)
  (my/writing-toggle 'variable))

(map! :leader
      :desc "Monospace writing view" "t x" #'my/writing-monospace
      :desc "Variable-font writing view" "t n" #'my/writing-variable-pitch)

(provide '+writing)
