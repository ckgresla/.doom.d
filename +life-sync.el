;;; +life-sync.el --- Explicit, repository-scoped Life sync -*- lexical-binding: t; -*-

(require 'cl-lib)
(defvar my/life-sync-push t
  "Push after a successful Life sync. Never force-push.
Set to nil to keep sync checkpoints local instead.")
(defvar my/life-sync-include-new nil
  "Offer to include untracked files in Life sync, with explicit confirmation.")
(defvar my/life-sync-process nil)
(defconst my/life-sync-script
  (expand-file-name "scripts/life-sync.sh"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defvar-local my/life-sync-window-configuration nil
  "Window layout to restore when leaving the full-window sync log.")

(define-derived-mode my/life-sync-mode special-mode "Life Sync"
  "Read-only Life sync output.  Press q to restore the previous layout.")

(defun my/life-sync-quit ()
  "Restore the previous layout without killing the sync log or its process."
  (interactive)
  (let ((buffer (current-buffer))
        (configuration my/life-sync-window-configuration))
    (setq my/life-sync-window-configuration nil)
    (if (and (window-configuration-p configuration)
             (eq (window-configuration-frame configuration) (selected-frame)))
        (progn
          (set-window-configuration configuration)
          (bury-buffer buffer))
      (quit-window))))

(define-key my/life-sync-mode-map (kbd "q") #'my/life-sync-quit)
(define-key my/life-sync-mode-map [remap quit-window] #'my/life-sync-quit)

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/life-sync-mode 'motion)
  (evil-define-key '(normal motion) my/life-sync-mode-map
    (kbd "q") #'my/life-sync-quit))

(defun my/life-sync-show-log (buffer)
  "Select BUFFER in a full-size ordinary window on the current frame."
  (let* ((frame (selected-frame))
         (configuration
          (or (and (eq (window-buffer) buffer)
                   (with-current-buffer buffer
                     (and (window-configuration-p my/life-sync-window-configuration)
                          (eq (window-configuration-frame my/life-sync-window-configuration)
                              frame)
                          my/life-sync-window-configuration)))
              (current-window-configuration)))
         (window (cl-find-if
                  (lambda (candidate)
                    (and (not (window-parameter candidate 'window-side))
                         (not (window-dedicated-p candidate))))
                  (window-list frame 'nomini))))
    (unless window (user-error "No ordinary window available for Life sync"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'my/life-sync-mode)
        (my/life-sync-mode)))
    (select-window window)
    ;; Prefer the main window even when an old log is already in a side popup.
    ;; The overriding action wins over Doom's popup rules for this call only.
    (let ((display-buffer-overriding-action
           '((display-buffer-same-window) (inhibit-same-window . nil)))
          (ignore-window-parameters t))
      (pop-to-buffer buffer)
      (delete-other-windows))
    (setq my/life-sync-window-configuration configuration)))

(defun my/life-sync ()
  "Save, fetch, checkpoint tracked edits, rebase, and normally push ~/life.
Refuse an existing staged index or in-progress Git operation. A failed rebase
is aborted, retaining fetched refs and the local checkpoint without pushing.
Push only when `my/life-sync-push' is non-nil, and never force. Show Git output
in *Life sync*; all repository changes are explicit in that log."
  (interactive)
  (let* ((root (file-truename (expand-file-name "~/life/")))
         (here (and (not (file-remote-p default-directory))
                    (locate-dominating-file default-directory ".git"))))
    (unless (and here (equal (file-truename here) root))
      (user-error "Life sync is only available inside ~/life"))
    (when (process-live-p my/life-sync-process)
      (user-error "Life sync is already running"))
    (let ((in-life (lambda ()
                     (and buffer-file-name
                          (file-in-directory-p buffer-file-name root)))))
      (save-some-buffers nil in-life)
      (when (cl-some (lambda (buffer)
                       (with-current-buffer buffer
                         (and (buffer-modified-p) (funcall in-life))))
                     (buffer-list))
        (user-error "Save or discard modified Life buffers before syncing")))
    (let* ((default-directory root)
           (buffer (get-buffer-create "*Life sync*"))
           (include-new
            (and my/life-sync-include-new
                 (yes-or-no-p "Include ALL new, non-ignored Life files in this sync? "))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)) (erase-buffer)))
      (my/life-sync-show-log buffer)
      (setq my/life-sync-process
            (make-process
             :name "life-sync" :buffer buffer :noquery t
             :connection-type 'pipe
             :command (list shell-file-name my/life-sync-script root
                            (if (eq system-type 'android) "galaxy" "hackbook")
                            (if my/life-sync-push "yes" "no")
                            (if include-new "yes" "no"))
             :sentinel
             (lambda (process _event)
               (when (memq (process-status process) '(exit signal))
                 (if (zerop (process-exit-status process))
                     (message "Life sync complete; see *Life sync* for details")
                   (message "Life sync stopped; see *Life sync* (no forced resolution)")))))))))

(map! :leader :desc "Sync Life repository" "g S" #'my/life-sync)

(provide '+life-sync)
