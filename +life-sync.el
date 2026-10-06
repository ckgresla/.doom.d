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
        (let ((inhibit-read-only t)) (erase-buffer))
        (special-mode))
      (display-buffer buffer)
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
                   (display-buffer (process-buffer process))
                   (message "Life sync stopped; see *Life sync* (no forced resolution)")))))))))

(map! :leader :desc "Sync Life repository" "g S" #'my/life-sync)

(provide '+life-sync)
