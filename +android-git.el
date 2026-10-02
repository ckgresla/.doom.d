;;; +android-git.el --- Lightweight Android Magit refresh -*- lexical-binding: t; -*-
;; Loaded only by +android.el.  Keep Git operations and status information
;; intact; avoid unnecessary process tracing and project-cache churn.

(defvar my/android-magit-direct-git nil
  "Git executable verified to work without Android's exec loader this session.")

(defun my/android-magit-probe-direct-git ()
  "Check direct Git execution, retaining the loader if this device needs it."
  (setq my/android-magit-direct-git nil)
  (when (boundp 'android-use-exec-loader)
    (let ((program (magit-git-executable))
          (default-directory (expand-file-name "~/"))
          (android-use-exec-loader nil))
      (when (condition-case nil
                (eq 0 (process-file program nil nil nil "--version"))
              (file-error nil))
        (setq my/android-magit-direct-git program)))))

(defun my/android-magit-refresh-fast (original &rest args)
  "Use verified direct execution for local Magit refresh queries only.
Normal Git commands, hooks, SSH operations and remote repositories retain
their usual execution path.  No global Android execution setting is changed."
  (let ((android-use-exec-loader
         (if (and my/android-magit-direct-git
                  (not (file-remote-p default-directory))
                  (equal my/android-magit-direct-git (magit-git-executable)))
             nil
           android-use-exec-loader)))
    (apply original args)))

(defvar my/android-magit-file-inventories (make-hash-table :test #'equal)
  "Last filename inventory observed in each local repository's status buffer.")

(defun my/android-magit-file-inventory ()
  "Fingerprint tracked, untracked and missing files, or return nil on failure.
NUL separation handles arbitrary filenames.  Including --deleted also detects
unstaged deletions, whose paths are still present in the index.  File content,
staging ordinary edits, and commit messages do not affect this inventory."
  (with-temp-buffer
    (when (eq 0 (magit-process-git
                 t "ls-files" "-z" "--cached" "--others" "--deleted"
                 "--exclude-standard"))
      ;; Git groups untracked and indexed paths separately.  Sort so staging
      ;; an existing path does not look like a change to the filename set.
      (secure-hash 'sha256
                   (mapconcat #'identity
                              (sort (split-string (buffer-string) "\0" t)
                                    #'string<)
                              "\0")))))

(defun my/android-magit-invalidate-projectile-on-file-change ()
  "Keep Projectile's hot cache until the repository's filename inventory changes.
The first status refresh establishes a baseline and invalidates once.  Remote
repositories retain Doom's original behavior.  Failed scans are not cached."
  (when (bound-and-true-p projectile-mode)
    (if (file-remote-p default-directory)
        (+magit-invalidate-projectile-cache-h)
      (when (derived-mode-p 'magit-status-mode)
        (when-let* ((root (magit-toplevel))
                    (inventory (my/android-magit-file-inventory)))
          (unless (equal (gethash root my/android-magit-file-inventories)
                         inventory)
            ;; Match Doom's cheap hot-cache invalidation, without rewriting
            ;; persistent caches or cleaning recentf during every refresh.
            (let (projectile-require-project-root
                  projectile-enable-caching
                  projectile-verbose)
              (cl-letf (((symbol-function 'recentf-cleanup) #'ignore))
                (projectile-invalidate-cache nil)))
            (puthash root inventory my/android-magit-file-inventories)))))))

(after! magit
  (my/android-magit-probe-direct-git)
  (advice-add 'magit-refresh-buffer :around #'my/android-magit-refresh-fast)
  (remove-hook 'magit-refresh-buffer-hook #'+magit-invalidate-projectile-cache-h)
  (add-hook 'magit-refresh-buffer-hook
            #'my/android-magit-invalidate-projectile-on-file-change))

;;; +android-git.el ends here
