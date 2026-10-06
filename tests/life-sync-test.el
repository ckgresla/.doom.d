;;; life-sync-test.el --- Disposable-repository sync tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
;; Match +android.el: Android's system shell cannot launch Termux children.
(when (eq system-type 'android)
  (setq shell-file-name "/data/data/com.termux/files/usr/bin/sh"))
(defconst life-test-script
  (expand-file-name "../scripts/life-sync.sh"
                    (file-name-directory (or load-file-name buffer-file-name))))
(defconst life-test-config
  (expand-file-name "../+life-sync.el"
                    (file-name-directory (or load-file-name buffer-file-name))))
(defvar my/life-sync-push)
(defvar my/life-sync-process)
(defvar my/life-sync-include-new)
(defun life-test-load-config ()
  ;; Exercise the real wrapper without needing Doom's keymap macro in batch.
  (cl-letf (((symbol-function 'map!)
             (cons 'macro (lambda (&rest _) nil))))
    (load life-test-config nil t)))
(defun life-test-process-options ()
  "Capture process options without touching the real Life repo or its windows."
  (let* ((root (expand-file-name "~/life/"))
         (default-directory root)
         (my/life-sync-process nil)
         (my/life-sync-include-new nil)
         (log (generate-new-buffer " *Life sync command test*"))
         options)
    (unwind-protect
        (cl-letf (((symbol-function 'file-truename) #'identity)
                  ((symbol-function 'locate-dominating-file)
                   (lambda (&rest _) root))
                  ((symbol-function 'save-some-buffers) #'ignore)
                  ((symbol-function 'file-in-directory-p)
                   (lambda (&rest _) nil))
                  ((symbol-function 'get-buffer-create) (lambda (&rest _) log))
                  ((symbol-function 'my/life-sync-show-log) #'ignore)
                  ((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq options args)
                     nil)))
          (my/life-sync)
          options)
      (kill-buffer log))))

(defun life-test-command ()
  "Capture the interactive command's argv without touching the real Life repo."
  (plist-get (life-test-process-options) :command))

(ert-deftest life-sync-default-pushes-on-both-platforms ()
  ;; An unbound value proves the actual defvar default, not a test override.
  (cl-progv '(my/life-sync-push) nil
    (makunbound 'my/life-sync-push)
    (life-test-load-config)
    (should (eq my/life-sync-push t))
    (dolist (platform '(darwin android))
      (let* ((system-type platform)
             (command (life-test-command)))
        (should (equal (nth 3 command)
                       (if (eq platform 'android) "galaxy" "hackbook")))
        (should (equal (nth 4 command) "yes"))
        (should (equal (nth 5 command) "no"))))))

(ert-deftest life-sync-explicit-local-only-survives-reload ()
  (let ((my/life-sync-push nil))
    (life-test-load-config)
    (should-not my/life-sync-push)
    (dolist (platform '(darwin android))
      (let ((system-type platform))
        (should (equal (nth 4 (life-test-command)) "no"))))))

(ert-deftest life-sync-log-fills-current-frame-and-q-restores-layout ()
  (life-test-load-config)
  (save-window-excursion
    (let ((first (generate-new-buffer " *Life first*"))
          (second (generate-new-buffer " *Life second*"))
          (log (generate-new-buffer " *Life log*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer first)
            (with-current-buffer first (insert "first\nsecond\nthird\n"))
            (goto-char 8)
            (set-window-buffer (split-window-right) second)
            (let ((before (current-window-configuration))
                  (display-buffer-alist
                   '(("Life log" (display-buffer-in-side-window) (side . bottom)))))
              (my/life-sync-show-log log)
              (should (eq (current-buffer) log))
              (should (= (length (window-list nil 'nomini)) 1))
              (should-not (window-parameter nil 'window-side))
              (should buffer-read-only)
              (should (eq (key-binding (kbd "q")) #'my/life-sync-quit))
              ;; Showing the same running log again must retain its return path.
              (my/life-sync-show-log log)
              (call-interactively (key-binding (kbd "q")))
              (should (compare-window-configurations before (current-window-configuration)))
              (should (eq (current-buffer) first))
              (should (= (point) 8))
              (should (buffer-live-p log))))
        (mapc #'kill-buffer (list first second log))))))

(ert-deftest life-sync-log-replaces-an-old-side-popup-with-a-main-window ()
  (life-test-load-config)
  (save-window-excursion
    (let ((main (generate-new-buffer " *Life main*"))
          (log (generate-new-buffer " *Life old popup*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer main)
            (select-window
             (display-buffer-in-side-window log '((side . bottom) (window-height . 0.25))))
            (let ((before (current-window-configuration)))
              (my/life-sync-show-log log)
              (should (eq (current-buffer) log))
              (should (= (length (window-list nil 'nomini)) 1))
              (should-not (window-parameter nil 'window-side))
              (my/life-sync-quit)
              (should (compare-window-configurations before (current-window-configuration)))))
        (mapc #'kill-buffer (list main log))))))

(ert-deftest life-sync-log-has-evil-navigation-and-restoring-quit ()
  ;; The base suite also runs without Doom packages; exercise real Evil when
  ;; its package directories are provided on load-path (as in Doom itself).
  (skip-unless (require 'evil nil t))
  (life-test-load-config)
  (with-temp-buffer
    (my/life-sync-mode)
    (evil-local-mode 1)
    (should (eq evil-state 'motion))
    (should (eq (key-binding (kbd "j")) #'evil-next-line))
    (should (eq (key-binding (kbd "q")) #'my/life-sync-quit))
    (evil-normal-state)
    (should (eq (key-binding (kbd "q")) #'my/life-sync-quit))))

(ert-deftest life-sync-completion-does-not-reopen-or-select-log ()
  (life-test-load-config)
  (let ((sentinel (plist-get (life-test-process-options) :sentinel)))
    (dolist (exit-code '(0 1))
      (with-temp-buffer
        (let ((buffer (current-buffer))
              (window (selected-window))
              notice)
          (cl-letf (((symbol-function 'process-status) (lambda (&rest _) 'exit))
                    ((symbol-function 'process-exit-status) (lambda (&rest _) exit-code))
                    ((symbol-function 'message)
                     (lambda (format &rest args) (setq notice (apply #'format format args))))
                    ((symbol-function 'display-buffer)
                     (lambda (&rest _) (ert-fail "Completion reopened log")))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (&rest _) (ert-fail "Completion selected log"))))
            (funcall sentinel 'test-process "finished\n"))
          (should (eq (current-buffer) buffer))
          (should (eq (selected-window) window))
          (should (string-match-p (if (zerop exit-code) "complete" "stopped") notice)))))))

(defun life-test-git (dir &rest args)
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (unless (zerop (apply #'process-file "git" nil t nil args))
        (error "Git test setup failed: %s" (buffer-string)))
      (string-trim (buffer-string)))))
(defun life-test-write (dir file text)
  (with-temp-file (expand-file-name file dir) (insert text)))
(defun life-test-run (dir &optional push include)
  (with-temp-buffer
    (process-file shell-file-name nil t nil life-test-script dir "galaxy"
                  (if push "yes" "no") (if include "yes" "no"))))
(defmacro life-test-repos (&rest body)
  `(let* ((tmp (make-temp-file "life-sync-test-" t))
          (remote (expand-file-name "remote" tmp))
          (local (expand-file-name "local" tmp))
          (peer (expand-file-name "peer" tmp))
          (process-environment (copy-sequence process-environment)))
     (setenv "GIT_CONFIG_GLOBAL" "/dev/null")
     (setenv "GIT_CONFIG_NOSYSTEM" "1")
     (setenv "GIT_AUTHOR_NAME" "Sync Test")
     (setenv "GIT_AUTHOR_EMAIL" "test@example.invalid")
     (setenv "GIT_COMMITTER_NAME" "Sync Test")
     (setenv "GIT_COMMITTER_EMAIL" "test@example.invalid")
     (unwind-protect
         (progn
           (life-test-git tmp "init" "--bare" "--initial-branch=main" remote)
           (life-test-git tmp "clone" remote local)
           (life-test-write local "note.org" "base\n")
           (life-test-git local "add" ".")
           (life-test-git local "commit" "-m" "initial")
           (life-test-git local "push" "-u" "origin" "main")
           (life-test-git tmp "clone" remote peer)
           ,@body)
       (delete-directory tmp t))))

(ert-deftest life-sync-commits-tracked-only-with-device-and-time ()
  (life-test-repos
   (life-test-write local "note.org" "local\n")
   (life-test-write local "new.org" "new\n")
   (should (zerop (life-test-run local)))
   (should (string-match-p "org: sync galaxy @ [0-9]"
                           (life-test-git local "log" "-1" "--format=%s")))
   (should (equal (life-test-git local "status" "--porcelain") "?? new.org"))
   (should-not (equal (life-test-git local "rev-parse" "HEAD")
                      (life-test-git remote "rev-parse" "main")))))

(ert-deftest life-sync-rebases-then-pushes-without-configured-upstream ()
  (life-test-repos
   (life-test-git local "branch" "--unset-upstream")
   (life-test-write peer "remote.org" "remote\n")
   (life-test-git peer "add" ".")
   (life-test-git peer "commit" "-m" "remote edit")
   (life-test-git peer "push")
   (life-test-write local "note.org" "local\n")
   (should (zerop (life-test-run local t)))
   (should (file-exists-p (expand-file-name "remote.org" local)))
   (should (equal (life-test-git local "rev-parse" "HEAD")
                  (life-test-git remote "rev-parse" "main")))))

(ert-deftest life-sync-conflict-aborts-but-keeps-fetch-and-local-commit ()
  (life-test-repos
   (life-test-write peer "note.org" "remote\n")
   (life-test-git peer "commit" "-am" "remote edit")
   (life-test-git peer "push")
   (life-test-write local "note.org" "local\n")
   (should-not (zerop (life-test-run local t)))
   (should (equal (life-test-git local "show" "HEAD:note.org") "local"))
   (should (equal (life-test-git local "show" "origin/main:note.org") "remote"))
   (should (equal (life-test-git local "status" "--porcelain") ""))
   (should-not (file-exists-p (expand-file-name ".git/rebase-merge" local)))
   (should-not (equal (life-test-git local "rev-parse" "HEAD")
                      (life-test-git remote "rev-parse" "main")))))

(ert-deftest life-sync-push-rejection-keeps-local-checkpoint-and-remote ()
  (life-test-repos
   (let ((remote-tip (life-test-git remote "rev-parse" "main")))
     (life-test-write remote "hooks/pre-receive" "#!/bin/sh\nexit 1\n")
     (set-file-modes (expand-file-name "hooks/pre-receive" remote) #o700)
     (life-test-write local "note.org" "local checkpoint\n")
     (should-not (zerop (life-test-run local t)))
     (should (equal (life-test-git local "show" "HEAD:note.org") "local checkpoint"))
     (should (equal (life-test-git remote "rev-parse" "main") remote-tip))
     (should-not (equal (life-test-git local "rev-parse" "HEAD") remote-tip))
     (should (equal (life-test-git local "status" "--porcelain") ""))
     (should-not (file-exists-p (expand-file-name ".git/life-sync.lock" local))))))

(ert-deftest life-sync-refuses-preexisting-index ()
  (life-test-repos
   (life-test-write local "note.org" "staged\n")
   (life-test-git local "add" ".")
   (let ((before (life-test-git local "diff" "--cached")))
     (should-not (zerop (life-test-run local)))
     (should (equal before (life-test-git local "diff" "--cached"))))))

(ert-deftest life-sync-untracked-collision-is-preserved ()
  (life-test-repos
   (life-test-write peer "new.org" "remote\n")
   (life-test-git peer "add" ".")
   (life-test-git peer "commit" "-m" "new file")
   (life-test-git peer "push")
   (life-test-write local "new.org" "precious local\n")
   (should-not (zerop (life-test-run local)))
   (with-temp-buffer
     (insert-file-contents (expand-file-name "new.org" local))
     (should (equal (buffer-string) "precious local\n")))))

(ert-deftest life-sync-can-include-new-files-explicitly ()
  (life-test-repos
   (life-test-write local "new.org" "new\n")
   (should (zerop (life-test-run local nil t)))
   (should (equal (life-test-git local "show" "HEAD:new.org") "new"))))

(ert-deftest life-sync-fetch-failure-does-not-stage-or-commit ()
  (life-test-repos
   (life-test-git local "remote" "set-url" "origin" "/nonexistent-life-sync-test")
   (life-test-write local "note.org" "local\n")
   (let ((head (life-test-git local "rev-parse" "HEAD")))
     (should-not (zerop (life-test-run local)))
     (should (equal head (life-test-git local "rev-parse" "HEAD")))
     (should (equal "" (life-test-git local "diff" "--cached"))))))

(ert-run-tests-batch-and-exit "^life-sync-")
