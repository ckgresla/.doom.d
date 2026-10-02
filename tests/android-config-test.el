;;; android-config-test.el --- Android configuration regressions -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -l tests/android-config-test.el
(require 'ert)
(require 'cl-lib)

(defvar jit-lock-defer-time)
(defconst android-config-test-directory
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defun android-config-test-single-form (file predicate)
  "Read the unique top-level form matching PREDICATE from config FILE."
  (let (matches)
    (with-temp-buffer
      (insert-file-contents (expand-file-name file android-config-test-directory))
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (funcall predicate form)
                (push form matches))))
        (end-of-file nil)))
    (should (= (length matches) 1))
    (car matches)))

(defun android-config-test-apply-fontification-setting ()
  "Evaluate the real platform gate and setting without loading Doom modules."
  (let ((gate (android-config-test-single-form
               "config.el"
               (lambda (form)
                 (and (eq (car-safe form) 'when)
                      (member '(load! "+android") (cddr form))))))
        (setting (android-config-test-single-form
                  "+android.el"
                  (lambda (form)
                    (and (eq (car-safe form) 'setq)
                         (eq (cadr form)
                             'redisplay-skip-fontification-on-input)))))
        (loads 0))
    (cl-letf (((symbol-function 'load!)
               (lambda (name &rest _)
                 (should (equal name "+android"))
                 (cl-incf loads)
                 (eval setting t)))
              ;; Do not mistake a test machine's filesystem for an Android app.
              ((symbol-function 'file-directory-p) (lambda (&rest _) nil)))
      (eval gate t))
    loads))

(ert-deftest android-config-fontifies-on-input ()
  (let ((system-type 'android)
        (redisplay-skip-fontification-on-input t))
    (should (= (android-config-test-apply-fontification-setting) 1))
    (should-not redisplay-skip-fontification-on-input)))

(ert-deftest android-config-desktop-fontification-setting-untouched ()
  (dolist (platform '(darwin gnu/linux))
    (let ((system-type platform)
          (redisplay-skip-fontification-on-input t))
      (should (= (android-config-test-apply-fontification-setting) 0))
      (should redisplay-skip-fontification-on-input))))

(ert-deftest android-config-fontification-setting-is-idempotent ()
  (let ((system-type 'android)
        (redisplay-skip-fontification-on-input t))
    (dotimes (_ 2)
      (should (= (android-config-test-apply-fontification-setting) 1))
      (should-not redisplay-skip-fontification-on-input))))

(ert-deftest android-config-keeps-other-scrolling-and-jit-settings ()
  (dolist (fast-scroll '(nil t))
    (dolist (defer-time '(nil 0.25))
      (let ((system-type 'android)
            (redisplay-skip-fontification-on-input t)
            (fast-but-imprecise-scrolling fast-scroll)
            (jit-lock-defer-time defer-time))
        (should (= (android-config-test-apply-fontification-setting) 1))
        (should-not redisplay-skip-fontification-on-input)
        (should (eq fast-but-imprecise-scrolling fast-scroll))
        (should (equal jit-lock-defer-time defer-time))))))

(ert-run-tests-batch-and-exit "^android-config-")
