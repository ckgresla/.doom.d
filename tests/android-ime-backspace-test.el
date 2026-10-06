;;; android-ime-backspace-test.el --- Android IME Backspace regressions -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -l tests/android-ime-backspace-test.el
(require 'ert)
(require 'cl-lib)

(defvar text-conversion-style)
(defvar overriding-text-conversion-style)
(defconst android-ime-backspace-test-config
  (expand-file-name "../+android.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun android-ime-backspace-test-form (predicate)
  "Read the unique real config form matching PREDICATE without loading Doom."
  (let (matches)
    (with-temp-buffer
      (insert-file-contents android-ime-backspace-test-config)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (funcall predicate form)
                (push form matches))))
        (end-of-file nil)))
    (should (= (length matches) 1))
    (car matches)))

(dolist (name '(my/android-ime-reset-after-backspace
                my/android-ime-reset-after-backspace-a))
  (eval (android-ime-backspace-test-form
         (lambda (form)
           (and (memq (car-safe form) '(defcustom defun))
                (eq (cadr form) name))))
        t))

(defmacro android-ime-backspace-test-with-command (&rest body)
  "Test the real advice with a minimal interactive deletion command."
  (declare (indent 0) (debug t))
  `(let ((system-type 'android)
         (my/android-ime-reset-after-backspace t)
         (text-conversion-style t)
         (overriding-text-conversion-style 'lambda)
         (this-command 'evil-delete-backward-char-and-join)
         (reset-calls nil)
         (command-calls 0))
     (cl-letf (((symbol-function 'evil-delete-backward-char-and-join)
                (lambda (count)
                  (interactive "p")
                  (cl-incf command-calls)
                  (delete-char (- count))
                  'original-result))
               ((symbol-function 'set-text-conversion-style)
                (lambda (style &rest args)
                  (push (list style args (point) (buffer-string)) reset-calls))))
       (let* ((setup (android-ime-backspace-test-form
                      (lambda (form)
                        (and (eq (car-safe form) 'after!)
                             (eq (cadr form) 'evil)))))
              (registration
               (cl-remove-if-not
                (lambda (form) (eq (car-safe form) 'advice-add)) (cddr setup))))
         (should (= (length registration) 1))
         (eval (car registration) t)
         (unwind-protect
             (with-temp-buffer
               (insert "prefix tha")
               ,@body)
           (advice-remove 'evil-delete-backward-char-and-join
                          #'my/android-ime-reset-after-backspace-a))))))

(defun android-ime-backspace-test-press ()
  "Simulate an interactive invocation even under the batch test runner."
  (let ((noninteractive nil))
    (call-interactively #'evil-delete-backward-char-and-join)))

(ert-deftest android-ime-backspace-resets-after-deletion-and-preserves-style ()
  (dolist (style '(t action))
    (android-ime-backspace-test-with-command
      (setq text-conversion-style style)
      (should (eq (android-ime-backspace-test-press) 'original-result))
      (should (equal (buffer-string) "prefix th"))
      (should (= command-calls 1))
      (should (= (point) 10))
      (should (equal reset-calls (list (list style nil 10 "prefix th"))))
      (should (eq text-conversion-style style))
      (should (eq overriding-text-conversion-style 'lambda)))))

(ert-deftest android-ime-backspace-disabled-conversion-is-untouched ()
  (android-ime-backspace-test-with-command
    (setq text-conversion-style nil)
    (android-ime-backspace-test-press)
    (should (equal (buffer-string) "prefix th"))
    (should-not text-conversion-style)
    (should-not reset-calls)))

(ert-deftest android-ime-backspace-explicit-overrides-are-untouched ()
  (dolist (override '(nil t action password))
    (android-ime-backspace-test-with-command
      (setq overriding-text-conversion-style override)
      (android-ime-backspace-test-press)
      (should (equal (buffer-string) "prefix th"))
      (should (eq overriding-text-conversion-style override))
      (should-not reset-calls))))

(ert-deftest android-ime-backspace-option-can-disable-workaround ()
  (android-ime-backspace-test-with-command
    (setq my/android-ime-reset-after-backspace nil)
    (android-ime-backspace-test-press)
    (should (equal (buffer-string) "prefix th"))
    (should-not reset-calls)))

(ert-deftest android-ime-backspace-desktop-is-untouched ()
  (dolist (platform '(darwin gnu/linux))
    (android-ime-backspace-test-with-command
      (setq system-type platform)
      (android-ime-backspace-test-press)
      (should (equal (buffer-string) "prefix th"))
      (should-not reset-calls))))

(ert-deftest android-ime-backspace-noninteractive-calls-do-not-reset ()
  (android-ime-backspace-test-with-command
    ;; Even a stale matching `this-command' does not authorize a reset.
    (should (eq (evil-delete-backward-char-and-join 1) 'original-result))
    (should (equal (buffer-string) "prefix th"))
    (should-not reset-calls)))

(ert-deftest android-ime-backspace-other-commands-do-not-reset ()
  (android-ime-backspace-test-with-command
    (setq this-command 'another-command)
    (android-ime-backspace-test-press)
    (should (equal (buffer-string) "prefix th"))
    (should-not reset-calls)))

(ert-deftest android-ime-backspace-missing-native-api-is-harmless ()
  (android-ime-backspace-test-with-command
    (fmakunbound 'set-text-conversion-style)
    (android-ime-backspace-test-press)
    (should (equal (buffer-string) "prefix th"))
    (should-not reset-calls)))

(ert-deftest android-ime-backspace-original-error-is-preserved ()
  (android-ime-backspace-test-with-command
    (goto-char (point-min))
    (should-error (android-ime-backspace-test-press) :type 'beginning-of-buffer)
    (should (equal (buffer-string) "prefix tha"))
    (should (= command-calls 1))
    (should-not reset-calls)))

(ert-run-tests-batch-and-exit "^android-ime-backspace-")
