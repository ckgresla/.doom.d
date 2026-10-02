;;; personal-crypto-test.el --- Scoped personal encryption tests -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -l tests/personal-crypto-test.el
(require 'ert)
(require 'cl-lib)
(require 'epa-file)
(defvar native-comp-enable-subr-trampolines)

(defconst personal-crypto-test-module
  (expand-file-name "../+personal-crypto.el"
                    (file-name-directory (or load-file-name buffer-file-name))))
(load personal-crypto-test-module nil t)

(defmacro personal-crypto-test-with-target (&rest body)
  "Run BODY in an empty buffer with the target's name; never read its file."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (setq buffer-file-name (expand-file-name "~/life/org/personal.org.gpg"))
     ,@body))

(ert-deftest personal-crypto-only-exact-target-matches ()
  (personal-crypto-test-with-target
    (should (my/personal-crypto-target-p)))
  (dolist (file (list nil
                     (expand-file-name "~/life/org/other.org.gpg")
                     (expand-file-name "~/life/org/personal.org")
                     (expand-file-name "~/other/org/personal.org.gpg")
                     (concat (expand-file-name "~/life/org/personal.org.gpg") "~")
                     "/ssh:example.invalid:/life/org/personal.org.gpg"))
    (with-temp-buffer
      (setq buffer-file-name file)
      (should-not (my/personal-crypto-target-p)))))

(ert-deftest personal-crypto-sets-only-buffer-local-recipient-and-safeguards ()
  (let ((defaults (mapcar #'default-value
                          '(epa-file-encrypt-to epa-file-select-keys
                            epa-file-inhibit-auto-save backup-inhibited))))
    (personal-crypto-test-with-target
      (setq-local epa-file-encrypt-to '("old-recipient")
                  epa-file-select-keys t
                  epa-file-inhibit-auto-save nil
                  backup-inhibited nil
                  buffer-auto-save-file-name "/not-written/#personal#")
      (my/personal-crypto-setup)
      (should (equal epa-file-encrypt-to (list my/personal-crypto-recipient)))
      (should (local-variable-p 'epa-file-encrypt-to))
      (should-not epa-file-select-keys)
      (should epa-file-inhibit-auto-save)
      (should backup-inhibited)
      (should-not buffer-auto-save-file-name)
      (should (eq major-mode 'fundamental-mode))
      (should-not (buffer-modified-p)))
    (should (equal defaults
                   (mapcar #'default-value
                           '(epa-file-encrypt-to epa-file-select-keys
                             epa-file-inhibit-auto-save backup-inhibited))))))

(ert-deftest personal-crypto-other-encrypted-file-stays-unchanged ()
  (with-temp-buffer
    (setq buffer-file-name (expand-file-name "~/life/org/other.org.gpg"))
    (setq-local epa-file-encrypt-to '("other-recipient")
                epa-file-select-keys t
                epa-file-inhibit-auto-save nil
                backup-inhibited nil
                buffer-auto-save-file-name "/not-written/#other#")
    (let ((before (buffer-local-variables)))
      (cl-letf (((symbol-function 'epg-list-keys)
                 (lambda (&rest _) (ert-fail "Unexpected GPG lookup"))))
        (my/personal-crypto-setup)
        (my/personal-crypto-save-guard))
      (should (equal before (buffer-local-variables))))))

(ert-deftest personal-crypto-reload-does-not-read-save-or-duplicate-hooks ()
  (personal-crypto-test-with-target
    (cl-letf (((symbol-function 'find-file-noselect)
               (lambda (&rest _) (ert-fail "Unexpected file visit")))
              ((symbol-function 'epg-list-keys)
               (lambda (&rest _) (ert-fail "Unexpected GPG lookup")))
              ((symbol-function 'save-buffer)
               (lambda (&rest _) (ert-fail "Unexpected file save"))))
      (dotimes (_ 2) (load personal-crypto-test-module nil t)))
    (should (= 1 (cl-count #'my/personal-crypto-setup find-file-hook)))
    (should (= 1 (cl-count #'my/personal-crypto-setup after-revert-hook)))
    (should (= 1 (cl-count #'my/personal-crypto-save-guard write-file-functions)))
    (should (equal epa-file-encrypt-to (list my/personal-crypto-recipient)))))

(ert-deftest personal-crypto-revert-restores-full-recipient-fingerprint ()
  (personal-crypto-test-with-target
    (my/personal-crypto-setup)
    ;; EasyPG sets recipients from packet key IDs when reading/reverting.
    (setq epa-file-encrypt-to '("short-key-id"))
    (run-hooks 'after-revert-hook)
    (should (equal epa-file-encrypt-to (list my/personal-crypto-recipient)))))

(ert-deftest personal-crypto-save-check-requires-installed-public-key ()
  (personal-crypto-test-with-target
    (cl-letf (((symbol-function 'epg-make-context) (lambda (&rest _) 'context))
              ((symbol-function 'epg-list-keys) (lambda (&rest _) nil)))
      (should-error (my/personal-crypto-save-guard) :type 'user-error))))

(ert-deftest personal-crypto-save-check-uses-exact-fingerprint ()
  (personal-crypto-test-with-target
    (setq-local epa-file-encrypt-to '("wrong-recipient"))
    (cl-letf (((symbol-function 'epg-make-context) (lambda (&rest _) 'context))
              ((symbol-function 'epg-list-keys)
               (lambda (context recipients &rest _)
                 (should (eq context 'context))
                 (should (equal recipients (list my/personal-crypto-recipient)))
                 '(installed-key))))
      (should-not (my/personal-crypto-save-guard))
      (should (equal epa-file-encrypt-to (list my/personal-crypto-recipient))))))

(ert-deftest personal-crypto-save-check-refuses-disabled-encryption-handler ()
  (personal-crypto-test-with-target
    (let ((file-name-handler-alist nil))
      (cl-letf (((symbol-function 'epg-list-keys)
                 (lambda (&rest _) (ert-fail "Should fail before GPG lookup"))))
        (should-error (my/personal-crypto-save-guard) :type 'user-error)))))

(ert-deftest personal-crypto-missing-key-really-aborts-basic-save-buffer ()
  ;; Exercise Emacs's real save hook order; merely testing a hook's error is
  ;; insufficient because errors in `before-save-hook' do not abort saving.
  (with-temp-buffer
    (insert "Synthetic test contents, not personal data.\n")
    (setq buffer-file-name (expand-file-name "~/life/org/personal.org.gpg"))
    (my/personal-crypto-setup)
    (let ((native-comp-enable-subr-trampolines nil))
      (cl-letf (((symbol-function 'epg-make-context) (lambda (&rest _) 'context))
                ((symbol-function 'epg-list-keys) (lambda (&rest _) nil))
                ((symbol-function 'file-exists-p) (lambda (&rest _) t))
                ((symbol-function 'verify-visited-file-modtime) (lambda (&rest _) t))
                ((symbol-function 'vc-before-save) #'ignore)
                ((symbol-function 'write-region)
                 (lambda (&rest _) (ert-fail "Save continued with a missing key"))))
        (should-error (basic-save-buffer) :type 'user-error)))))

(ert-run-tests-batch-and-exit "^personal-crypto-")
