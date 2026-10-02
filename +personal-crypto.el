;;; +personal-crypto.el --- Shared-key encryption for personal notes -*- lexical-binding: t; -*-

;; This public fingerprint is safe to track.  The corresponding private key
;; belongs only in each machine's GnuPG keyring, never in this configuration.
(defconst my/personal-crypto-recipient
  "BF678B4861E23D24D46E74CE92DB8C9695ACC524"
  "Public-key fingerprint used only for ~/life/org/personal.org.gpg.")

(defun my/personal-crypto-target-p ()
  "Whether the current buffer visits the exact personal notes path.
Do not resolve symlinks or contact remote hosts just to classify a buffer."
  (and buffer-file-name
       (equal (expand-file-name buffer-file-name)
              (expand-file-name "~/life/org/personal.org.gpg"))))

(defun my/personal-crypto-save-guard ()
  "Keep personal notes key-encrypted, failing closed if setup is missing."
  (when (my/personal-crypto-target-p)
    (my/personal-crypto-setup)
    (unless (eq (find-file-name-handler buffer-file-name 'write-region)
                'epa-file-handler)
      (user-error "EasyPG file encryption must be enabled before saving personal notes"))
    ;; Without a matching key, EasyPG can pass nil recipients to GnuPG and
    ;; fall back to symmetric encryption.  Do not silently ask for a password.
    (unless (epg-list-keys (epg-make-context)
                           (list my/personal-crypto-recipient))
      (user-error "Install the shared personal-notes GPG key before saving"))))

(defun my/personal-crypto-setup ()
  "Configure shared-key encryption and no-plaintext-copy safeguards locally.
Other .gpg files retain their own recipients and save settings.  This function
does not open, decrypt, save, or change the major mode of any file."
  (when (my/personal-crypto-target-p)
    ;; New buffers need the EasyPG variables before setting local values;
    ;; visiting an existing .gpg file normally has already loaded the library.
    (require 'epa-file)
    (setq-local epa-file-encrypt-to (list my/personal-crypto-recipient)
                epa-file-select-keys nil
                epa-file-inhibit-auto-save t
                backup-inhibited t)
    ;; Ordinary #autosave# files may bypass the .gpg handler.  Preserve
    ;; EasyPG's conservative default even if a global hook enabled autosave.
    (auto-save-mode -1)
    ;; `before-save-hook' errors are demoted by Emacs and do not stop a save.
    ;; A nil-returning `write-file-functions' guard allows normal EasyPG saving
    ;; on success, but a setup error here really aborts before anything writes.
    (add-hook 'write-file-functions #'my/personal-crypto-save-guard nil t)
    (add-hook 'after-revert-hook #'my/personal-crypto-setup nil t)))

;; Append after the usual find-file setup, including file-local variables.
(add-hook 'find-file-hook #'my/personal-crypto-setup t)

;; Reloads update an already-open target buffer, without visiting its file or
;; touching other buffers.  Named hooks remain unique across repeated loads.
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (my/personal-crypto-setup)))

(provide '+personal-crypto)
;;; +personal-crypto.el ends here
