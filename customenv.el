;;; customenv.el -*- lexical-binding: t; -*-


;;; customenv.el --- Support for `direnv' that operates buffer-locally  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: processes, tools
;; Homepage: https://github.com/purcell/customenv
;; Package-Requires: ((emacs "27.1") (inheritenv "0.1") (seq "2.24"))
;; Package-Version: 0.12

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Use direnv (https://direnv.net/) to set environment variables on a
;; per-buffer basis.  This means that when you work across multiple
;; projects which have `.customenv` files, all processes launched from the
;; buffers "in" those projects will be executed with the environment
;; variables specified in those files.  This allows different versions
;; of linters and other tools to be installed in each project if
;; desired.

;; Enable `customenv-global-mode' late in your startup files.  For
;; interaction with this functionality, see `customenv-mode-map', and the
;; commands `customenv-reload', `customenv-allow' and `customenv-deny'.

;; In particular, you can enable keybindings for the above commands by
;; binding your preferred prefix to `customenv-command-map' in
;; `customenv-mode-map', e.g.

;;    (with-eval-after-load 'customenv
;;      (define-key customenv-mode-map (kbd "C-c e") 'customenv-command-map))

;;; Code:

;; TODO: special handling for DIRENV_* vars? exclude them? use them to safely reload more aggressively?
;; TODO: handle nil default-directory (rarely happens, but is possible)
;; TODO: limit size of *direnv* buffer
;; TODO: special handling of compilation-environment?
;; TODO: handle use of "cd" and other changes of `default-directory' in a buffer over time?
;; TODO: handle "allow" asynchronously?
;; TODO: describe env
;; TODO: click on mode lighter to get details
;; TODO: handle when direnv is not installed?
;; TODO: provide a way to disable in certain projects?
;; TODO: cleanup the cache?

(require 'seq)
(require 'json)
(require 'subr-x)
(require 'ansi-color)
(require 'cl-lib)
(require 'diff-mode) ; for its faces
(require 'inheritenv)
(eval-when-compile (require 'tramp))

;;; Custom vars and minor modes

(defgroup customenv nil
  "Apply per-buffer environment variables using the direnv tool."
  :group 'processes)

(defcustom customenv-debug nil
  "Whether or not to output debug messages while in operation.
Messages are written into the *customenv-debug* buffer."
  :type 'boolean)

(defcustom customenv-update-on-eshell-directory-change t
  "Whether customenv will update environment when changing directory in eshell."
  :type 'boolean)

(defcustom customenv-show-summary-in-minibuffer t
  "When non-nil, show a summary of the changes made by direnv in the minibuffer."
  :group 'customenv
  :type 'boolean)

(defcustom customenv-direnv-executable "direnv"
  "The direnv executable used by customenv."
  :type 'string)

(define-obsolete-variable-alias 'customenv--lighter 'customenv-lighter "2021-05-17")

(defcustom customenv-lighter '(:eval (customenv--lighter))
  "The mode line lighter for `customenv-mode'.
You can set this to nil to disable the lighter."
  :type 'sexp)
(put 'customenv-lighter 'risky-local-variable t)

(defcustom customenv-none-lighter '(" customenv[" (:propertize "none" face customenv-mode-line-none-face) "]")
  "Lighter spec used by the default `customenv-lighter' when customenv is inactive."
  :type 'sexp)

(defcustom customenv-on-lighter '(" customenv[" (:propertize "on" face customenv-mode-line-on-face) "]")
  "Lighter spec used by the default `customenv-lighter' when customenv is on."
  :type 'sexp)

(defcustom customenv-error-lighter '(" customenv[" (:propertize "error" face customenv-mode-line-error-face) "]")
  "Lighter spec used by the default `customenv-lighter' when customenv has errored."
  :type 'sexp)

(defcustom customenv-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'customenv-allow)
    (define-key map (kbd "d") 'customenv-deny)
    (define-key map (kbd "r") 'customenv-reload)
    (define-key map (kbd "l") 'customenv-show-log)
    map)
  "Keymap for commands in `customenv-mode'.
See `customenv-mode-map' for how to assign a prefix binding to these."
  :type '(restricted-sexp :match-alternatives (keymapp)))
(fset 'customenv-command-map customenv-command-map)

(defcustom customenv-mode-map (make-sparse-keymap)
  "Keymap for `customenv-mode'.
To access `customenv-command-map' from this map, give it a prefix keybinding,
e.g. (define-key customenv-mode-map (kbd \"C-c e\") \\='customenv-command-map)"
  :type '(restricted-sexp :match-alternatives (keymapp)))

(defcustom customenv-remote nil
  "Whether or not to enable direnv over TRAMP."
  :type 'boolean)

(defcustom customenv-supported-tramp-methods '("ssh")
  "Tramp connection methods that are supported by customenv."
  :type '(repeat string))

;;;###autoload
(define-minor-mode customenv-mode
  "A local minor mode in which env vars are set by direnv."
  :init-value nil
  :lighter customenv-lighter
  :keymap customenv-mode-map
  (if customenv-mode
      (progn
        (customenv--update)
        (when (and (derived-mode-p 'eshell-mode) customenv-update-on-eshell-directory-change)
          (add-hook 'eshell-directory-change-hook #'customenv--update nil t)))
    (customenv--clear (current-buffer))
    (remove-hook 'eshell-directory-change-hook #'customenv--update t)))

;;;###autoload
(define-globalized-minor-mode customenv-global-mode customenv-mode
  (lambda ()
    (when
        (cond
         ((minibufferp) nil)
         ((file-remote-p default-directory)
          (and customenv-remote
               (seq-contains-p
                customenv-supported-tramp-methods
                (with-parsed-tramp-file-name default-directory vec vec-method))))
         (t (executable-find customenv-direnv-executable)))
      (customenv-mode 1))))

(defface customenv-mode-line-on-face '((t :inherit success))
  "Face used in mode line to indicate that direnv is in effect.")

(defface customenv-mode-line-error-face '((t :inherit error))
  "Face used in mode line to indicate that direnv failed.")

(defface customenv-mode-line-none-face '((t :inherit warning))
  "Face used in mode line to indicate that direnv is not active.")

;;; Global state

(defvar customenv--cache (make-hash-table :test 'equal :size 10)
  "Known customenv directories and their direnv results.
The values are as produced by `customenv--export'.")

;;; Local state

(defvar-local customenv--status 'none
  "Symbol indicating state of the current buffer's direnv.
One of \\='(none on error).")

(defvar-local customenv--remote-path nil
  "Buffer local variable for remote path.
If set, this will override `tramp-remote-path' via connection
local variables.")

;;; Internals

(defun customenv--lighter ()
  "Return a colourised version of `customenv--status' for use in the mode line."
  (pcase customenv--status
    (`on customenv-on-lighter)
    (`error customenv-error-lighter)
    (`none customenv-none-lighter)))

(defun customenv--env-dir-p (dir)
  "Return non-nil if DIR contains a config file for direnv."
  (or
   (file-exists-p (expand-file-name ".customenv" dir))
   (file-exists-p (expand-file-name ".env" dir))))

(defun customenv--find-env-dir ()
  "Return the customenv directory for the current buffer, if any.
This is based on a file scan.  In most cases we prefer to use the
cached list of known directories.

Regardless of buffer file name, we always use
`default-directory': the two should always match, unless the user
called `cd'"
  (let ((env-dir (locate-dominating-file default-directory #'customenv--env-dir-p)))
    (when env-dir
      ;; `locate-dominating-file' appears to sometimes return abbreviated paths, e.g. with ~
      (setq env-dir (expand-file-name env-dir)))
    env-dir))

(defun customenv--cache-key (env-dir process-env)
  "Get a hash key for the result of invoking direnv in ENV-DIR with PROCESS-ENV.
PROCESS-ENV should be the environment in which direnv was run,
since its output can vary according to its initial environment."
  (string-join (cons env-dir process-env) "\0"))

(defun customenv--update ()
  "Update the current buffer's environment if it is managed by direnv.
All customenv.el-managed buffers with this env will have their
environments updated."
  (let* ((env-dir (customenv--find-env-dir))
         (result
          (if env-dir
              (let ((cache-key (customenv--cache-key env-dir (default-value 'process-environment))))
                (pcase (gethash cache-key customenv--cache 'missing)
                  (`missing (let ((calculated (customenv--export env-dir)))
                              (puthash cache-key calculated customenv--cache)
                              calculated))
                  (cached cached)))
            'none)))
    (customenv--apply (current-buffer) result)))

(defmacro customenv--at-end-of-special-buffer (name &rest body)
  "At the end of `special-mode' buffer NAME, execute BODY.
To avoid confusion, `customenv-mode' is explicitly disabled in the buffer."
  (declare (indent 1))
  `(with-current-buffer (get-buffer-create ,name)
     (unless (derived-mode-p 'special-mode)
       (special-mode))
     (when customenv-mode (customenv-mode -1))
     (goto-char (point-max))
     (let ((inhibit-read-only t))
       ,@body)))

(defun customenv--debug (msg &rest args)
  "A version of `message' which does nothing if `customenv-debug' is nil.
MSG and ARGS are as for that function."
  (when customenv-debug
    (customenv--at-end-of-special-buffer "*customenv-debug*"
      (insert (apply 'format msg args))
      (newline))))

(defun customenv--summarise-changes (items)
  "Create a summary string for ITEMS."
  (if items
      (cl-loop for (name . val) in items
               with process-environment = (default-value 'process-environment)
               unless (string-prefix-p "DIRENV_" name)
               collect (cons name
                             (if val
                                 (if (getenv name)
                                     '("~" diff-changed)
                                   '("+" diff-added))
                               '("-" diff-removed)))
               into entries
               finally return (cl-loop for (name prefix face) in (seq-sort-by 'car 'string< entries)
                                       collect (propertize (concat prefix name) 'face face) into strings
                                       finally return (string-join strings " ")))
    "no changes"))

(defun customenv--show-summary (result directory)
  "Summarise successful RESULT in the minibuffer.
DIRECTORY is the directory in which the environment changes."
  (message "direnv: %s %s"
           (customenv--summarise-changes result)
           (propertize (concat "(" (abbreviate-file-name (directory-file-name directory)) ")")
                       'face 'font-lock-comment-face)))

(defun customenv--export (env-dir)
  "Export the env vars for ENV-DIR using direnv.
Return value is either \\='error, \\='none, or an alist of environment
variable names and values."
  (unless (customenv--env-dir-p env-dir)
    (error "%s is not a directory with a .customenv" env-dir))
  (message "Running direnv in %s ... (C-g to abort)" env-dir)
  (let ((stderr-file (make-temp-file "customenv"))
        result)
    (unwind-protect
        (let ((default-directory env-dir))
          (with-temp-buffer
            (let ((exit-code (condition-case nil
                                 (customenv--call-process-with-global-env customenv-direnv-executable nil (list t stderr-file) nil "export" "json")
                               (quit
                                (message "interrupted!!")
                                'interrupted))))
              (customenv--debug "Direnv exited with %s and stderr=%S, stdout=%S"
                            exit-code
                            (with-temp-buffer
                              (insert-file-contents stderr-file)
                              (buffer-string))
                            (buffer-string))
              (if (eq 0 exit-code) ;; zerop is not an option, as exit-code may sometimes be a symbol
                  (progn
                    (if (zerop (buffer-size))
                        (setq result 'none)
                      (goto-char (point-min))
                      (prog1
                          (setq result (let ((json-key-type 'string)) (json-read-object)))
                        (when customenv-show-summary-in-minibuffer
                          (customenv--show-summary result env-dir)))))
                (message "Direnv failed in %s" env-dir)
                (setq result 'error))
              (customenv--at-end-of-special-buffer "*customenv*"
                (insert "──── " (format-time-string "%Y-%m-%d %H:%M:%S") " ──── " env-dir " ────\n\n")
                (let ((initial-pos (point)))
                  (insert-file-contents stderr-file)
                  (goto-char (point-max))
                  (let (ansi-color-context)
                    (ansi-color-apply-on-region initial-pos (point)))
                  (add-face-text-property initial-pos (point) (if (eq 0 exit-code) 'success 'error)))
                (insert "\n\n")
                (when (and (numberp exit-code) (/= 0 exit-code))
                  (display-buffer (current-buffer)))))))
      (delete-file stderr-file))
    result))

;; Forward declaration for the byte compiler
(defvar eshell-path-env)

(defun customenv--merged-environment (process-env pairs)
  "Make a `process-environment' value that merges PROCESS-ENV with PAIRS.
PAIRS is an alist obtained from direnv's output.
Values from PROCESS-ENV will be included, but their values will
be masked by Emacs' handling of `process-environment' if they
also appear in PAIRS."
  (append (mapcar (lambda (pair)
                    (if (cdr pair)
                        (format "%s=%s" (car pair) (cdr pair))
                      ;; Plain env name is the syntax for unsetting vars
                      (car pair)))
                  pairs)
          process-env))

(defun customenv--clear (buf)
  "Remove any effects of `customenv-mode' from BUF."
  (with-current-buffer buf
    (kill-local-variable 'exec-path)
    (kill-local-variable 'process-environment)
    (kill-local-variable 'info-directory-list)
    (when (derived-mode-p 'eshell-mode)
      (if (fboundp 'eshell-set-path)
          (eshell-set-path (butlast exec-path))
        (kill-local-variable 'eshell-path-env)))))


(defun customenv--apply (buf result)
  "Update BUF with RESULT, which is a result of `customenv--export'."
  (with-current-buffer buf
    (setq-local customenv--status (if (listp result) 'on result))
    (if (memq result '(none error))
        (progn
          (customenv--clear buf)
          (customenv--debug "[%s] reset environment to default" buf))
      (customenv--debug "[%s] applied merged environment" buf)
      (let* ((remote (when-let* ((fn (buffer-file-name buf)))
                       (file-remote-p fn)))
             (env (customenv--merged-environment
                   (default-value (if remote
                                      'tramp-remote-process-environment
                                    'process-environment))
                   result))
             (path (getenv-internal "PATH" env))
             (parsed-path (parse-colon-path path)))
        (if remote
            (setq-local tramp-remote-process-environment env)
          (setq-local process-environment env))
        ;; Get PATH from the merged environment: direnv may not have changed it
        (if remote
            (setq-local customenv--remote-path parsed-path)
          (setq-local exec-path parsed-path))
        (when (derived-mode-p 'eshell-mode)
          (if (fboundp 'eshell-set-path)
              (eshell-set-path path)
            (setq-local eshell-path-env path)))
        (when-let* ((info-path (getenv-internal "INFOPATH" env)))
          (setq-local Info-directory-list
                      (seq-filter #'identity (parse-colon-path info-path))))))))

(defun customenv--update-env (env-dir)
  "Refresh the state of the direnv in ENV-DIR and apply in all relevant buffers."
  (customenv--debug "Invalidating cache for env %s" env-dir)
  (cl-loop for k being the hash-keys of customenv--cache
           if (string-prefix-p (concat env-dir "\0") k)
           do (remhash k customenv--cache))
  (customenv--debug "Refreshing all buffers in env  %s" env-dir)
  (dolist (buf (customenv--mode-buffers))
    (with-current-buffer buf
      (when (string= (customenv--find-env-dir) env-dir)
        (customenv--update)))))

(defun customenv--mode-buffers ()
  "Return a list of all live buffers in which `customenv-mode' is enabled."
  (seq-filter (lambda (b) (and (buffer-live-p b)
                               (with-current-buffer b
                                 customenv-mode)))
              (buffer-list)))

(defmacro customenv--with-required-current-env (varname &rest body)
  "With VARNAME set to the current env dir path, execute BODY.
If there is no current env dir, abort with a user error."
  (declare (indent 1))
  (cl-assert (symbolp varname))
  `(let ((,varname (customenv--find-env-dir)))
     (unless ,varname
       (user-error "No enclosing .customenv"))
     ,@body))

(defun customenv--call-process-with-global-env (&rest args)
  "Like `call-process', but always use the global process environment.
In particular, we ensure the default variable `exec-path' and
`process-environment' are used.  This ensures an .customenv doesn't take
`customenv-direnv-executable' out of our path.
ARGS is as for `call-process'."
  (let ((exec-path (default-value 'exec-path))
        (process-environment (default-value 'process-environment)))
    (apply 'process-file args)))

(defun customenv-reload ()
  "Reload the current env."
  (interactive)
  (customenv--with-required-current-env env-dir
    (let* ((default-directory env-dir)
           (exit-code (customenv--call-process-with-global-env customenv-direnv-executable nil (get-buffer-create "*customenv-reload*") nil "reload")))
      (if (zerop exit-code)
          (customenv--update-env env-dir)
        (display-buffer "*customenv-reload*")
        (user-error "Error running direnv reload")))))

(defun customenv-allow ()
  "Run \"direnv allow\" in the current env."
  (interactive)
  (customenv--with-required-current-env env-dir
    (let* ((default-directory env-dir)
           (exit-code (customenv--call-process-with-global-env customenv-direnv-executable nil (get-buffer-create "*customenv-allow*") nil "allow")))
      (if (zerop exit-code)
          (customenv--update-env env-dir)
        (display-buffer "*customenv-allow*")
        (user-error "Error running direnv allow")))))

(defun customenv-deny ()
  "Run \"direnv deny\" in the current env."
  (interactive)
  (customenv--with-required-current-env env-dir
    (let* ((default-directory env-dir)
           (exit-code (customenv--call-process-with-global-env customenv-direnv-executable nil (get-buffer-create "*customenv-deny*") nil "deny")))
      (if (zerop exit-code)
          (customenv--update-env env-dir)
        (display-buffer "*customenv-deny*")
        (user-error "Error running direnv deny")))))

(defun customenv-reload-all ()
  "Reload direnvs for all buffers.
This can be useful if a .customenv has been deleted."
  (interactive)
  (customenv--debug "Invalidating cache for all envs")
  (clrhash customenv--cache)
  (dolist (buf (customenv--mode-buffers))
    (with-current-buffer buf
      (customenv--update))))

(defun customenv-show-log ()
  "Open customenv log buffer."
  (interactive)
  (if-let* ((buffer (get-buffer "*customenv*")))
      (pop-to-buffer buffer)
    (message "Customenv log buffer does not exist")))


;;; Propagate local environment to commands that use temp buffers

(defun customenv-propagate-environment (orig &rest args)
  "Advice function to wrap a command ORIG and make it use our local env.
This can be used to force compliance where ORIG starts processes
in a temp buffer.  ARGS is as for ORIG."
  (if customenv-mode
      (inheritenv (apply orig args))
    (apply orig args)))

(defun customenv-propagate-tramp-environment (buf)
  "Advice function to propagate `tramp-remote-path' and
`tramp-remote-process-environment' from buffer local values."
  (when customenv-mode
    (let ((cur-path customenv--remote-path)
          (cur-env tramp-remote-process-environment))
      (with-current-buffer buf
        (setq-local tramp-remote-process-environment cur-env)
        (setq-local customenv--remote-path cur-path))))
  buf)

(defun customenv-get-remote-path (fn vec)
  "Advice function to wrap FN (`tramp-get-remote-path'),
with its argument VEC.
Shortcuts tramp caching direnv sets the variable `exec-path'."
  (with-current-buffer (tramp-get-connection-buffer vec)
    (or customenv--remote-path
        (apply fn vec nil))))

(advice-add 'shell-command :around #'customenv-propagate-environment)
(advice-add 'org-babel-eval :around #'customenv-propagate-environment)
(advice-add 'org-export-file :around #'customenv-propagate-environment)
(advice-add 'tramp-get-connection-buffer :filter-return #'customenv-propagate-tramp-environment)
(advice-add 'tramp-get-remote-path :around #'customenv-get-remote-path)

;;; Major mode for .customenv files

;; Generate direnv keywords with:
;;     $ rg "Usage:\s+([^_]\w+)" DIRENV_SRC/stdlib.sh -Nor '"$1"' | sort | uniq
(defvar customenv-file-extra-keywords
  '("MANPATH_add" "PATH_add" "PATH_rm" "direnv_apply_dump" "direnv_layout_dir"
    "direnv_load" "direnv_version" "dotenv" "dotenv_if_exists"
    "env_vars_required" "expand_path" "fetchurl" "find_up" "has" "join_args"
    "layout" "load_prefix" "log_error" "log_status" "on_git_branch" "path_add"
    "path_rm" "rvm" "semver_search" "source_env" "source_env_if_exists"
    "source_up" "source_up_if_exists" "source_url" "strict_env" "unstrict_env"
    "use" "use_flake" "use_flox" "use_guix" "use_nix" "use_vim" "user_rel_path"
    "watch_dir" "watch_file")
  "Useful direnv keywords to be highlighted.")

(declare-function sh-set-shell "sh-script")

;;;###autoload
(define-derived-mode customenv-file-mode
  sh-mode "customenv"
  "Major mode for .customenv files as used by direnv.
\\{customenv-file-mode-map}"
  (sh-set-shell "bash")
  (font-lock-add-keywords
   nil `((,(regexp-opt customenv-file-extra-keywords 'symbols)
          (0 font-lock-keyword-face)))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.customenv\\'" . customenv-file-mode))


(provide 'customenv)
;;; customenv.el ends here

;; LocalWords:  customenv direnv

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
