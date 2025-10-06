;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "JetBrains Mono" :size 12)
      doom-variable-pitch-font (font-spec :family "Inter" :size 12)
      doom-big-font (font-spec :family "JetBrains Mono" :size 18))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-flatwhite)
(require 'kaolin-themes)
(require 'ef-themes)

(defun my/apply-theme (appearance)
  "
  Load theme, taking current system APPEARANCE into consideration.
  the good looking themes:
  - modus-operandi
  - modus-vivendi
  - doom-plain
  - doom-monokai-ristretto
  - doom-homage-white
  - doom-homage-black
  - doom-one-light
  - doom-one
  "
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'doom-flatwhite t))
    ('dark (load-theme 'doom-meltbus t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)


;; native fullscreen behavior on macos -- emacs-plus things
(setq ns-use-native-fullscreen t)


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; ya organize your life in plain-text right?
(setq org-directory "~/life/org")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.



;; CKG's Config

(add-to-list 'initial-frame-alist '(width . 120))
(add-to-list 'initial-frame-alist '(height . 70))

;; auto-open the hidden long outputs in compilation buffers
(after! compile
  (setq compilation-max-output-line-length nil)
  (setq compilation-hide-output nil))

;; enable evil mode in all minibuffers!
(evil-collection-init 'minibuffer)

;; KEYBINDINGS
;; if one uses emacs and evil mode, chances are you like to hack
(map! :leader
      (:prefix "c"
        :desc "Shell Command" "s" #'shell-command
        :desc "Async Shell Command" "a" #'async-shell-command
        :desc "Grep" "g" #'grep
        :desc "Kill Current Compilation" "C-c" #'kill-compilation))

;; nicer lookup binding
(map! :leader "s k" #'+lookup/documentation)
;; `occur' is a wonderful emacs command
(map! :leader
      (:prefix-map ("s" . "search")
       :desc "Occur" "o" #'occur))


;; in magit, write/close commits with ':wq' as opposed to only "C-c C-c"
(after! evil
  (add-hook! 'git-commit-mode-hook
    (evil-define-key 'normal with-editor-mode-map
      "ZZ" #'with-editor-finish
      "ZQ" #'with-editor-cancel)))

;; the magic of browse-at-remote, quickly
(map! :leader
      "S-<return>" #'browse-at-remote)


;; markdown mode, continue indentation and items on `RET'
(after! markdown-mode
  (setq markdown-indent-on-enter 'indent-and-new-item))


;; override default doom yank file path, force fullpath, avoid abbreviating home dir as: "~", and strip TRAMP prefixes (i.e: "/sshx:")
(defun ckg/yank-buffer-or-dir-full-path ()
  "Copy buffer's full file or dir path, stripping TRAMP prefix but keeping hostname"
  (interactive)
  (let* ((filename (or (buffer-file-name)
                       (and (eq major-mode 'dired-mode)
                            (expand-file-name (dired-current-directory))))))
    (if filename
        (let* ((fullpath (expand-file-name filename))
               ;; Strip trailing slash for directories
               (fullpath (if (file-directory-p fullpath)
                             (directory-file-name fullpath)
                           fullpath)))
          (if (file-remote-p fullpath)
              ;; It's a TRAMP path: keep hostname, strip method
              (let* ((vec (tramp-dissect-file-name fullpath))
                     (host (tramp-file-name-host vec))
                     (local (tramp-file-name-localname vec))
                     (result (concat host ":" local)))
                (kill-new result)
                (message "Copied full path: %s" result))
            ;; Local file or directory
            (kill-new fullpath)
            (message "Copied full path: %s" fullpath)))
      (user-error "Current buffer is not visiting a file or directory"))))

(map! :leader
      (:prefix ("f" . "file")
       :desc "Yank buffer full path" "y" #'ckg/yank-buffer-or-dir-full-path))


;; global text scaling that persists across buffers
(use-package! default-text-scale
  :config
  (default-text-scale-mode))

;; enable soft wrapping of lines globally
(global-visual-line-mode 1)

;; scrolling so smooth, "i cannot believe its not butter"
(use-package! ultra-scroll
  :init
  (setq scroll-conservatively 3    ; or whatever value you prefer
        scroll-margin 0)           ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))


;; bind option+del and cmd+del (not backspace but del) to delete the next word/line, similar to default on MacOS
(global-set-key (kbd "M-<delete>") 'kill-word)
(global-set-key (kbd "s-<delete>") 'kill-line)

;; Set key bindings using Command (s-) instead of Control for zooming
;; s- prefix is for the Command/Super key on macOS
(map! "s-=" #'default-text-scale-increase  ;; Command+=
      "s--" #'default-text-scale-decrease  ;; Command+-
      "s-0" #'default-text-scale-reset)    ;; Command+0

;; Also add leader key bindings as a backup
(map! :leader
      (:prefix ("z" . "zoom")
       :desc "Increase font size" "+" #'default-text-scale-increase
       :desc "Decrease font size" "-" #'default-text-scale-decrease
       :desc "Reset font size"    "0" #'default-text-scale-reset))
(use-package! default-text-scale
  :config
  (default-text-scale-mode))

;; Set default key bindings (Ctrl +/- for zooming)
(map! :leader
      (:prefix ("z" . "zoom")
       :desc "Increase font size" "+" #'default-text-scale-increase
       :desc "Decrease font size" "-" #'default-text-scale-decrease
       :desc "Reset font size"    "0" #'default-text-scale-reset))


;; vim
;; H and L to nudge screen when reach edge from first press of keys
(after! evil
  (defvar last-command-was-h nil)
  (defvar last-command-was-l nil)

  (defun smart-evil-h ()
    "Move to top of screen, or scroll up if already at top."
    (interactive)
    (if (and last-command-was-h
             (= (line-number-at-pos) (line-number-at-pos (window-start))))
        (evil-scroll-up 10)
      (evil-window-top))
    (setq last-command-was-h t)
    (setq last-command-was-l nil))

  (defun smart-evil-l ()
    "Move to bottom of screen, or scroll down if already at bottom."
    (interactive)
    (let ((bottom-line (save-excursion
                         (move-to-window-line -1)
                         (line-number-at-pos))))
      (if (and last-command-was-l
               (= (line-number-at-pos) bottom-line))
          (evil-scroll-down 10)
        (evil-window-bottom)))
    (setq last-command-was-l t)
    (setq last-command-was-h nil))

  ;; Reset the state variables when other commands are executed
  (advice-add 'evil-command-window-execute :before
              (lambda (&rest _) (setq last-command-was-h nil
                                      last-command-was-l nil)))

  ;; Bind the new functions
  (evil-define-key '(normal visual) 'global
    "H" 'smart-evil-h
    "L" 'smart-evil-l))


;; Workspaces
;; enable workspace auto-save and auto-resume
(setq persp-auto-save-opt 1)
(add-hook 'kill-emacs-hook #'persp-save-state-to-file)

;; Force projects to open and be accessible only in a single/current workspace
(after! persp-mode
  (setq persp-is-ibc-as-persp t) ; Isolate buffers per workspace
  (setq persp-auto-resume-time -1) ; Don't auto-resume
  (setq persp-auto-save-opt 1)) ; Auto-save workspaces

(after! projectile
  ;; command for opening projectile projects, dired
  (setq projectile-switch-project-action #'projectile-dired)
  ;; Prevent projectile from killing buffers on its own
  (setq projectile-kill-buffers-filter 'projectile-kill-buffers-filter-none))


;; TRAMP -- better remote development
(require 'tramp)
(setq tramp-default-method "sshx")  ;; using gui emacs
(setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
(setq tramp-chunksize 2000)
;; Configure TRAMP to allow editing remote files by default
(after! tramp
  ;; Disable read-only mode for remote files
  (setq tramp-allow-unsafe-temporary-files t)

  ;; Don't prompt for confirmation when saving to remote files
  (setq remote-file-name-inhibit-locks nil)

  ;; Don't ask for confirmation when saving
  (setq tramp-save-ad-hoc-proxies t)

  ;; Add a hook to disable read-only mode for TRAMP buffers
  (defun +make-remote-writable-if-possible ()
    "If visiting a remote file and it is writable, make buffer writable."
    (when (and buffer-file-name
               (file-remote-p buffer-file-name)
               (file-writable-p buffer-file-name))
      (read-only-mode -1)
      (message "Made remote buffer writable: %s" buffer-file-name)))


)

;; Add to multiple hooks
;; (add-hook 'find-file-hook #'+make-remote-writable-if-possible)
;; (add-hook 'after-find-file #'+make-remote-writable-if-possible)
;; (add-hook 'dired-mode-hook #'+make-remote-writable-if-possible)

(add-hook 'find-file-hook
  (defun +make-remote-writable-if-possible ()
    "If visiting a remote file and it is writable, make buffer writable."
    (when (and buffer-file-name
             (file-remote-p buffer-file-name)
             (file-writable-p buffer-file-name))
    (read-only-mode -1))))


;; projectile
;; better find file behavior over ssh/tramp
;; (after! projectile
;;   ;; Use native indexing for remote projects
;;   (setq projectile-indexing-method 'native)

;;   ;; Force Projectile to use the correct command for remote files
;;   (setq projectile-generic-command
;;         (cond ((executable-find "fd")
;;                "fd . -0 --type f --color=never --strip-cwd-prefix")
;;               ((executable-find "fdfind")
;;                "fdfind . -0 --type f --color=never --strip-cwd-prefix")
;;               ((executable-find "rg")
;;                "rg --files --color=never --null -0")
;;               ((executable-find "find")
;;                "find . -type f -print0"))))

;; (setq projectile-track-known-projects-automatically nil)
(setq projectile-verbose t)  ;; DEBUG


;; enable custom.el file
(setq custom-file (expand-file-name "custom.el" doom-user-dir))
(when (file-exists-p custom-file)
  (load custom-file))



;; Fix vterm shell for TRAMP - use login shell on remote
(setq vterm-tramp-shells '(("sshx" login-shell "/bin/zsh")))


;; magit -- the best git
(after! magit
  ;; don't show the diff by default in the commit buffer. Use `spc g s' to display it
  (setq magit-commit-show-diff nil)
  ;; don't show git variables in magit branch
  (setq magit-branch-direct-configure nil)
  ;; don't automatically refresh the status buffer after running a git command
  (setq magit-refresh-status-buffer nil)
)

;; LSP Configurations
;; configure LSP to show symbol info on mouse hover
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-mouse t)
;; watch files + increase default threshold
(after! lsp-mode
  (setq lsp-enable-file-watchers t)
  (setq lsp-file-watch-threshold 10000))

;; Python
(require 'flycheck-mypy)
(add-hook 'python-mode-hook 'flycheck-mode)

;; Rust
(after! lsp-rust
  (setq lsp-rust-analyzer-server-command '("/Users/ckg/.cargo/bin/rust-analyzer")))

(after! exec-path-from-shell
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs '("PATH"))))

;; Or explicitly add the cargo bin path
(add-to-list 'exec-path "/Users/ckg/.cargo/bin")
(setenv "PATH" (concat "/Users/ckg/.cargo/bin:" (getenv "PATH")))


;; CUDA config
;; Configure cuda-mode for proper file associations
(use-package! cuda-mode
  :mode "\\.cu\\'"
  :mode "\\.cuh\\'"
  :config
  ;; Optional: if you want .h files in CUDA projects to be treated as C++ headers
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))




;; evil-easymotion -- function and form
;; permit combinations of QWERTY chars as easymotion targets
(after! avy
  (setq avy-keys (append (number-sequence ?a ?z))))

;; hook, set custom evil-mode stuff on theme change
(defun my/avy-easymotion-face-overrides ()
  (dolist (face '(avy-lead-face avy-lead-face-0 avy-lead-face-1 avy-background-face))
    (when (facep face)
      (pcase face
        ('avy-lead-face
         (set-face-attribute face nil :foreground "#ff5f5f" :background nil :weight 'bold))
        ('avy-lead-face-0
         (set-face-attribute face nil :foreground "#ff5f5f" :background nil :weight 'bold))
        ('avy-lead-face-1
         (set-face-attribute face nil :foreground "#ff5f5f" :background nil :weight 'bold))
        ('avy-background-face
         (set-face-attribute face nil :foreground "#888888" :background nil))))))

(add-hook 'doom-load-theme-hook #'my/avy-easymotion-face-overrides)

;; Optionally, also run after avy loads, in case theme loads before avy
(with-eval-after-load 'avy
  (my/avy-easymotion-face-overrides))


;; locate -- a wonderful thing ala `spc f l`
;; make consult + locate remote aware, prepending the TRAMP remote prefix for search results
(defun my/consult-locate--find-file-advice (orig-fn file &rest args)
  "If in a TRAMP buffer, prefix locate result with TRAMP prefix before opening."
  (let ((tramp-prefix (and (tramp-tramp-file-p default-directory)
                           (file-remote-p default-directory))))
    (if (and tramp-prefix
             (string-prefix-p "/" file)
             (not (string-prefix-p tramp-prefix file)))
        (apply orig-fn (concat tramp-prefix file) args)
      (apply orig-fn file args))))

(advice-add #'find-file :around #'my/consult-locate--find-file-advice)

;; TODO: figure out a way to use different executables for locate & consult-locate, depending on the computer a buffer is present on
;; Use Spotlight for locate on macOS -- BSD locate don't jive w the doom emacs
;; Configure both instances of the canoncial cmds im doom: locate and consult-locate
;; in remote situations, default back to the usual `locate'
;; (when IS-MAC
;;   (setq locate-command "mdfind")
;;   (after! consult
;;     (setq consult-locate-args "mdfind")))



;; A DEMONSTRATION -- of how IT IS fucking possible to have something sane in TRAMP work with conda environments...
;; is this even possible...
;; Add to TRAMP remote path
;; (add-to-list 'tramp-remote-path "/opt/miniconda/wafer/bin")

;; ;; Set environment for remote connections
;; (connection-local-set-profile-variables
;;  'remote-conda-profile
;;  '((process-environment . ("PATH=/opt/miniconda/wafer/bin:/usr/local/bin:/usr/bin:/bin"
;;                           "CONDA_DEFAULT_ENV=wafer"))))
;; (connection-local-set-profiles
;;  '(:application tramp :protocol "sshx")
;;  'remote-conda-profile)



(after! compile
  (setq compilation-shell-minor-mode t)

  ;; Custom compilation environment
  (defun my/compilation-setup ()
    "Setup compilation with login shell environment."
    (setq-local shell-file-name "/bin/zsh")
    (setq-local shell-command-switch "-lc"))

  (add-hook 'compilation-mode-hook #'my/compilation-setup))





;; FIXES
; as of 2025-07-10, emacs-plus doesn't jive w MacOS's sed cmd (regex issue)
;; solution, is to use gnu sed instead, get it with: `brew install gnu-sed'
(setq Man-sed-command "gsed")



