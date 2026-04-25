;;; +android.el --- Android-only config (Po Lu Emacs port) -*- lexical-binding: t; -*-
;;
;; Loaded from config.el only when running on the Android Emacs port,
;; so the same .doom.d works unchanged on macOS / Linux desktops.
;;
;; Contents:
;;   1. Termux <-> Android Emacs PATH / LD_LIBRARY_PATH bridge.
;;      com.termux and org.gnu.emacs share UID on this device, so this
;;      app can use Termux's binaries and shared libraries directly.
;;   2. pdf-tools: pin to a prebuilt epdfinfo (built from Termux side
;;      against Termux's poppler-glib).
;;   3. Content-URI materializer for pdf-view-mode: when a file is
;;      opened via Android Intent (e.g. Chrome → "Open with Emacs"),
;;      the path emacs sees is /content/by-authority-named/...; that
;;      path is virtual and only accessible from inside emacs's
;;      file-name-handler. Subprocesses like epdfinfo cannot fopen it,
;;      so we copy the bytes to a real cache file and rebind the
;;      buffer before pdf-view-mode talks to its subprocess.

;; ----- Termux <-> Android Emacs bridging ---------------------------
;; com.termux and org.gnu.emacs share UID on this device, so this app
;; can use Termux's binaries and shared libraries directly. Without
;; this block, M-x compile, magit's git, async-shell-command, and
;; pdf-tools's autobuild lookup all fail with "command not found"
;; because Android Emacs starts with an empty PATH / LD_LIBRARY_PATH.
(let* ((termux-prefix "/data/data/com.termux/files/usr")
       (termux-bin    (concat termux-prefix "/bin"))
       (termux-lib    (concat termux-prefix "/lib")))
  (when (file-directory-p termux-bin)
    (add-to-list 'exec-path termux-bin)
    (setenv "PATH" (concat termux-bin ":" (or (getenv "PATH") "")))
    (setenv "LD_LIBRARY_PATH"
            (concat termux-lib ":" (or (getenv "LD_LIBRARY_PATH") "")))
    (unless (getenv "TMPDIR")
      (setenv "TMPDIR" (concat termux-prefix "/tmp")))))

;; ----- pdf-tools: use the prebuilt epdfinfo ------------------------
;; The binary was compiled in Termux against Termux poppler-glib.
;; Setting this BEFORE pdf-tools-install runs short-circuits the
;; "autobuild not found" code path entirely.
(setq pdf-info-epdfinfo-program
      (expand-file-name "~/.local/bin/epdfinfo")
      pdf-tools-handle-upgrades nil)




;; ----- Android: open PDFs handed in via Intent / content:// URIs ---
;; Chrome and other apps share files with emacs as content URIs. The
;; Android Emacs port surfaces these as a virtual path like
;; /content/by-authority-named/... that elisp can read via emacs's
;; file-name-handler, but external subprocesses (epdfinfo, rg, etc.)
;; cannot fopen them — they error with "No such file or directory".
;; Materialize the bytes to a real cache file and rebind the buffer
;; before pdf-view-mode connects to epdfinfo.

(defvar-local my/pdf-android-cache-file nil
  "Path of the materialized cache file for this buffer, if any.")

(defun my/pdf-android-materialize-content-uri ()
  "If buffer-file-name is an Android /content/ virtual path, copy
contents to a real file under ~/.cache/pdf-tools-content/ and
rebind the buffer to that path so subprocesses can read it."
  (when (and (stringp buffer-file-name)
             (string-prefix-p "/content/" buffer-file-name))
    (let* ((src buffer-file-name)
           (cache-dir (expand-file-name "~/.cache/pdf-tools-content/"))
           (basename (or (file-name-nondirectory src) "untitled.pdf"))
           (dst (expand-file-name
                 (concat (format-time-string "%s-") basename)
                 cache-dir)))
      (make-directory cache-dir t)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (let ((coding-system-for-read 'no-conversion))
          (insert-file-contents-literally src))
        (let ((coding-system-for-write 'no-conversion))
          (write-region (point-min) (point-max) dst nil 'silent)))
      (set-visited-file-name dst t t)
      (rename-buffer (file-name-nondirectory dst) t)
      (set-buffer-modified-p nil)
      (setq my/pdf-android-cache-file dst)
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when (and my/pdf-android-cache-file
                             (file-exists-p my/pdf-android-cache-file))
                    (ignore-errors (delete-file my/pdf-android-cache-file))))
                nil 'local))))

;; :before advice runs before pdf-view-mode talks to epdfinfo, so by
;; the time the subprocess sees the path, it's a real file.
(advice-add 'pdf-view-mode :before #'my/pdf-android-materialize-content-uri)


;; ----- pdf-view memory + wheel-throttle (Android phone tuning) -----
;; Symptom on the Pixel: a fast 2-finger drag fires many wheel events
;; in quick succession; each that crosses a page boundary triggers a
;; pdf-tools re-render, the image cache balloons, and the GL/UI thread
;; stalls behind the queue. We:
;;   1) Cap pdf-tools' image cache so memory doesn't blow up.
;;   2) Disable page prefetching for the same reason.
;;   3) Around-advise the page/line nav functions to throttle calls
;;      that originate from wheel/touch events; keyboard nav passes
;;      through unchanged (last-input-event isn't a wheel event).

(defvar my/pdf-wheel-last 0
  "Float-time of the last wheel-driven pdf-view navigation call.")
(defconst my/pdf-wheel-interval 0.18
  "Minimum seconds between wheel-driven pdf-view navigation calls.")

(defun my/pdf-wheel-event-p (e)
  "Return non-nil if E is a wheel/scroll/touch event."
  (and (consp e)
       (memq (car e) '(wheel-up wheel-down wheel-left wheel-right
                       mouse-wheel
                       touchscreen-update touchscreen-end touchscreen-scroll
                       drag-mouse-1))))

(defun my/pdf-wheel-throttle (orig-fn &rest args)
  "Throttle ORIG-FN if the triggering event was a wheel/touch event.
Keyboard-driven calls pass through unchanged."
  (if (my/pdf-wheel-event-p last-input-event)
      (let ((now (float-time)))
        (when (> (- now my/pdf-wheel-last) my/pdf-wheel-interval)
          (setq my/pdf-wheel-last now)
          (apply orig-fn args)))
    (apply orig-fn args)))

(after! pdf-view
  ;; Modest image cache so a runaway scroll can't balloon RSS.
  (setq pdf-cache-image-limit 8)
  ;; Kill prefetching: render only what's currently visible.
  (when (boundp 'pdf-cache-prefetch-pages-front-limit)
    (setq pdf-cache-prefetch-pages-front-limit 0))
  (when (fboundp 'pdf-cache-prefetch-minor-mode)
    (pdf-cache-prefetch-minor-mode -1))
  ;; Throttle wheel-driven page nav.
  (advice-add 'pdf-view-next-line-or-next-page :around #'my/pdf-wheel-throttle)
  (advice-add 'pdf-view-previous-line-or-previous-page :around #'my/pdf-wheel-throttle))
