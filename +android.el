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


;; ----- pdf-view touch UX (Android) --------------------------------
;; Defaults that misbehave with a finger:
;;   - [down-mouse-1] = pdf-view-mouse-set-mark  → "mark set" on tap
;;   - [drag-mouse-1] = pdf-view-mouse-extend-region → text region drag
;;   - [pinch] (via global touch-screen-display-pinch-to-zoom = t)
;;     adjusts text-scale, which on pdf-view chains into per-event
;;     pdf-view-enlarge / epdfinfo re-renders and crashes the app.
;; Plan:
;;   1) No-op down-mouse-1/mouse-1 so touches don't print/select.
;;   2) drag-mouse-1 → page turn, throttled to one fire / 0.4s.
;;   3) Disable global pinch handler entirely; zoom via keyboard
;;      (+ / - / 0). We'll layer a bespoke pinch handler back later
;;      once we know the event shape Android actually emits.

(defvar my/pdf-drag-last 0
  "Float-time of the last accepted drag-mouse-1 in pdf-view.")
(defconst my/pdf-drag-interval 0.4
  "Minimum seconds between accepted single-finger drags.")

(defun my/pdf-noop (&rest _)
  "No-op binding to suppress unwanted pdf-view mouse defaults."
  (interactive))

(defun my/pdf-touch-drag (event)
  "Single-finger drag → page turn, throttled.
Sign: drag-up (finger-up) = next page; drag-down = prev page."
  (interactive "e")
  (condition-case err
      (let ((now (float-time)))
        (when (> (- now my/pdf-drag-last) my/pdf-drag-interval)
          (setq my/pdf-drag-last now)
          (let* ((s (posn-x-y (event-start event)))
                 (e (posn-x-y (event-end event)))
                 (dy (- (cdr s) (cdr e)))
                 (dx (- (car s) (car e))))
            (cond
             ((>  dy 60)  (pdf-view-next-page))
             ((< dy -60)  (pdf-view-previous-page))
             ((>  dx 60)  (pdf-view-next-page))
             ((< dx -60)  (pdf-view-previous-page))))))
    (error (message "[pdf-drag] %S" err))))

(after! pdf-view
  ;; Stop the global pinch->text-scale handler from intercepting pinches
  ;; in pdf-view buffers (which used to crash the app).
  (setq touch-screen-display-pinch-to-zoom nil)
  (map! :map pdf-view-mode-map
        [down-mouse-1] #'my/pdf-noop
        [mouse-1]      #'my/pdf-noop
        [drag-mouse-1] #'my/pdf-touch-drag))


;; ----- harder PDF safety: pinch nuked, cache tightened, prefetch off
;; Symptoms still seen on Pixel after the previous block:
;;   - first pinch crashes the app (no minibuffer error -> native-side
;;     handler or sequenced event is reaching pdf-view before the
;;     `after!' setq took effect).
;;   - third page-turn freezes (cache limit 8 still too big -- render
;;     of the 4th page tips memory over).
;; This block is overcautious by design; we'll relax once stable.

;; Disable the global pinch->text-scale handler at TOP LEVEL so it's
;; off before pdf-view ever loads.
(setq touch-screen-display-pinch-to-zoom nil)

;; Defensive: bind every flavor of pinch event we know to a no-op,
;; both globally and in pdf-view-mode-map. If the Android port emits
;; under any of these names, the keymap lookup hits a no-op instead
;; of any default behavior.
(dolist (key '([pinch] [touchscreen-pinch] [touch-screen-pinch]))
  (define-key global-map key #'my/pdf-noop))

(after! pdf-view
  ;; Tighter image cache (4 pages ~= ~120 MB peak on this device).
  (setq pdf-cache-image-limit 4)
  ;; Same nukes in the pdf-view-mode-map for redundancy.
  (dolist (key '([pinch] [touchscreen-pinch] [touch-screen-pinch]))
    (define-key pdf-view-mode-map key #'my/pdf-noop))
  ;; Turn off the prefetch minor-mode for every pdf buffer.
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (when (fboundp 'pdf-cache-prefetch-minor-mode)
                (pdf-cache-prefetch-minor-mode -1)))))

;; ----- one-shot probe: dump Android-port-specific touch/pinch vars
;; on the next emacs start so we can see what controls native pinch
;; (the Termux emacs we ssh into doesn't have these symbols).
(let ((dump "/data/data/org.gnu.emacs/files/.cache/android-emacs-probe.txt"))
  (when (file-directory-p "/data/data/org.gnu.emacs")
    (make-directory (file-name-directory dump) t)
    (with-temp-file dump
      (insert (format ";; emacs %s on %s\n" emacs-version system-type))
      (insert (format ";; system-configuration-features = %s\n\n"
                      system-configuration-features))
      (mapatoms
       (lambda (s)
         (when (and (boundp s)
                    (or (string-match-p "android" (symbol-name s))
                        (string-match-p "pinch"   (symbol-name s))
                        (string-match-p "touch-screen" (symbol-name s))))
           (condition-case nil
               (insert (format "%-50s = %S\n" s (symbol-value s)))
             (error nil))))))))
