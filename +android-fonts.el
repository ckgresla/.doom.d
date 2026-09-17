;;; +android-fonts.el --- CJK fallback for native Android -*- lexical-binding: t; -*-

;; Keep JetBrains Mono NL and Inter as the primary faces.  Android's bundled
;; CJK collections use outlines that sfnt-android cannot read; install the
;; TrueType Noto Sans CJK JP variable font in ~/fonts instead.  Despite the
;; JP name, this full-coverage font includes Chinese and Korean as well.
;; See docs/android-fonts.md for the download, license and checksum.

(defvar my/android-cjk-font-configured nil)

(defun my/android-setup-cjk-font (&optional frame)
  "Supply CJK glyphs without changing Latin fonts, sizes or symbol fonts."
  (when (and (eq system-type 'android)
             (display-graphic-p frame)
             (not my/android-cjk-font-configured))
    (let ((font (font-spec :family "Noto Sans CJK JP")))
      (when (find-font font frame)
        ;; Prefer the known-working font over Samsung's Hangul fallback.
        ;; Do not remap `symbol' or all of Unicode: that would replace Doom's
        ;; icons and punctuation that the primary fonts already render.
        (dolist (script '(han kana hangul cjk-misc bopomofo))
          (set-fontset-font t script font nil 'prepend))
        (setq my/android-cjk-font-configured t)))))

;; The default fontset is shared by faces and frames, and survives Doom font
;; reloads.  Register once; retry when a GUI frame becomes available.
(add-hook 'after-make-frame-functions #'my/android-setup-cjk-font)
(add-hook 'after-setting-font-hook #'my/android-setup-cjk-font)
(my/android-setup-cjk-font)

(provide '+android-fonts)
