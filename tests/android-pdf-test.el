;;; android-pdf-test.el --- Android PDF safety regressions -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -l tests/android-pdf-test.el
(require 'ert)
(require 'cl-lib)
(defvar pdf-view-inhibit-hotspots nil)
(defvar pdf-view-mode-hook nil)
(defvar pdf-view-roll-minor-mode nil)
(defvar-local pdf-view-display-size nil)
(defvar-local my/pdf-raw-touch-points nil)
(defvar-local my/pdf-raw-touch-primary nil)
(defvar-local my/pdf-raw-touch-last-xy nil)
(defvar-local my/pdf-raw-touch-moved nil)
(defvar-local my/pdf-raw-touch-pinch-distance nil)
(defvar-local my/pdf-raw-touch-start-time 0)
(defvar-local my/pdf-pinch-base-scale nil)
(defvar-local my/pdf-pinch-ratio 1.0)
(defvar-local my/pdf-pinch-timer nil)
(defvar my/pdf-pinch-idle-delay)
(defvar my/pdf-pinch-min-scale)
(defvar my/pdf-pinch-max-scale)
(defconst android-pdf-test-config
  (expand-file-name "../+android-pdf.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun android-pdf-test-load-touch-functions ()
  "Load only the gesture defuns, without executing Android initialization."
  (let ((wanted '(my/pdf-raw-touch-reset my/pdf-raw-touch-distance
                  my/pdf-raw-touch-begin my/pdf-raw-touch-update
                  my/pdf-raw-touch-end my/android-pinch-event-ratio
                  my/pdf-current-scale my/pdf-apply-pinch my/pdf-touch-pinch
                  my/pdf-touch-scroll)))
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "+android.el"
                         (file-name-directory android-pdf-test-config)))
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (eq (car-safe form) 'defun)
                         (memq (cadr form) wanted))
                (eval form t)
                (setq wanted (delq (cadr form) wanted)))))
        (end-of-file nil)))
    (when wanted (error "Missing Android gesture definitions: %S" wanted))))

(defun android-pdf-test-position (window x y)
  "Build an event position for WINDOW at pixel coordinates X and Y."
  (list window 1 (cons x y) 0))

(ert-deftest android-pdf-desktop-keeps-hotspots ()
  (let ((system-type 'darwin)
        (features (cons 'pdf-view features))
        (after-load-alist nil)
        (pdf-view-mode-hook nil)
        (pdf-view-inhibit-hotspots nil))
    (load android-pdf-test-config nil t)
    (should-not (default-value 'pdf-view-inhibit-hotspots))))

(ert-deftest android-pdf-disables-hotspots-before-first-render ()
  (let ((system-type 'android)
        (features (cons 'pdf-view features))
        (after-load-alist nil)
        (pdf-view-mode-hook nil)
        (pdf-view-inhibit-hotspots nil))
    (load android-pdf-test-config nil t)
    (with-temp-buffer
      (should pdf-view-inhibit-hotspots))))

(ert-deftest android-pdf-deferred-load-is-also-safe ()
  (let ((system-type 'android)
        (features (remq 'pdf-view (copy-sequence features)))
        (after-load-alist nil)
        (pdf-view-mode-hook nil)
        (pdf-view-inhibit-hotspots nil))
    (load android-pdf-test-config nil t)
    (should-not pdf-view-inhibit-hotspots)
    ;; Simulate the feature becoming available outside this test file's load.
    (let ((load-file-name nil)) (provide 'pdf-view))
    (should pdf-view-inhibit-hotspots)))

(ert-deftest android-pdf-pinch-preserves-safe-scale ()
  (load android-pdf-test-config nil t)
  (should (= 3.5 (my/android-pdf-limit-scale 3.5 '(595 . 842)))))

(ert-deftest android-pdf-pinch-bounds-large-page-allocations ()
  (load android-pdf-test-config nil t)
  (dolist (size '((595 . 842) (1440 . 2880) (2880 . 1440)))
    (let ((scale (my/android-pdf-limit-scale 4.0 size)))
      (should (<= scale 4.0))
      (should (<= (* scale scale (car size) (cdr size))
                  (+ 0.01 my/android-pdf-max-render-pixels))))))

(ert-deftest android-pdf-roll-default-is-android-only ()
  (dolist (platform '(darwin android))
    (let ((system-type platform)
          (features (cons 'pdf-view features))
          (after-load-alist nil)
          (pdf-view-mode-hook nil)
          (pdf-view-inhibit-hotspots nil))
      (load android-pdf-test-config nil t)
      (should (eq (not (null (memq #'my/android-pdf-enable-roll
                                   pdf-view-mode-hook)))
                  (eq platform 'android))))))

(ert-deftest android-pdf-roll-default-enables-after-deferred-load ()
  (let ((system-type 'android)
        (features (remq 'pdf-view (copy-sequence features)))
        (after-load-alist nil)
        (pdf-view-mode-hook nil)
        (pdf-view-inhibit-hotspots nil)
        calls)
    (load android-pdf-test-config nil t)
    (let ((load-file-name nil)) (provide 'pdf-view))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (should (eq feature 'pdf-roll))
                 t))
              ((symbol-function 'pdf-view-roll-minor-mode)
               (lambda (arg) (push arg calls))))
      (with-temp-buffer
        (run-hooks 'pdf-view-mode-hook)))
    (should (equal calls '(1)))))

(ert-deftest android-pdf-roll-missing-package-is-safe ()
  (load android-pdf-test-config nil t)
  (let (called)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename noerror)
                 (should (eq feature 'pdf-roll))
                 (should noerror)
                 nil))
              ((symbol-function 'pdf-view-roll-minor-mode)
               (lambda (&rest _) (setq called t))))
      (my/android-pdf-enable-roll))
    (should-not called)))

(ert-deftest android-pdf-roll-missing-command-is-safe ()
  (load android-pdf-test-config nil t)
  (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
            ((symbol-function 'pdf-view-roll-minor-mode) nil))
    (should-not (fboundp 'pdf-view-roll-minor-mode))
    (my/android-pdf-enable-roll)))

(ert-deftest android-pdf-roll-scroll-preserves-pixels-and-direction ()
  (load android-pdf-test-config nil t)
  (let ((pdf-view-roll-minor-mode t)
        (window (selected-window))
        calls)
    (cl-letf (((symbol-function 'pdf-roll-scroll-forward)
               (lambda (&rest args) (push (cons 'forward args) calls)))
              ((symbol-function 'pdf-roll-scroll-backward)
               (lambda (&rest args) (push (cons 'backward args) calls)))
              ((symbol-function 'pdf-view-next-line-or-next-page)
               (lambda (&rest _) (ert-fail "Roll scroll used line navigation")))
              ((symbol-function 'pdf-view-previous-line-or-previous-page)
               (lambda (&rest _) (ert-fail "Roll scroll used line navigation"))))
      (my/android-pdf-scroll-pixels 27 window)
      (my/android-pdf-scroll-pixels -13 window))
    (should (equal (nreverse calls)
                   `((forward 27 ,window t) (backward 13 ,window t))))))

(ert-deftest android-pdf-nonroll-scroll-keeps-line-navigation ()
  (load android-pdf-test-config nil t)
  (let ((pdf-view-roll-minor-mode nil)
        calls)
    (cl-letf (((symbol-function 'pdf-view-next-line-or-next-page)
               (lambda (count) (push (cons 'next count) calls)))
              ((symbol-function 'pdf-view-previous-line-or-previous-page)
               (lambda (count) (push (cons 'previous count) calls)))
              ((symbol-function 'pdf-roll-scroll-forward)
               (lambda (&rest _) (ert-fail "Inactive roll mode scrolled")))
              ((symbol-function 'pdf-roll-scroll-backward)
               (lambda (&rest _) (ert-fail "Inactive roll mode scrolled"))))
      (my/android-pdf-scroll-pixels 27 (selected-window))
      (my/android-pdf-scroll-pixels -13 (selected-window)))
    (should (equal (nreverse calls) '((next . 1) (previous . 1))))))

(ert-deftest android-pdf-image-spec-accepts-images-and-slices ()
  (load android-pdf-test-config nil t)
  (let ((image '(image :type png :data "test-page")))
    (should (equal (my/android-pdf-image-spec image) image))
    (should (equal (my/android-pdf-image-spec
                    (list '(slice 0 0 100 100) image))
                   image))
    (should-not (my/android-pdf-image-spec '(space :height 1000)))
    (should-not (my/android-pdf-image-spec nil))))

(ert-deftest android-pdf-roll-overlay-lookup-ignores-window-highlights ()
  (load android-pdf-test-config nil t)
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "a\nb\nc\n")
      (let* ((window (selected-window))
             (other-window (split-window-right))
             (page (make-overlay 1 2))
             (margin (make-overlay 3 4))
             (other-page (make-overlay 5 6))
             ;; A window-local highlight can cover both image and margin text.
             (highlight (make-overlay 1 6)))
        (set-window-buffer other-window (current-buffer))
        (overlay-put page 'window window)
        (overlay-put page 'category 'pdf-roll)
        (overlay-put margin 'window window)
        (overlay-put margin 'category 'pdf-roll-margin)
        (overlay-put other-page 'window other-window)
        (overlay-put other-page 'category 'pdf-roll)
        (overlay-put highlight 'window window)
        (overlay-put highlight 'face 'hl-line)
        (overlay-put highlight 'priority 100)
        (should (eq (my/android-pdf-roll-pos-overlay 1 window) page))
        (should (eq (my/android-pdf-roll-pos-overlay 3 window) margin))
        ;; Neither a highlight for this window nor a page for another window
        ;; may stand in for a missing page overlay.
        (should-not (my/android-pdf-roll-pos-overlay 5 window))
        (should-not (my/android-pdf-roll-pos-overlay 1 other-window))
        (should (eq (my/android-pdf-roll-pos-overlay 5 other-window)
                    other-page))))))

(ert-deftest android-pdf-roll-redisplay-targets-buffer-windows ()
  (load android-pdf-test-config nil t)
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let* ((pdf-buffer (current-buffer))
             (first (selected-window))
             (second (split-window-right))
             (unrelated (split-window-below))
             calls)
        (set-window-buffer second pdf-buffer)
        (with-temp-buffer
          (set-window-buffer unrelated (current-buffer))
          (select-window unrelated)
          (let ((original (lambda (window)
                            (should (eq (current-buffer) pdf-buffer))
                            (push window calls)
                            'redisplayed)))
            ;; Theme changes run for each PDF buffer while an unrelated
            ;; buffer can remain selected in its own window.
            (with-current-buffer pdf-buffer
              (my/android-pdf-roll-redisplay original t))
            (should (= (length calls) 2))
            (should (memq first calls))
            (should (memq second calls))
            (should-not (memq unrelated calls))
            (should (eq (selected-window) unrelated))
            (setq calls nil)
            (with-current-buffer pdf-buffer
              (should (eq (my/android-pdf-roll-redisplay original second)
                          'redisplayed))
              (should (eq (my/android-pdf-roll-redisplay original)
                          'redisplayed)))
            (should (equal (nreverse calls) (list second nil)))))))))

(ert-deftest android-pdf-native-cache-retains-visible-roll-images ()
  (load android-pdf-test-config nil t)
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "a\nb\n")
      (let* ((frame (selected-frame))
             (visible '(image :type png :data "visible-page"))
             (sliced '(image :type png :data "visible-slice"))
             (retired '(image :type png :data "retired-page"))
             (first (make-overlay 1 2))
             (second (make-overlay 3 4))
             flushed)
        (dolist (overlay (list first second))
          (overlay-put overlay 'category 'pdf-roll)
          (overlay-put overlay 'window (selected-window)))
        (overlay-put first 'display visible)
        (overlay-put second 'display (list '(slice 0 0 10 10) sliced))
        (cl-letf (((symbol-function 'image-flush)
                   (lambda (image &optional image-frame)
                     (push (list image image-frame) flushed))))
          (my/android-pdf-flush-retired-images
           (list (copy-tree visible) (copy-tree sliced) retired) frame)
          (should (equal flushed (list (list retired frame))))
          ;; Removing the visible reference makes that image eligible too.
          (overlay-put first 'display '(space :height 1000))
          (setq flushed nil)
          (my/android-pdf-flush-retired-images (list visible sliced) frame)
          (should (equal flushed (list (list visible frame)))))))))

(ert-deftest android-pdf-native-cache-retains-another-window-image ()
  (load android-pdf-test-config nil t)
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "a\n")
      (let* ((frame (selected-frame))
             (other-window (split-window-right))
             (image '(image :type png :data "shared-page"))
             (overlay (make-overlay 1 2))
             flushed)
        (set-window-buffer other-window (current-buffer))
        (overlay-put overlay 'category 'pdf-roll)
        (overlay-put overlay 'window other-window)
        (overlay-put overlay 'display image)
        (cl-letf (((symbol-function 'image-flush)
                   (lambda (spec &optional image-frame)
                     (push (list spec image-frame) flushed))))
          (my/android-pdf-flush-retired-images (list image) frame)
          (should-not flushed)
          ;; A stale overlay owned by a deleted window no longer pins pixels.
          (delete-window other-window)
          (my/android-pdf-flush-retired-images (list image) frame)
          (should (equal flushed (list (list image frame)))))))))

(ert-deftest android-pdf-raw-slow-drag-accumulates-and-keeps-small-deltas ()
  (android-pdf-test-load-touch-functions)
  (with-temp-buffer
    (let ((window (selected-window))
          deltas tapped)
      (cl-letf (((symbol-function 'my/pdf-touch-scroll)
                 (lambda (event) (push (nth 3 event) deltas)))
                ((symbol-function 'my/pdf-touch-tap)
                 (lambda (&rest _) (setq tapped t))))
        (my/pdf-raw-touch-begin
         (list 'touchscreen-begin
               (cons 1 (android-pdf-test-position window 100 100))))
        (dolist (y '(96 92))
          (my/pdf-raw-touch-update
           (list 'touchscreen-update
                 (list (cons 1 (android-pdf-test-position window 100 y))))))
        (should-not deltas)
        ;; The third small move crosses the total threshold; the fourth stays
        ;; below that threshold but must still move an already active drag.
        (dolist (y '(89 88))
          (my/pdf-raw-touch-update
           (list 'touchscreen-update
                 (list (cons 1 (android-pdf-test-position window 100 y))))))
        (should (equal (nreverse deltas) '(11 1)))
        (my/pdf-raw-touch-end
         (list 'touchscreen-end
               (cons 1 (android-pdf-test-position window 100 88)) nil))
        (should-not tapped)
        (should-not my/pdf-raw-touch-points)))))

(ert-deftest android-pdf-pinch-pause-retains-original-scale-baseline ()
  (load android-pdf-test-config nil t)
  (android-pdf-test-load-touch-functions)
  (let ((my/pdf-pinch-idle-delay 0.22)
        (my/pdf-pinch-min-scale 0.6)
        (my/pdf-pinch-max-scale 4.0))
    (save-window-excursion
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (setq major-mode 'pdf-view-mode
              pdf-view-display-size 2.0
              my/pdf-raw-touch-points '((1 . primary) (2 . secondary)))
        (let (renders)
          (cl-letf (((symbol-function 'run-with-idle-timer)
                     (lambda (&rest _) 'scheduled-pinch))
                    ((symbol-function 'pdf-view-current-page) (lambda (&rest _) 1))
                    ((symbol-function 'pdf-cache-pagesize) (lambda (&rest _) '(595 . 842)))
                    ((symbol-function 'pdf-view-redisplay)
                     (lambda (&rest _) (push pdf-view-display-size renders))))
            (my/pdf-touch-pinch (list 'touchscreen-pinch (selected-window) 1.5))
            ;; Run the idle callback while both fingers remain on the screen.
            (my/pdf-apply-pinch (current-buffer))
            (should (= pdf-view-display-size 3.0))
            (should (= my/pdf-pinch-base-scale 2.0))
            ;; Continuing the same gesture reports another cumulative ratio.
            (my/pdf-touch-pinch (list 'touchscreen-pinch (selected-window) 1.6))
            (my/pdf-apply-pinch (current-buffer))
            (should (= pdf-view-display-size 3.2))
            (should (= my/pdf-pinch-base-scale 2.0))
            (should (equal (nreverse renders) '(3.0 3.2)))))))))

(ert-deftest android-pdf-pinch-secondary-release-rebases-remaining-drag ()
  (android-pdf-test-load-touch-functions)
  (with-temp-buffer
    (let ((window (selected-window))
          (commits 0)
          deltas)
      (cl-letf (((symbol-function 'my/pdf-touch-scroll)
                 (lambda (event) (push (nth 3 event) deltas)))
                ((symbol-function 'my/pdf-touch-pinch) (lambda (&rest _) nil))
                ((symbol-function 'my/pdf-apply-pinch)
                 (lambda (&rest _) (cl-incf commits)))
                ((symbol-function 'my/pdf-touch-tap)
                 (lambda (&rest _) (ert-fail "Pinch release became a tap"))))
        (my/pdf-raw-touch-begin
         (list 'touchscreen-begin
               (cons 1 (android-pdf-test-position window 100 100))))
        (my/pdf-raw-touch-begin
         (list 'touchscreen-begin
               (cons 2 (android-pdf-test-position window 200 100))))
        (my/pdf-raw-touch-update
         (list 'touchscreen-update
               (list (cons 1 (android-pdf-test-position window 100 140))
                     (cons 2 (android-pdf-test-position window 220 140)))))
        (should-not deltas)
        (my/pdf-raw-touch-end
         (list 'touchscreen-end
               (cons 2 (android-pdf-test-position window 220 140)) nil))
        (should (= commits 1))
        (should (= my/pdf-raw-touch-primary 1))
        (should (equal my/pdf-raw-touch-last-xy '(100 . 140)))
        (should-not my/pdf-raw-touch-pinch-distance)
        (my/pdf-raw-touch-update
         (list 'touchscreen-update
               (list (cons 1 (android-pdf-test-position window 100 145)))))
        (should (equal deltas '(-5)))))))

(ert-deftest android-pdf-pinch-selects-the-pdf-window-for-idle-render ()
  (load android-pdf-test-config nil t)
  (android-pdf-test-load-touch-functions)
  (let ((my/pdf-pinch-min-scale 0.6)
        (my/pdf-pinch-max-scale 4.0))
    (save-window-excursion
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (setq major-mode 'pdf-view-mode
              pdf-view-display-size 2.0
              my/pdf-pinch-base-scale 2.0
              my/pdf-pinch-ratio 1.25)
        (let ((pdf-buffer (current-buffer))
              (pdf-window (selected-window))
              rendered)
          (with-temp-buffer
            (let ((other-window (split-window-right)))
              (set-window-buffer other-window (current-buffer))
              (select-window other-window)
              (cl-letf (((symbol-function 'pdf-view-current-page)
                         (lambda (&rest _)
                           (should (eq (selected-window) pdf-window))
                           (should (eq (current-buffer) pdf-buffer))
                           1))
                        ((symbol-function 'pdf-cache-pagesize)
                         (lambda (&rest _) '(595 . 842)))
                        ((symbol-function 'pdf-view-redisplay)
                         (lambda (window)
                           (should (eq window pdf-window))
                           (should (eq (selected-window) pdf-window))
                           (should (eq (current-buffer) pdf-buffer))
                           (setq rendered t))))
                (my/pdf-apply-pinch pdf-buffer))
              (should rendered)
              (should (eq (selected-window) other-window))
              (with-current-buffer pdf-buffer
                (should (= pdf-view-display-size 2.5))
                (should-not my/pdf-pinch-base-scale)))))))))

(defmacro android-pdf-test-with-pan-window (&rest body)
  "Run BODY with a real WINDOW and controllable PDF image pixel metrics."
  (declare (indent 0) (debug t))
  `(progn
     (load android-pdf-test-config nil t)
     (save-window-excursion
       (with-temp-buffer
         (switch-to-buffer (current-buffer))
         (setq major-mode 'pdf-view-mode)
         (setq-local auto-hscroll-mode nil)
         (let ((window (selected-window))
               (image-width 1000)
               (body-width 500)
               (char-width 10)
               (props (make-hash-table :test #'equal)))
           (set-window-hscroll window 0)
           (cl-letf (((symbol-function 'pdf-view-image-size)
                      (lambda (displayed &optional win _page)
                        (should displayed)
                        (should (eq (or win (selected-window)) window))
                        (cons image-width 2000)))
                     ((symbol-function 'window-body-width)
                      (lambda (&optional win pixels)
                        (should (eq (or win (selected-window)) window))
                        (should pixels)
                        body-width))
                     ((symbol-function 'frame-char-width)
                      (lambda (&optional _frame) char-width))
                     ((symbol-function 'image-mode-window-get)
                      (lambda (prop &optional win)
                        (gethash (cons (or win (selected-window)) prop) props)))
                     ((symbol-function 'image-mode-window-put)
                      (lambda (prop value &optional win)
                        (puthash (cons (or win (selected-window)) prop)
                                 value props))))
             ,@body))))))

(ert-deftest android-pdf-horizontal-pan-keeps-native-and-saved-offsets ()
  (android-pdf-test-with-pan-window
    (my/android-pdf-pan-pixels 120 window)
    (should (= (window-hscroll window) 12))
    (should (= (image-mode-window-get 'hscroll window) 12))
    (my/android-pdf-pan-pixels -50 window)
    (should (= (window-hscroll window) 7))
    (should (= (image-mode-window-get 'hscroll window) 7))))

(ert-deftest android-pdf-horizontal-pan-accumulates-sub-column-motion ()
  (android-pdf-test-with-pan-window
    (dotimes (_ 20) (my/android-pdf-pan-pixels 1 window))
    (should (= (window-hscroll window) 2))
    (dotimes (_ 10) (my/android-pdf-pan-pixels -1 window))
    (should (= (window-hscroll window) 1))
    (should (= (nth 1 (image-mode-window-get 'my/android-pdf-pan-state window))
               10))))

(ert-deftest android-pdf-horizontal-pan-clamps-and-reverses-at-edges ()
  (android-pdf-test-with-pan-window
    ;; Non-integral overflow still exposes the final pixels of the page.
    (setq image-width 1003)
    (my/android-pdf-pan-pixels 2000 window)
    (should (= (window-hscroll window) 51))
    (should (= (nth 1 (image-mode-window-get 'my/android-pdf-pan-state window))
               503))
    (my/android-pdf-pan-pixels -20 window)
    (should (= (window-hscroll window) 48))
    (my/android-pdf-pan-pixels -2000 window)
    (should (= (window-hscroll window) 0))
    ;; Clipped overscroll must not leave a debt that consumes reverse motion.
    (my/android-pdf-pan-pixels 20 window)
    (should (= (window-hscroll window) 2))))

(ert-deftest android-pdf-horizontal-pan-clamps-after-zoom-out ()
  (android-pdf-test-with-pan-window
    (my/android-pdf-pan-pixels 400 window)
    (setq image-width 603)
    (my/android-pdf-pan-pixels 0 window)
    (should (= (window-hscroll window) 11))
    (should (= (image-mode-window-get 'hscroll window) 11))
    ;; Fit-width/fit-page must clear both native and persistent offsets.
    (setq image-width body-width)
    (my/android-pdf-pan-pixels 0 window)
    (should (= (window-hscroll window) 0))
    (should (= (image-mode-window-get 'hscroll window) 0))
    (should (= (nth 1 (image-mode-window-get 'my/android-pdf-pan-state window))
               0))))

(ert-deftest android-pdf-horizontal-pan-resyncs-external-scroll-and-font ()
  (android-pdf-test-with-pan-window
    (my/android-pdf-pan-pixels 123 window)
    ;; A keyboard alignment command supersedes the previous touch position.
    (set-window-hscroll window 30)
    (my/android-pdf-pan-pixels 10 window)
    (should (= (window-hscroll window) 31))
    ;; Canonical frame character widths can change independently of zoom.
    (setq char-width 5)
    (my/android-pdf-pan-pixels 5 window)
    (should (= (window-hscroll window) 32))
    (should (= (nth 1 (image-mode-window-get 'my/android-pdf-pan-state window))
               160))))

(ert-deftest android-pdf-scroll-dispatches-both-axes-and-horizontal-only ()
  (load android-pdf-test-config nil t)
  (let ((pdf-view-roll-minor-mode t)
        (window (selected-window))
        calls)
    (cl-letf (((symbol-function 'pdf-roll-scroll-forward)
               (lambda (&rest args) (push (cons 'vertical args) calls)))
              ((symbol-function 'my/android-pdf-pan-pixels)
               (lambda (&rest args) (push (cons 'horizontal args) calls))))
      (my/android-pdf-scroll-pixels 27 window 14)
      (my/android-pdf-scroll-pixels 0 window -9)
      (my/android-pdf-scroll-pixels 0 window 0))
    (should (equal (nreverse calls)
                   `((vertical 27 ,window t)
                     (horizontal 14 ,window)
                     (horizontal -9 ,window)
                     (horizontal 0 ,window))))))

(ert-deftest android-pdf-touch-scroll-preserves-horizontal-event-distance ()
  (android-pdf-test-load-touch-functions)
  (let ((window (selected-window))
        calls)
    (cl-letf (((symbol-function 'my/pdf-touch-cancel-stale-timer)
               (lambda () nil))
              ((symbol-function 'my/android-pdf-scroll-pixels)
               (lambda (&rest args) (push args calls))))
      (my/pdf-touch-scroll (list 'touchscreen-scroll window 25 -17)))
    (should (equal calls (list (list -17 window 25))))))

(ert-deftest android-pdf-roll-redraw-clamps-in-the-pdf-window ()
  (load android-pdf-test-config nil t)
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (setq major-mode 'pdf-view-mode)
      (setq-local pdf-view-roll-minor-mode t)
      (let ((pdf-buffer (current-buffer))
            (window (selected-window))
            calls)
        (with-temp-buffer
          (let ((other (split-window-right)))
            (set-window-buffer other (current-buffer))
            (select-window other)
            (cl-letf (((symbol-function 'my/android-pdf-pan-pixels)
                       (lambda (dx win)
                         (should (eq (current-buffer) pdf-buffer))
                         (push (list dx win) calls))))
              (my/android-pdf-roll-clamp-pan window))
            (should (eq (selected-window) other))
            (should (equal calls (list (list 0 window))))))))))

(ert-run-tests-batch-and-exit "^android-pdf-")
