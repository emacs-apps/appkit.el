;;; appkit-media-grid-test.el --- Sliced grid regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-media-grid)

(defun appkit-media-grid-test--image (slices width)
  "Return an inert image descriptor with SLICES rows and WIDTH columns."
  `(image :type png :width ,width :appkit-media-nslices ,slices))

(defmacro appkit-media-grid-test--with-geometry (&rest body)
  "Run BODY with deterministic text-line and image geometry."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'appkit-media--char-pixel-height)
              (lambda () 10))
             ((symbol-function 'image-size)
              (lambda (image &rest _)
                (cons (plist-get (cdr image) :width) 1))))
     ,@body))

(ert-deftest appkit-media-grid-mixed-heights-pad-with-independent-actions ()
  (with-temp-buffer
    (appkit-media-grid-test--with-geometry
      (let* (calls
             (first (lambda () (push 'first calls)))
             (second (lambda () (push 'second calls)))
             (prefix (appkit-ui-make-prefix-state "HEAD " "REST "))
             (span
              (appkit-media-insert-grid
               (list (appkit-media-grid-cell-create
                      :image (appkit-media-grid-test--image 3 8)
                      :action first :properties '(item first))
                     (appkit-media-grid-cell-create
                      :image (appkit-media-grid-test--image 1 5)
                      :action second :properties '(item second)
                      :help-echo "Second"))
               :gap 4 :prefix prefix :prefix-face 'shadow)))
        (should (equal span '(1 . 12)))
        (should (equal (buffer-string) "   \n   \n   "))
        (dotimes (row 3)
          (let ((start (+ 1 (* row 4))))
            (should (equal (get-text-property start 'line-prefix)
                           (if (= row 0) "HEAD " "REST ")))
            (should (equal (get-text-property start 'wrap-prefix) "REST "))
            (should (eq (get-text-property start 'item) 'first))
            (should (eq (get-text-property (+ start 2) 'item) 'second))
            (should (equal (get-text-property (+ start 1) 'display)
                           '(space :width (4))))
            (should-not (get-text-property (+ start 1) appkit-ui-action-property))
            (should-not (get-text-property (+ start 1) 'item))
            (should (appkit-ui-activate-at start))
            (should (appkit-ui-activate-at (+ start 2)))
            (when (> row 0)
              (should (equal (get-text-property (+ start 2) 'display)
                             '(space :width 5))))))
        (should (equal calls '(second first second first second first)))
        (dolist (newline '(4 8))
          (should (eq (get-text-property newline 'line-height) t))
          (should-not (get-text-property newline appkit-ui-action-property)))
        (should (equal (appkit-ui-prefix-state-current prefix) "REST "))))))

(ert-deftest appkit-media-grid-repeated-bands-resume-image-slices ()
  (with-temp-buffer
    (appkit-media-grid-test--with-geometry
      (let ((cells (list
                    (appkit-media-grid-cell-create
                     :image (appkit-media-grid-test--image 4 8)
                     :properties '(item tall))
                    (appkit-media-grid-cell-create
                     :image (appkit-media-grid-test--image 2 4)
                     :properties '(item top))
                    (appkit-media-grid-cell-create
                     :image (appkit-media-grid-test--image 2 4)
                     :properties '(item bottom))))
            tall-image)
        (appkit-media-insert-grid cells :bands '((0 1) (0 2))
                                  :band-slices 2 :gap 2)
        (dotimes (row 4)
          (let* ((start (+ 1 (* row 4)))
                 (left (get-text-property start 'display))
                 (right (get-text-property (+ start 2) 'display)))
            (should (equal (car left) `(slice 0 ,(* row 10) 1.0 10)))
            (should (equal (car right) `(slice 0 ,(* (% row 2) 10) 1.0 10)))
            (should (eq (get-text-property (+ start 2) 'item)
                        (if (< row 2) 'top 'bottom)))
            (if tall-image
                (should (eq tall-image (cadr left)))
              (setq tall-image (cadr left)))))
        (should-not (eq tall-image (appkit-media-grid-cell-image (car cells))))
        (should-not (plist-get (cdr (appkit-media-grid-cell-image (car cells)))
                               :height))))))

(ert-deftest appkit-media-grid-fallback-padding-keeps-metadata-without-action ()
  (with-temp-buffer
    (let* ((fallback (propertize "alt" 'face 'shadow))
           (cell (appkit-media-grid-cell-create
                  :fallback fallback :properties '(item text))))
      (appkit-media-insert-grid (list cell) :band-slices 2 :prefix " > ")
      (should (equal (buffer-string) "alt\n   "))
      (should (eq (get-text-property 1 'face) 'shadow))
      (should (eq (get-text-property 5 'item) 'text))
      (should-not (get-text-property 5 appkit-ui-action-property))
      (should-not (get-text-property 0 'item fallback))
      (let ((copy (appkit-ui-buffer-substring-filter 1 (point-max) nil)))
        (should (equal copy "alt\n   "))
        (should-not (text-property-not-all 0 (length copy) 'line-prefix nil copy))))))

(ert-deftest appkit-media-grid-reused-descriptor-creates-fresh-occurrences ()
  (with-temp-buffer
    (appkit-media-grid-test--with-geometry
      (let* ((image (appkit-media-grid-test--image 2 8))
             (_ (setcdr image (plist-put (cdr image) :appkit-media-playback t)))
             (cell (appkit-media-grid-cell-create :image image))
             (first (appkit-media-insert-grid (list cell)))
             (occurrence (get-text-property (car first) 'appkit-media-occurrence)))
        (should occurrence)
        (should (eq occurrence (get-text-property 3 'appkit-media-occurrence)))
        (insert "\n")
        (let ((second (appkit-media-insert-grid (list cell))))
          (should (get-text-property (car second) 'appkit-media-occurrence))
          (should-not (eq occurrence
                          (get-text-property (car second) 'appkit-media-occurrence))))))))

(ert-deftest appkit-media-grid-empty-input-does-not-consume-prefix ()
  (with-temp-buffer
    (let ((prefix (appkit-ui-make-prefix-state "first" "rest")))
      (should-not (appkit-media-insert-grid nil :prefix prefix))
      (should (equal (appkit-ui-prefix-state-current prefix) "first"))
      (should (= (point-min) (point-max))))))

(provide 'appkit-media-grid-test)
;;; appkit-media-grid-test.el ends here
