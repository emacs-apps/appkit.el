;;; appkit-media-grid.el --- Protocol-neutral sliced media grids -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;;; Commentary:

;; Execute caller-supplied bands of prepared image cells.  Acquisition,
;; topology selection and playback mounting remain with their existing owners.

;;; Code:

(require 'cl-lib)
(require 'appkit-ui)
(require 'appkit-media-image)

(cl-defstruct (appkit-media-grid-cell
               (:constructor appkit-media-grid-cell-create))
  "One protocol-neutral grid item.
IMAGE is a prepared image descriptor, or nil to display FALLBACK text.
ACTION is a zero-argument function; PROPERTIES and HELP-ECHO describe the
inserted cell, including padding below shorter images.  Descriptors can be
reused: each grid insertion creates fresh slice occurrences."
  image fallback action properties help-echo)

(defun appkit-media-grid--prepare-cell (cell)
  "Return a (ROWS . PADDING) pair for typed CELL in the current buffer."
  (cl-check-type cell appkit-media-grid-cell)
  (let* ((image (appkit-media-grid-cell-image cell))
         (rows (and image (appkit-media-image-slice-rows image))))
    (if rows
        (let* ((display (get-text-property 0 'display (car rows)))
               (render-image (appkit-media--display-image-spec display))
               (width (max 1 (ceiling
                              (or (car (ignore-errors (image-size render-image)))
                                  1)))))
          (cons (vconcat rows)
                (propertize " " 'display `(space :width ,width))))
      (let ((fallback (or (appkit-media-grid-cell-fallback cell) "[image]")))
        (cons (vector fallback)
              (make-string (max 1 (string-width fallback)) ?\s))))))

(defun appkit-media-grid--insert-cell (cell prepared row)
  "Insert slice ROW of CELL using its PREPARED rows and padding."
  (let ((start (point))
        (rows (car prepared)))
    (insert (if (< row (length rows)) (aref rows row) (cdr prepared)))
    (when (< start (point))
      (when-let* ((properties (appkit-media-grid-cell-properties cell)))
        (add-text-properties start (point) properties))
      (appkit-ui-add-fallback-action
       start (point) (appkit-media-grid-cell-action cell)
       :help-echo (appkit-media-grid-cell-help-echo cell))
      ;; Cell actions and occurrence metadata must never spill into gutters.
      (put-text-property start (point) 'rear-nonsticky t))))

(cl-defun appkit-media-insert-grid
    (cells &key bands band-slices (gap 0) prefix prefix-face)
  "Insert typed CELLS as caller-supplied sliced BANDS and return their span.
BANDS is a list of lists of zero-based cell indices; nil means one band with
all cells.  BAND-SLICES is a positive integer, or nil to use the tallest cell
in each band.  A cell repeated in successive bands resumes after the slices
reserved for it in earlier bands.  Short cells receive width-matched padding.
GAP is the pixel gutter between columns.  PREFIX is display-only text or a
mutable Appkit prefix state; PREFIX-FACE styles it without changing body text.
No final newline is inserted.  Empty CELLS returns nil.  Prepared descriptors
remain inert: this executor does not acquire resources or mount playback."
  (when cells
    (when (and band-slices
               (not (and (integerp band-slices) (> band-slices 0))))
      (error "Grid band-slices must be a positive integer"))
    (let* ((items (vconcat cells))
           (prepared (vconcat (mapcar #'appkit-media-grid--prepare-cell cells)))
           (bands (or bands (list (number-sequence 0 (1- (length items))))))
           (offsets (make-vector (length items) 0))
           (gutter (propertize " " 'display `(space :width (,gap))
                               'rear-nonsticky t))
           (start (point))
           inserted-row)
      (dolist (band bands)
        (when band
          (let ((row-count
                 (or band-slices
                     (apply #'max
                            (mapcar (lambda (index)
                                      (length (car (aref prepared index))))
                                    band)))))
            (dotimes (row row-count)
              (when inserted-row
                (appkit-media-insert-slice-newline))
              (let ((line-start (point)))
                (cl-loop for index in band
                         for column from 0
                         do (when (> column 0) (insert gutter))
                         do (appkit-media-grid--insert-cell
                             (aref items index) (aref prepared index)
                             (+ (aref offsets index) row)))
                (appkit-ui-apply-line-prefix
                 line-start (point) prefix prefix-face))
              (setq inserted-row t))
            (dolist (index (delete-dups (copy-sequence band)))
              (aset offsets index (+ (aref offsets index) row-count))))))
      (when (< start (point))
        (cons start (point))))))

(provide 'appkit-media-grid)
;;; appkit-media-grid.el ends here
