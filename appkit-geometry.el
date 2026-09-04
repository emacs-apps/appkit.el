;;; appkit-geometry.el --- Window presentation geometry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Window-local measurement and display-space primitives.  This module has no
;; App, Surface, or client lifecycle ownership.

;;; Code:

(require 'cl-lib)

(defun appkit-geometry-display-window (&optional buffer frame-predicate)
  "Return the canonical live window displaying BUFFER.

The selected window wins when it displays BUFFER.  Otherwise return the
widest live window displaying BUFFER across all frames.  Optional
FRAME-PREDICATE restricts candidates by their owning frame."
  (unless (or (null frame-predicate) (functionp frame-predicate))
    (error "Appkit display frame predicate is not callable: %S"
           frame-predicate))
  (let* ((buffer (or buffer (current-buffer)))
         (selected (selected-window))
         (eligible-p
          (lambda (window)
            (and (window-live-p window)
                 (eq (window-buffer window) buffer)
                 (or (null frame-predicate)
                     (funcall frame-predicate (window-frame window)))))))
    (if (funcall eligible-p selected)
        selected
      (let ((best nil)
            (best-width -1))
        (dolist (window (get-buffer-window-list buffer nil t) best)
          (when (funcall eligible-p window)
            (let ((width (window-width window 'remap)))
              (when (> width best-width)
                (setq best window
                      best-width width)))))))))

(defun appkit-geometry-display-frame (&optional buffer frame-predicate)
  "Return the frame of BUFFER's canonical display window."
  (when-let* ((window
               (appkit-geometry-display-window buffer frame-predicate)))
    (window-frame window)))

(defun appkit-geometry-default-line-pixel-height ()
  "Return the default-face line height for the current buffer in pixels."
  (save-excursion
    (let ((window (appkit-geometry-display-window)))
      (max 1
           (or (and window
                    (eq (window-buffer window) (current-buffer))
                    (ignore-errors (window-font-height window 'default)))
               (ignore-errors (default-line-height))
               (frame-char-height)
               16)))))

(defun appkit-geometry-columns-pixel-width (columns &optional window)
  "Return pixel width for COLUMNS using WINDOW's remapped buffer metrics."
  (let* ((win (or window (appkit-geometry-display-window)))
         (frame (and (window-live-p win) (window-frame win)))
         (buffer (and (window-live-p win) (window-buffer win)))
         (char-width
          (or (and (frame-live-p frame)
                   (display-graphic-p frame)
                   (fboundp 'string-pixel-width)
                   (ignore-errors
                     (string-pixel-width
                      (propertize "0" 'face 'default)
                      buffer)))
              (and (frame-live-p frame)
                   (let* ((font (ignore-errors (face-font 'default frame)))
                          (info (and font (ignore-errors (font-info font frame)))))
                     (when info
                       (let ((width (aref info 11)))
                         (if (> width 0) width (aref info 10))))))
              (and (frame-live-p frame) (frame-char-width frame))
              (frame-char-width)
              1)))
    (* (max 0 columns)
       (if (and (frame-live-p frame) (display-graphic-p frame))
           (max 1 char-width)
         1))))

(defun appkit-geometry-current-column ()
  "Like `current-column', accounting for prior absolute alignment spacers."
  (let* ((bol (line-beginning-position))
         (point-now (point))
         (scan point-now)
         align-column)
    (while (and (not align-column)
                (> scan bol)
                (setq scan (previous-single-char-property-change
                            scan 'display nil bol)))
      (let ((display (get-text-property scan 'display)))
        (when (and (listp display)
                   (> (length display) 2)
                   (eq (nth 0 display) 'space)
                   (eq (nth 1 display) :align-to))
          (let ((align-value (nth 2 display)))
            (setq align-column
                  (+ (if (listp align-value)
                         (ceiling
                          (/ (or (car align-value) 0)
                             (float
                              (max 1
                                   (appkit-geometry-columns-pixel-width 1)))))
                       (or align-value 0))
                     (string-width
                      (buffer-substring scan point-now))))))))
    (or align-column (current-column))))

(cl-defun appkit-geometry-display-space
    (&key (columns nil columns-p)
          (pixels nil pixels-p)
          (align-to nil align-to-p))
  "Return one display-only space sized by COLUMNS, PIXELS, or ALIGN-TO."
  (unless (= 1 (length (delq nil (list columns-p pixels-p align-to-p))))
    (error "Appkit display space requires exactly one sizing argument"))
  (when (and columns-p
             (not (and (numberp columns) (>= columns 0))))
    (error "Appkit display-space columns are invalid: %S" columns))
  (when (and pixels-p
             (not (and (numberp pixels) (>= pixels 0))))
    (error "Appkit display-space pixels are invalid: %S" pixels))
  (propertize
   " "
   'display
   (cond
    (columns-p `(space :width ,columns))
    (pixels-p `(space :width (,pixels)))
    (t `(space :align-to ,align-to)))
   'rear-nonsticky '(display)))

(defun appkit-geometry-insert-alignment-space (target-column)
  "Insert an absolute display spacer targeting TARGET-COLUMN."
  (let* ((target (max 0 (or target-column 0)))
         (window (appkit-geometry-display-window))
         (frame (and (window-live-p window) (window-frame window)))
         (align-to
          (if (and (frame-live-p frame) (display-graphic-p frame))
              (list (appkit-geometry-columns-pixel-width target window))
            target)))
    (insert (appkit-geometry-display-space :align-to align-to))))

(defun appkit-geometry-window-width (&optional window margin-columns)
  "Return usable columns for WINDOW, reserving MARGIN-COLUMNS at the right."
  (let ((window (or window (appkit-geometry-display-window))))
    (when (window-live-p window)
      (let* ((margins (window-margins window))
             (width (+ (window-width window 'remap)
                       (or (car margins) 0)
                       (or (cdr margins) 0)))
             (line-numbers-p
              (with-current-buffer (window-buffer window)
                (bound-and-true-p display-line-numbers-mode)))
             (line-number-pixels
              (if line-numbers-p
                  (with-selected-window window
                    (line-number-display-width 'pixels))
                0))
             (character-pixels
              (max 1 (appkit-geometry-columns-pixel-width 1 window)))
             (line-number-columns
              (if (and (numberp line-number-pixels)
                       (> line-number-pixels 0))
                  (ceiling (/ line-number-pixels (float character-pixels)))
                0)))
        (max 1 (- width
                  (max 0 (or margin-columns 0))
                  line-number-columns))))))

(provide 'appkit-geometry)

;;; appkit-geometry.el ends here
