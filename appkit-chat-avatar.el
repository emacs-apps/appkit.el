;;; appkit-chat-avatar.el --- Shared two-line chat avatars  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Telega renders a chat avatar as two image slices: the upper slice prefixes
;; the sender heading and the lower slice prefixes the first content row.  This
;; module exposes that geometry as protocol-neutral prefix strings so chat
;; clients can share the same layout through `appkit-ui' prefix states.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-media-image)
(require 'appkit-geometry)

(defun appkit-chat-avatar--spacing-pixels (spacing font-height)
  "Return SPACING converted to pixels relative to FONT-HEIGHT."
  (cond
   ((integerp spacing) spacing)
   ((floatp spacing) (round (* spacing font-height)))
   ((consp spacing)
    (+ (appkit-chat-avatar--spacing-pixels (car spacing) font-height)
       (appkit-chat-avatar--spacing-pixels (cdr spacing) font-height)))
   (t 0)))

(defun appkit-chat-avatar-line-pixel-height ()
  "Return one default-face text line's pixel height in the current buffer."
  ;; `window-font-height' can move point to WINDOW's window-point when WINDOW
  ;; is not selected.  Avatar geometry is commonly measured from inside an
  ;; EWOC printer while another window has focus, so leaking that movement
  ;; makes the remainder of the row print at an unrelated buffer position.
  (save-excursion
    (let ((window (appkit-geometry-display-window)))
      (max
       1
       (or (and window
                (let* ((font-height
                        (ignore-errors
                          (window-font-height window 'default)))
                       (spacing
                        (or line-spacing
                            (frame-parameter (window-frame window)
                                             'line-spacing))))
                  (and (numberp font-height)
                       (+ font-height
                          (appkit-chat-avatar--spacing-pixels
                           spacing font-height)))))
           ;; `default-line-height' already includes buffer/frame line spacing.
           (ignore-errors (default-line-height))
           (frame-char-height)
           16)))))

(defun appkit-chat-avatar-column-pixel-width ()
  "Return one default-face text column's pixel width in the current buffer."
  ;; Keep geometry queries observational: window font APIs may select the
  ;; queried window's point as an implementation detail.
  (save-excursion
    (let ((window (appkit-geometry-display-window)))
      (max 1
           (or (and window
                    (ignore-errors (window-font-width window 'default)))
               (and (fboundp 'string-pixel-width)
                    (ignore-errors
                      (string-pixel-width
                       (propertize " " 'face 'default)
                       (current-buffer))))
               (frame-char-width)
               1)))))

(defun appkit-chat-avatar--render-frame ()
  "Return an image-capable frame used to render avatars."
  (or (appkit-media-image-capable-frame (current-buffer))
      (appkit-geometry-display-frame (current-buffer))
      (selected-frame)))

(defun appkit-chat-avatar-two-line-pixel-size ()
  "Return the pixel size of an avatar occupying exactly two text lines."
  (* 2 (appkit-chat-avatar-line-pixel-height)))

(defun appkit-chat-avatar-resize-image (image pixel-size)
  "Return avatar IMAGE normalized to square PIXEL-SIZE.

Avatar sources are expected to be square.  Both axes are deliberately set so
malformed non-square sources cannot destabilize two-line layout geometry."
  (when (appkit-media-image-object-valid-p image)
    (let* ((type (car image))
           (properties (copy-sequence (cdr image))))
      (setq properties (plist-put properties :width pixel-size))
      (setq properties (plist-put properties :height pixel-size))
      (setq properties (plist-put properties :ascent 'center))
      (cons type properties))))

(defun appkit-chat-avatar--fit-text (text width)
  "Return TEXT truncated or padded to exactly WIDTH columns."
  (let* ((target (max 1 width))
         (trimmed (truncate-string-to-width (or text "") target nil nil ""))
         (trim-width (string-width trimmed)))
    (if (< trim-width target)
        (concat trimmed (make-string (- target trim-width) ?\s))
      trimmed)))

(defun appkit-chat-avatar--image-text (image slice-index)
  "Return textual fallback stored on IMAGE for SLICE-INDEX."
  (let ((text (and (consp image)
                   (plist-get (cdr image) :appkit-chat-avatar-text))))
    (cond
     ((stringp text) text)
     ((and (listp text)
           (integerp slice-index)
           (>= slice-index 0)
           (< slice-index (length text)))
      (nth slice-index text))
     (t nil))))

(defun appkit-chat-avatar-image-char-width (image)
  "Return IMAGE's rendered width in text columns."
  (or (and (consp image)
           (let ((width (plist-get (cdr image)
                                   :appkit-chat-avatar-char-width)))
             (and (integerp width) (> width 0) width)))
      (let* ((size (and (appkit-media-image-object-valid-p image)
                        (ignore-errors
                          (image-size image t
                                      (appkit-chat-avatar--render-frame)))))
             (width-pixels (and (consp size) (car size)))
             (char-width (appkit-chat-avatar-column-pixel-width)))
        (max 1
             (if (numberp width-pixels)
                 (ceiling (/ (float width-pixels) (float char-width)))
               1)))))

(cl-defun appkit-chat-avatar--prepare-image
    (image fallback &key pixel-size resize slice-height)
  "Return IMAGE decorated for two-line slicing.

FALLBACK supplies the textual first slice.  PIXEL-SIZE is used when RESIZE is
non-nil.  SLICE-HEIGHT overrides the inferred one-line pixel height."
  (let* ((target-size (or pixel-size
                          (appkit-chat-avatar-two-line-pixel-size)))
         (base-image (if resize
                         (appkit-chat-avatar-resize-image image target-size)
                       image)))
    (when (appkit-media-image-object-valid-p base-image)
      (let* ((size (ignore-errors
                     (image-size base-image t
                                 (appkit-chat-avatar--render-frame))))
             (height-pixels (or (and (consp size) (cdr size)) target-size))
             (width-pixels (or (and (consp size) (car size)) target-size))
             (width-chars (appkit-chat-avatar-image-char-width base-image))
             (effective-slice-height
              (or slice-height
                  (and (consp base-image)
                       (plist-get (cdr base-image)
                                  :appkit-chat-avatar-slice-height))
                  (max 1 (floor (/ (float height-pixels) 2)))))
             (type (car base-image))
             (properties (copy-sequence (cdr base-image)))
             (top-text (appkit-chat-avatar--fit-text fallback width-chars))
             (bottom-text (make-string width-chars ?\u00a0)))
        (setq properties
              (plist-put properties :appkit-chat-avatar-char-width width-chars))
        (setq properties
              (plist-put properties :appkit-chat-avatar-pixel-width
                         width-pixels))
        (setq properties
              (plist-put properties :appkit-chat-avatar-slice-height
                         effective-slice-height))
        (setq properties
              (plist-put properties :appkit-chat-avatar-text
                         (list top-text bottom-text)))
        (cons type properties)))))

(defun appkit-chat-avatar--slice-display (image slice-index)
  "Return an Emacs display specification for IMAGE SLICE-INDEX."
  (when (appkit-media-image-object-valid-p image)
    (let* ((size (ignore-errors
                   (image-size image t (appkit-chat-avatar--render-frame))))
           (height (and (consp size) (cdr size)))
           (slice-height (or (and (consp image)
                                  (plist-get
                                   (cdr image)
                                   :appkit-chat-avatar-slice-height))
                             (and (numberp height)
                                  (max 1 (floor (/ (float height) 2))))))
           (slice-y (and slice-height (* slice-height slice-index)))
           (remaining (and (numberp height) slice-y (- height slice-y)))
           (visible-height (and slice-height
                                (if (numberp remaining)
                                    (max 1 (min slice-height remaining))
                                  slice-height))))
      (when (and slice-y visible-height)
        (list (list 'slice 0 slice-y 1.0 visible-height) image)))))

(defun appkit-chat-avatar--slice-string (image slice-index)
  "Return a display string for IMAGE SLICE-INDEX."
  (let* ((text (or (appkit-chat-avatar--image-text image slice-index)
                   (make-string
                    (appkit-chat-avatar-image-char-width image) ?\s)))
         (display (appkit-chat-avatar--slice-display image slice-index)))
    (if display
        (propertize text 'display display 'rear-nonsticky '(display))
      text)))

(defun appkit-chat-avatar--pixel-spacer-string (columns pixel-width)
  "Return fallback text spanning COLUMNS columns.

On graphical displays it occupies exactly PIXEL-WIDTH pixels."
  (let ((text (make-string (max 1 columns) ?\u00a0)))
    (if (and (display-graphic-p (appkit-chat-avatar--render-frame))
             (numberp pixel-width)
             (> pixel-width 0))
        (propertize text
                    'display `(space :width (,(round pixel-width)))
                    'rear-nonsticky '(display))
      text)))

(defun appkit-chat-avatar--pad-prefix (prefix width)
  "Right-pad PREFIX so it occupies WIDTH columns."
  (let* ((text (or prefix ""))
         (target (max 0 width))
         (current (max 0 (string-width text))))
    (if (< current target)
        (concat text (make-string (- target current) ?\s))
      text)))

(cl-defun appkit-chat-avatar-prefixes
    (image fallback &key pixel-size resize slice-height)
  "Return telega-style prefixes for a two-line chat avatar.

The result is a plist containing `:header', `:first-body', and `:rest-body'.
IMAGE is split between the heading and first body line; later lines receive a
same-width blank prefix.  FALLBACK occupies the heading while IMAGE is absent.
When RESIZE is non-nil, resize IMAGE to square PIXEL-SIZE before slicing."
  (let* ((fallback-text (if (and (stringp fallback)
                                 (not (string-empty-p fallback)))
                            fallback
                          "@"))
         (prepared (appkit-chat-avatar--prepare-image
                    image fallback-text
                    :pixel-size pixel-size
                    :resize resize
                    :slice-height slice-height)))
    (if prepared
        (let* ((width (appkit-chat-avatar-image-char-width prepared))
               (pixel-width
                (plist-get (cdr prepared)
                           :appkit-chat-avatar-pixel-width))
               (header (concat
                        (appkit-chat-avatar--slice-string prepared 0) " "))
               (first-body (concat
                            (appkit-chat-avatar--slice-string prepared 1) " "))
               (normalized-width (max (1+ width)
                                      (string-width header)
                                      (string-width first-body)))
               (rest-body
                (concat
                 (appkit-chat-avatar--pixel-spacer-string width pixel-width)
                 " ")))
          (list :header (appkit-chat-avatar--pad-prefix
                         header normalized-width)
                :first-body (appkit-chat-avatar--pad-prefix
                             first-body normalized-width)
                :rest-body rest-body))
      (let* ((expected-image-width
              (and (numberp pixel-size)
                   (> pixel-size 0)
                   (ceiling (/ (float pixel-size)
                               (float (appkit-chat-avatar-column-pixel-width))))))
             (image-width (max 1
                               (string-width fallback-text)
                               (or expected-image-width 0)))
             (width (1+ image-width))
             (rest-body (make-string width ?\s)))
        (list :header (appkit-chat-avatar--pad-prefix
                       (concat (appkit-chat-avatar--fit-text
                                fallback-text image-width)
                               " ")
                       width)
              :first-body rest-body
              :rest-body rest-body)))))

(provide 'appkit-chat-avatar)
;;; appkit-chat-avatar.el ends here
