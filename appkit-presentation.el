;;; appkit-presentation.el --- Shared list and row presentation geometry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Protocol-independent list, row, and elision helpers used by generated
;; presentation surfaces.  Position preservation and keyed EWOC reconciliation
;; remain owned by `appkit-position' and `appkit-ewoc', respectively.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-position)
(require 'appkit-ui)
(require 'appkit-geometry)

(cl-defstruct (appkit-presentation-list-spec
               (:constructor appkit-presentation-list-spec-create))
  title
  summary
  loading-note
  items
  item-inserter
  empty-text
  footer-lines)

(defun appkit-presentation-render-list-spec (spec)
  "Render list SPEC in current buffer using `appkit-ui-render-list-view'."
  (appkit-ui-render-list-view
   :title (appkit-presentation-list-spec-title spec)
   :summary (appkit-presentation-list-spec-summary spec)
   :loading-note (appkit-presentation-list-spec-loading-note spec)
   :items (appkit-presentation-list-spec-items spec)
   :item-inserter (appkit-presentation-list-spec-item-inserter spec)
   :empty-text (appkit-presentation-list-spec-empty-text spec)
   :footer-lines (appkit-presentation-list-spec-footer-lines spec)))

(cl-defun appkit-presentation-render-list-spec-preserving-position
    (spec &key anchor-property preserve-window-start after-restore)
  "Render list SPEC and restore cursor/viewport context.

ANCHOR-PROPERTY, PRESERVE-WINDOW-START, and AFTER-RESTORE are forwarded to
`appkit-position-render-preserving'."
  (appkit-position-render-preserving
   (lambda ()
     (let ((inhibit-read-only t))
       (erase-buffer)
       (appkit-presentation-render-list-spec spec)
       (goto-char (point-min))))
   :anchor-property anchor-property
   :preserve-window-start preserve-window-start
   :after-restore after-restore))

(cl-defstruct (appkit-presentation-label-row
               (:constructor appkit-presentation-label-row-create))
  label
  prefix
  suffix
  icon-inserter
  icon-separator
  face
  line-properties
  help-echo
  mouse-face)

(defun appkit-presentation-insert-label-row (row)
  "Insert one simple label ROW."
  (let ((start (point)))
    (when-let* ((prefix (appkit-presentation-label-row-prefix row)))
      (insert prefix))
    (when-let* ((icon-inserter (appkit-presentation-label-row-icon-inserter row)))
      (funcall icon-inserter)
      (when-let* ((icon-separator (appkit-presentation-label-row-icon-separator row)))
        (insert icon-separator)))
    (insert (or (appkit-presentation-label-row-label row) ""))
    (when-let* ((suffix (appkit-presentation-label-row-suffix row)))
      (insert suffix))
    (insert "\n")
    (add-text-properties
     start
     (point)
     (append (or (appkit-presentation-label-row-line-properties row) '())
             (when-let* ((face (appkit-presentation-label-row-face row)))
               (list 'face face))
             (when-let* ((help-echo (appkit-presentation-label-row-help-echo row)))
               (list 'help-echo help-echo))
             (when-let* ((mouse-face (appkit-presentation-label-row-mouse-face row)))
               (list 'mouse-face mouse-face))))))

(cl-defun appkit-presentation-insert-label-line
    (label &key prefix suffix icon-inserter icon-separator
           face line-properties help-echo mouse-face)
  "Insert LABEL as one styled line.

PREFIX, SUFFIX, ICON-INSERTER, ICON-SEPARATOR, FACE, LINE-PROPERTIES,
HELP-ECHO, and MOUSE-FACE customize its presentation and interaction."
  (appkit-presentation-insert-label-row
   (appkit-presentation-label-row-create
    :label label
    :prefix prefix
    :suffix suffix
    :icon-inserter icon-inserter
    :icon-separator icon-separator
    :face face
    :line-properties line-properties
    :help-echo help-echo
    :mouse-face mouse-face)))

(cl-defun appkit-presentation-insert-heading-line
    (text &key face line-properties help-echo mouse-face)
  "Insert heading TEXT using FACE, LINE-PROPERTIES, HELP-ECHO, and MOUSE-FACE."
  (appkit-presentation-insert-label-line
   text
   :face face
   :line-properties line-properties
   :help-echo help-echo
   :mouse-face mouse-face))

(cl-defun appkit-presentation-insert-note-line
    (text &key face line-properties help-echo mouse-face)
  "Insert note TEXT using FACE, LINE-PROPERTIES, HELP-ECHO, and MOUSE-FACE."
  (appkit-presentation-insert-heading-line
   text
   :face (or face 'shadow)
   :line-properties line-properties
   :help-echo help-echo
   :mouse-face mouse-face))

(cl-defstruct (appkit-presentation-one-line-row
               (:constructor appkit-presentation-one-line-row-create))
  icon-inserter
  context
  context-open
  context-close
  context-trail
  context-trail-face
  preview
  time
  time-face
  time-tail-face
  line-properties
  help-echo
  mouse-face)

(defun appkit-presentation-canonicalize-number (spec base)
  "Resolve SPEC against BASE columns.

SPEC can be an integer, float ratio, or list (VALUE MIN MAX)."
  (let* ((raw (if (consp spec) (car spec) spec))
         (min-value (when (consp spec) (nth 1 spec)))
         (max-value (when (consp spec) (nth 2 spec)))
         (value (cond
                 ((integerp raw) raw)
                 ((floatp raw) (round (* raw base)))
                 ((numberp raw) (round raw))
                 (t base))))
    (when (numberp min-value)
      (setq value (max value min-value)))
    (when (numberp max-value)
      (setq value (min value max-value)))
    value))

(defun appkit-presentation-truncate-fill (text width &optional right-align)
  "Return TEXT truncated and padded to WIDTH.

When RIGHT-ALIGN is non-nil, pad on the left instead of right."
  (let* ((target (max 0 (or width 0)))
         (trimmed (truncate-string-to-width (or text "") target nil nil ""))
         (padding (max 0 (- target (string-width trimmed)))))
    (if right-align
        (concat (make-string padding ?\s) trimmed)
      (concat trimmed (make-string padding ?\s)))))

(defun appkit-presentation-elide-string (str max &optional face)
  "Return STR elided to MAX columns using display properties and FACE."
  (let* ((text (or str ""))
         (str-width (string-width text))
         (limit (max 0 (or max 0))))
    (if (<= str-width limit)
        text
      (let* ((elide-str "…")
             (elide-width (string-width elide-str))
             (elide-pos 1)
             (str-len (length text))
             (elide-trail (floor (* limit (- 1 elide-pos))))
             (trail-width
              (progn
                (while (and (> elide-trail 0)
                            (> (string-width text (- str-len elide-trail))
                               (floor (* limit (- 1 elide-pos)))))
                  (setq elide-trail (1- elide-trail)))
                (string-width text (- str-len elide-trail))))
             (elide-lead (- (min limit str-len) elide-width trail-width))
             (result (copy-sequence text)))
        (when (< elide-lead 0)
          (setq elide-lead 0))
        (while (and (> elide-lead 0)
                    (> (+ (string-width result 0 elide-lead)
                          elide-width trail-width)
                       limit))
          (setq elide-lead (1- elide-lead)))
        (add-text-properties
         elide-lead
         (- str-len elide-trail)
         (list 'display elide-str
               'rear-nonsticky '(display)
               'face face)
         result)
        result))))

(defun appkit-presentation--string-pixel-width (text &optional face)
  "Return graphical pixel width of TEXT rendered with FACE.

Return nil when the current buffer has no graphical display window."
  (when-let* ((window (get-buffer-window (current-buffer) t))
              ((window-live-p window))
              (frame (window-frame window))
              ((display-graphic-p frame))
              ((fboundp 'string-pixel-width)))
    (let ((measured (copy-sequence (or text ""))))
      (when (and face (> (length measured) 0))
        (add-face-text-property 0 (length measured) face t measured))
      (string-pixel-width measured (current-buffer)))))

(defun appkit-presentation--pixel-continuation-char-p (character)
  "Return non-nil when CHARACTER continues the preceding display cluster."
  (and character
       (or (memq (get-char-code-property character 'general-category)
                 '(Mn Mc Me))
           (<= #xFE00 character #xFE0F)
           (<= #xE0100 character #xE01EF)
           (<= #x1F3FB character #x1F3FF)
           (= character #x20E3))))

(defun appkit-presentation--regional-indicator-p (character)
  "Return non-nil when CHARACTER is a regional-indicator symbol."
  (and character (<= #x1F1E6 character #x1F1FF)))

(defun appkit-presentation--safe-elide-boundary (text boundary)
  "Move BOUNDARY left to a safe display-cluster edge in TEXT."
  (let ((position (max 0 (min (length text) boundary)))
        changed)
    (while (and (> position 0) (< position (length text))
                (progn
                  (setq changed nil)
                  (cond
                   ((appkit-presentation--pixel-continuation-char-p
                     (aref text position))
                    (setq position (1- position)
                          changed t))
                   ((= (aref text (1- position)) #x200D)
                    (setq position (1- position)
                          changed t))
                   ((and (appkit-presentation--regional-indicator-p
                          (aref text (1- position)))
                         (appkit-presentation--regional-indicator-p
                          (aref text position)))
                    (setq position (1- position)
                          changed t)))
                  changed)))
    position))

(defun appkit-presentation--elide-string-to-pixels (text pixel-limit face)
  "Return TEXT right-elided within PIXEL-LIMIT using FACE metrics."
  (let* ((ellipsis "…")
         (text-length (length text))
         (low 0)
         (high (max 0 (1- text-length)))
         (best 0))
    (while (<= low high)
      (let* ((middle (/ (+ low high) 2))
             (candidate (concat (substring text 0 middle) ellipsis))
             (width (appkit-presentation--string-pixel-width candidate face)))
        (if (and (numberp width) (<= width pixel-limit))
            (setq best middle
                  low (1+ middle))
          (setq high (1- middle)))))
    (setq best (appkit-presentation--safe-elide-boundary text best))
    (let ((result (copy-sequence text)))
      (add-text-properties
       best text-length
       (list 'display ellipsis
             'rear-nonsticky '(display)
             'face face)
       result)
      result)))

(defun appkit-presentation-elide-string-for-columns (str max &optional face)
  "Return STR visually elided to MAX display columns.

Graphical buffers use actual font pixels so emoji and variable-width glyphs
cannot push following aligned columns to the right.  Terminals use ordinary
column widths.  FACE supplies the font metrics used for measurement."
  (let* ((text (or str ""))
         (limit (max 0 (or max 0)))
         (pixel-width (appkit-presentation--string-pixel-width text face)))
    (if (numberp pixel-width)
        (let ((pixel-limit (appkit-geometry-columns-pixel-width limit)))
          (if (<= pixel-width pixel-limit)
              text
            (appkit-presentation--elide-string-to-pixels text pixel-limit face)))
      (appkit-presentation-elide-string text limit face))))

(defun appkit-presentation-one-line-column-widths
    (content-width context-width-spec &optional delimiter-width)
  "Split CONTENT-WIDTH using CONTEXT-WIDTH-SPEC for the context column.

DELIMITER-WIDTH is the combined display width of the opening and closing
context delimiters, defaulting to two columns."
  (let* ((delimiter-width (max 0 (or delimiter-width 2)))
         (fixed-width (1+ delimiter-width))
         (max-context-inner (max 8 (- content-width fixed-width)))
         (context-inner-width
          (max 8
               (min max-context-inner
                    (appkit-presentation-canonicalize-number context-width-spec
                                                     content-width))))
         (preview-width
          (max 0 (- content-width context-inner-width fixed-width))))
    (list :context-inner-width context-inner-width
          :preview-width preview-width
          :separator-width (if (> preview-width 0) 1 0))))

(cl-defun appkit-presentation-insert-one-line-row
    (row &key indent width icon-slot-width context-width-spec time-slot-width)
  "Insert ROW using one-line activity-style layout.

ROW is an `appkit-presentation-one-line-row' object.  INDENT is left padding in spaces.
WIDTH sets the total row width.  ICON-SLOT-WIDTH reserves columns for the
icon slot; zero suppresses the slot entirely.  CONTEXT-WIDTH-SPEC controls
context width using
`appkit-presentation-canonicalize-number' semantics.  TIME-SLOT-WIDTH reserves a stable
right-aligned timestamp column.  PREVIEW is an
`appkit-ui-one-line-preview'.  CONTEXT-OPEN and CONTEXT-CLOSE default to
square brackets; Appkit measures their actual display widths.  A non-empty
context trail is kept inside those delimiters and aligned to their right edge;
its width is reserved before the context is elided."
  (let* ((padding (make-string (max 0 (or indent 0)) ?\s))
         (context-text
          (appkit-ui-one-line-text (appkit-presentation-one-line-row-context row)))
         (context-trail-text
          (appkit-ui-one-line-text
           (appkit-presentation-one-line-row-context-trail row)))
         (context-open
          (appkit-ui-one-line-text
           (or (appkit-presentation-one-line-row-context-open row) "[")))
         (context-close
          (appkit-ui-one-line-text
           (or (appkit-presentation-one-line-row-context-close row) "]")))
         (preview (appkit-presentation-one-line-row-preview row))
         (time-text
          (appkit-ui-one-line-text (appkit-presentation-one-line-row-time row)))
         (context-open-width (string-width context-open))
         (context-close-width (string-width context-close))
         (context-delimiter-width
          (+ context-open-width context-close-width))
         (time-width
          (max (max 0 (or time-slot-width 0))
               (if (string-empty-p time-text)
                   0
                 (max 6 (string-width time-text)))))
         (line-start (point)))
    (insert padding)
    (let* ((icon-start (appkit-geometry-current-column))
           (slot-width (max 0 (if (null icon-slot-width)
                                  2
                                icon-slot-width))))
      (when (> slot-width 0)
        (when-let* ((icon-inserter
                     (appkit-presentation-one-line-row-icon-inserter row)))
          (funcall icon-inserter))
        (appkit-geometry-insert-alignment-space
         (max icon-start (1- (+ icon-start slot-width))))
        (insert " ")))
    (let* ((content-start (appkit-geometry-current-column))
           (time-gap (if (> time-width 0) 1 0))
           (content-width (max 20 (- (max 20 (or width 20))
                                     content-start
                                     time-width
                                     time-gap)))
           (widths (appkit-presentation-one-line-column-widths
                    content-width
                    (or context-width-spec '(0.45 20))
                    context-delimiter-width))
           (context-inner-width (or (plist-get widths :context-inner-width) 8))
           (preview-width (or (plist-get widths :preview-width) 0))
           (separator-width (or (plist-get widths :separator-width) 0)))
      (let ((context-start (appkit-geometry-current-column)))
        (insert context-open)
        (if (string-empty-p context-trail-text)
            (progn
              (insert (appkit-presentation-elide-string-for-columns
                       context-text context-inner-width 'default))
              (appkit-geometry-insert-alignment-space
               (+ context-start context-open-width context-inner-width)))
          (let* ((raw-trail-width (string-width context-trail-text))
                 (trail-width (min context-inner-width raw-trail-width))
                 (trail-start-offset
                  (max 0 (- context-inner-width trail-width)))
                 (context-separator-width
                  (if (and (not (string-empty-p context-text))
                           (> trail-start-offset 0))
                      1
                    0))
                 (context-width
                  (max 0 (- trail-start-offset context-separator-width)))
                 (trail-text
                  (if (> raw-trail-width trail-width)
                      (appkit-presentation-elide-string-for-columns
                       context-trail-text trail-width
                       (appkit-presentation-one-line-row-context-trail-face row))
                    context-trail-text)))
            (when (> context-width 0)
              (insert (appkit-presentation-elide-string-for-columns
                       context-text context-width 'default)))
            (appkit-geometry-insert-alignment-space
             (+ context-start context-open-width trail-start-offset))
            (let ((trail-start (point)))
              (insert trail-text)
              (when-let* ((trail-face
                           (appkit-presentation-one-line-row-context-trail-face row)))
                (add-text-properties trail-start (point)
                                     (list 'face trail-face))))))
        (insert context-close))
      (when (> preview-width 0)
        (when (> separator-width 0)
          (insert " "))
        (insert
         (appkit-ui-render-one-line-preview
          preview preview-width
          :face 'shadow
          :elide-function #'appkit-presentation-elide-string-for-columns)))
      (when (> time-width 0)
        (let ((target-time-col (- (max 20 (or width 20)) time-width)))
          (appkit-geometry-insert-alignment-space target-time-col)
          (let* ((time-start (point))
                 (time-face (or (appkit-presentation-one-line-row-time-face row) 'shadow))
                 (tail-face (appkit-presentation-one-line-row-time-tail-face row)))
            (insert (appkit-presentation-truncate-fill time-text time-width t))
            (if (and tail-face (> (point) time-start))
                (let ((tail-start (max time-start (1- (point)))))
                  (when (< time-start tail-start)
                    (add-text-properties time-start tail-start (list 'face time-face)))
                  (add-text-properties tail-start (point) (list 'face tail-face)))
              (add-text-properties time-start (point) (list 'face time-face))))))
      (insert "\n")
      (add-text-properties
       line-start
       (point)
       (append (or (appkit-presentation-one-line-row-line-properties row) '())
               (when-let* ((help-echo (appkit-presentation-one-line-row-help-echo row)))
                 (list 'help-echo help-echo))
               (when-let* ((mouse-face (appkit-presentation-one-line-row-mouse-face row)))
                 (list 'mouse-face mouse-face)))))))

(provide 'appkit-presentation)

;;; appkit-presentation.el ends here
