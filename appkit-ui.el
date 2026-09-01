;;; appkit-ui.el --- Shared UI rendering primitives for appkit  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Small shared insertion helpers used by room/root renderers.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'color)
(require 'subr-x)
(require 'svg nil t)

(defun appkit-ui-progress-bar (progress &optional width filled empty)
  "Return a WIDTH-column bar for PROGRESS, a 0-1 float.

WIDTH defaults to 10.  FILLED is a character or (FILL . TIP) cons and
defaults to `(?= . ?>)'.  EMPTY is the unfilled character and defaults
to space.  Nil or non-positive PROGRESS yields an empty track."
  (let* ((width (max 1 (or width 10)))
         (fill-spec (or filled '(?= . ?>)))
         (fill (if (consp fill-spec) (car fill-spec) fill-spec))
         (tip (if (consp fill-spec) (cdr fill-spec) fill))
         (empty (or empty ?\s))
         (ratio (if (and (numberp progress) (> progress 0))
                    (min 1.0 (max 0.0 (float progress)))
                  0.0))
         (filled-cols (min width (round (* width ratio)))))
    (cond
     ((= filled-cols 0) (make-string width empty))
     ((= filled-cols 1)
      (concat (char-to-string tip)
              (make-string (1- width) empty)))
     (t (concat (make-string (1- filled-cols) fill)
                (char-to-string tip)
                (make-string (- width filled-cols) empty))))))

;;; ── Vertical bar (SVG in GUI, Unicode in terminal) ──────────────────

(defvar appkit-ui--vbar-image-cache (make-hash-table :test #'equal)
  "Cache of SVG vertical-bar images keyed by (COLOR XH CHAR-W).")

(defun appkit-ui--face-foreground-color (face)
  "Extract foreground colour string from FACE.
FACE may be a symbol, a plist (:foreground ...) or a list of faces."
  (cond
   ((and (listp face) (plist-get face :foreground))
    (plist-get face :foreground))
   ((symbolp face)
    (ignore-errors (face-foreground face nil t)))
   ((and (listp face) (symbolp (car face)))
    (ignore-errors (face-foreground (car face) nil t)))))

(defun appkit-ui--create-vbar-svg (face)
  "Create an SVG vertical-bar image coloured with FACE.
The image fills the full line height so consecutive lines join seamlessly.
Returns nil in terminal frames or when SVG is unavailable."
  (when (and (display-graphic-p)
             (image-type-available-p 'svg)
             (fboundp 'svg-create)
             (fboundp 'svg-rectangle))
    (let* ((xh (or (ignore-errors (default-line-height))
                   (frame-char-height) 16))
           (char-w (max 1 (frame-char-width)))
           (bar-w (max 3 (round (* char-w 0.18))))
           (color (or (appkit-ui--face-foreground-color face)
                      (face-foreground 'default nil t)
                      "gray"))
           (key (list color xh char-w)))
      (or (gethash key appkit-ui--vbar-image-cache)
          (let* ((svg (svg-create char-w xh))
                 (_ (svg-rectangle svg 0 0 bar-w xh
                                   :fill-opacity 1 :fill color))
                 (data (with-temp-buffer (svg-print svg) (buffer-string)))
                 (image (create-image data 'svg t
                                      :scale 1.0
                                      :width char-w :height xh
                                      :ascent 'center)))
            (when image
              (puthash key image appkit-ui--vbar-image-cache))
            image)))))

(defun appkit-ui-vbar-string (face)
  "Return a one-column vertical bar string coloured with FACE.

GUI frames get an SVG image; terminal frames get a plain ▏ character.  The
result is display-only presentation and may safely stand in for source text
that remains in the buffer."
  (let ((image (appkit-ui--create-vbar-svg face)))
    (if image
        (propertize " " 'display image 'rear-nonsticky '(display))
      (if face (propertize "▏" 'face face) "▏"))))

(defun appkit-ui--rgb-triple-p (value)
  "Return non-nil when VALUE is an RGB triple of numeric channels."
  (and (listp value)
       (= (length value) 3)
       (numberp (nth 0 value))
       (numberp (nth 1 value))
       (numberp (nth 2 value))))

(defun appkit-ui--default-background-rgb ()
  "Return current default background as an RGB triple, or nil."
  (let ((background (face-background 'default nil t)))
    (when (and (stringp background)
               (not (member background '("unspecified" "unspecified-bg"))))
      (ignore-errors (color-name-to-rgb background)))))

(cl-defun appkit-ui-tinted-background-face (accent-face &key (alpha 0.10))
  "Return a subtle background face derived from ACCENT-FACE.

ALPHA is the fraction of ACCENT-FACE's foreground blended over the current
default background and must be between zero and one.  The returned face uses
`:extend t' so block-like rows paint to the visual line edge.  Return nil when
either colour cannot be resolved."
  (unless (and (numberp alpha) (<= 0 alpha) (<= alpha 1))
    (error "Appkit tinted background alpha must be between zero and one"))
  (let* ((accent-name (appkit-ui--face-foreground-color accent-face))
         (accent (and (stringp accent-name)
                      (ignore-errors (color-name-to-rgb accent-name))))
         (background (appkit-ui--default-background-rgb)))
    (when (and (appkit-ui--rgb-triple-p accent)
               (appkit-ui--rgb-triple-p background))
      (let ((blended
             (cl-mapcar
              (lambda (foreground base)
                (+ (* alpha foreground) (* (- 1 alpha) base)))
              accent background)))
        `(:background ,(apply #'color-rgb-to-hex (append blended '(2)))
          :extend t)))))

(defun appkit-ui-buffer-substring-filter (beg end delete)
  "Copy BEG..END while removing display-only Appkit presentation.

Underlying source characters remain intact.  Thus a source marker such as `>'
may be visually replaced by a vertical bar while copied and yanked text still
contains the original marker.  When DELETE is non-nil, delete the source region
after taking the copy."
  (let ((text (buffer-substring beg end)))
    (when delete
      (save-excursion
        (goto-char beg)
        (delete-region beg end)))
    (remove-list-of-text-properties
     0 (length text)
     '(display line-prefix wrap-prefix appkit-ui-source-line-marker
       rear-nonsticky)
     text)
    text))

;;; ── Buttons & styled lines ──────────────────────────────────────────

(defconst appkit-ui-action-property 'appkit-ui-action
  "Text property holding the zero-argument function for an action span.")

(defvar appkit-ui-action-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'appkit-ui-activate)
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [mouse-1] #'appkit-ui-activate)
    map)
  "Keymap used by inline Appkit action spans.")

(defun appkit-ui--event-position (&optional event)
  "Return the buffer position for EVENT, or point when EVENT is not a click."
  (or (and event
           (eventp event)
           (posn-point (event-start event)))
      (point)))

(defun appkit-ui-action-at (&optional position)
  "Return the Appkit action at POSITION or point.

POSITION may sit on the action or immediately after it."
  (let ((pos (or position (point))))
    (when (and (integer-or-marker-p pos)
               (<= (point-min) pos)
               (<= pos (point-max)))
      (or (and (< pos (point-max))
               (get-text-property pos appkit-ui-action-property))
          (and (> pos (point-min))
               (get-text-property (1- pos) appkit-ui-action-property))))))

(defun appkit-ui-activate-at (&optional position)
  "Run the Appkit action at POSITION or point.

Return non-nil when an action ran."
  (when-let* ((action (appkit-ui-action-at position)))
    (funcall action)
    t))

(defun appkit-ui-activate (&optional event)
  "Activate the Appkit action at point or EVENT.

When EVENT is a click, move point to that position first so the action
sees the same entry the user clicked."
  (interactive (list last-nonmenu-event))
  (let ((position (appkit-ui--event-position event)))
    (when (and event
               (integer-or-marker-p position)
               (/= position (point)))
      (goto-char position))
    (unless (appkit-ui-activate-at position)
      (user-error "No action at point"))))

(cl-defun appkit-ui-add-action (start end action &key help-echo face mouse-face)
  "Make START..END an Appkit action span that calls ACTION.

ACTION is a zero-argument function.  HELP-ECHO describes that action.
FACE is appended when non-nil.  MOUSE-FACE defaults to `highlight'.
Do nothing when ACTION is not callable or the region is empty."
  (when (and (functionp action)
             (integer-or-marker-p start)
             (integer-or-marker-p end)
             (< start end))
    (add-text-properties
     start end
     (list appkit-ui-action-property action
           'keymap appkit-ui-action-map
           'mouse-face (or mouse-face 'highlight)
           'pointer 'hand
           'help-echo (or help-echo "Activate")))
    (when face
      (add-face-text-property start end face 'append))
    action))

(cl-defun appkit-ui-insert-action-button (label action
                                                &key face help-echo properties)
  "Insert clickable button LABEL calling ACTION.

ACTION is a no-arg function.  FACE, HELP-ECHO and PROPERTIES customize button
text properties passed to `insert-text-button'."
  (let ((button-props
         (append
          (list 'follow-link t
                'action (lambda (_button)
                          (funcall action)))
          (when help-echo
            (list 'help-echo help-echo))
          (when face
            (list 'face face))
          properties)))
    (apply #'insert-text-button label button-props)))

(defvar appkit-ui-action-row-map
  (let ((map (make-sparse-keymap)))
    ;; Keep primary-click activation direct.  In particular, do not inherit
    ;; `button-map' or bind `follow-link': either would let Emacs translate a
    ;; mouse-1 event before the button at the event position is dispatched.
    (define-key map (kbd "RET") #'push-button)
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [mouse-1] #'push-button)
    ;; Retain the traditional button activation gesture explicitly, without
    ;; inheriting any of the default button keymap's link behavior.
    (define-key map [mouse-2] #'push-button)
    map)
  "Keymap used by whole-row Appkit action buttons.")

(defun appkit-ui--activate-action-row (button)
  "Activate action-row BUTTON using its exact stored object and action."
  (let ((object (button-get button 'appkit-ui-action-row-object))
        (action (button-get button 'appkit-ui-action-row-action)))
    (funcall action object)))

(define-button-type 'appkit-ui-action-row-button
  'face nil
  'mouse-face nil
  'help-echo nil
  'follow-link nil
  'keymap appkit-ui-action-row-map
  'action #'appkit-ui--activate-action-row)

(cl-defun appkit-ui-make-action-row
    (start end object action &key help-echo mouse-face)
  "Make START..END an action row invoking ACTION with OBJECT.

ACTION must be callable.  HELP-ECHO supplies hover help, while MOUSE-FACE
controls hover highlighting explicitly and defaults to nil.  When END follows
a terminating newline, that newline is excluded from the button.  Action rows
must otherwise be single-line.  Return the text button, or nil when ACTION is
invalid, the resulting span is empty, or it contains an internal newline."
  (let ((row-start (and (integer-or-marker-p start)
                        (if (markerp start) (marker-position start) start)))
        (row-end (and (integer-or-marker-p end)
                      (if (markerp end) (marker-position end) end))))
    (when (and (functionp action)
               (integerp row-start)
               (integerp row-end)
               (<= (point-min) row-start)
               (<= row-start row-end)
               (<= row-end (point-max)))
      (when (and (< row-start row-end)
                 (eq (char-before row-end) ?\n))
        (setq row-end (1- row-end)))
      (when (and (< row-start row-end)
                 (not (save-excursion
                        (goto-char row-start)
                        (search-forward "\n" row-end t))))
        (make-text-button
         row-start row-end
         :type 'appkit-ui-action-row-button
         'mouse-face mouse-face
         'help-echo help-echo
         'follow-link nil
         'keymap appkit-ui-action-row-map
         'action #'appkit-ui--activate-action-row
         'appkit-ui-action-row-object object
         'appkit-ui-action-row-action action)))))

;;; ── Prefix state machinery ─────────────────────────────────────────

(defconst appkit-ui--prefix-state-tag 'appkit-ui-prefix-state
  "Internal marker used to identify `line-prefix' state objects.")

(defun appkit-ui-prefix-state-p (value)
  "Return non-nil when VALUE is an `appkit-ui' `line-prefix' state object."
  (and (vectorp value)
       (= (length value) 4)
       (eq (aref value 0) appkit-ui--prefix-state-tag)))

(defun appkit-ui-make-prefix-state (first-prefix rest-prefix)
  "Return mutable `line-prefix' state with FIRST-PREFIX and REST-PREFIX."
  (vector appkit-ui--prefix-state-tag first-prefix rest-prefix nil))

(defun appkit-ui-prefix-state-current (state)
  "Return current prefix string for prefix STATE without consuming it."
  (when (appkit-ui-prefix-state-p state)
    (if (aref state 3)
        (aref state 2)
      (or (aref state 1) (aref state 2)))))

(defun appkit-ui-prefix-state-rest (state)
  "Return rest-prefix string from prefix STATE."
  (when (appkit-ui-prefix-state-p state)
    (aref state 2)))

(defun appkit-ui-prefix-state-consume (state)
  "Return current prefix from STATE and mark first-prefix as consumed."
  (when (appkit-ui-prefix-state-p state)
    (let ((prefix (appkit-ui-prefix-state-current state)))
      (aset state 3 t)
      prefix)))

(defun appkit-ui-prefix-string (prefix &optional consume default)
  "Return normalized prefix string from PREFIX source.

When PREFIX is state and CONSUME is non-nil, consume its first-prefix.
DEFAULT is used when PREFIX yields nil."
  (or (cond
       ((appkit-ui-prefix-state-p prefix)
        (if consume
            (appkit-ui-prefix-state-consume prefix)
          (appkit-ui-prefix-state-current prefix)))
       ((stringp prefix) prefix)
       (t nil))
      (or default "")))

;;; ── Card line-prefix helpers ───────────────────────────────────────

(defun appkit-ui-combine-faces (&rest faces)
  "Return one face value from FACES, dropping nil entries."
  (let ((values (delq nil faces)))
    (cond
     ((null values) nil)
     ((null (cdr values)) (car values))
     (t values))))

(defvar appkit-ui-card-indent-prefix "    "
  "Dynamic base indent prefix used by `appkit-ui-card-line-prefix'.")

(defvar appkit-ui-card-indent-prefix-state nil
  "Dynamic `line-prefix' state used by card renderers in insertion scope.")

(cl-defun appkit-ui-card-line-prefix (&key face (indent appkit-ui-card-indent-prefix))
  "Return a display-only card prefix string.

FACE colours the vertical bar marker; INDENT is kept plain.  In GUI frames
the marker is an SVG image that fills the full line height so consecutive
lines produce a seamless vertical bar.  The marker replaces INDENT's last
column so card content stays column-aligned with normal lines."
  (let* ((base (or indent ""))
         (mark (appkit-ui-vbar-string face))
         (base-len (length base)))
    (if (> base-len 0)
        (concat (substring base 0 (1- base-len)) mark)
      mark)))

(cl-defun appkit-ui-card-prefix-state (&key face indent)
  "Return card `line-prefix' state for current insertion scope.

FACE styles the vertical border and INDENT supplies the default indentation.
When `appkit-ui-card-indent-prefix-state' is bound to a prefix state, this
function consumes its first prefix for the card's first row and uses its rest
prefix for subsequent card rows."
  (let* ((line-state appkit-ui-card-indent-prefix-state)
         (default-indent (or indent appkit-ui-card-indent-prefix))
         (first-indent (appkit-ui-prefix-string line-state t default-indent))
         (rest-indent (appkit-ui-prefix-string line-state nil default-indent)))
    (appkit-ui-make-prefix-state
     (appkit-ui-card-line-prefix :face face :indent first-indent)
     (appkit-ui-card-line-prefix :face face :indent rest-indent))))

;;; ── Line prefix application ────────────────────────────────────────

(defun appkit-ui--apply-line-prefix-span (start end line-prefix-str &optional wrap-prefix-str)
  "Apply line/wrap prefix strings to START..END span.

LINE-PREFIX-STR is prepended to existing `line-prefix'.  WRAP-PREFIX-STR
defaults to LINE-PREFIX-STR and is prepended to existing `wrap-prefix'."
  (when (< start end)
    (let* ((line (or line-prefix-str ""))
           (wrap (or wrap-prefix-str line))
           (existing-line (get-text-property start 'line-prefix))
           (existing-wrap (get-text-property start 'wrap-prefix)))
      (add-text-properties
       start end
       (list 'line-prefix (concat line (if (stringp existing-line) existing-line ""))
             'wrap-prefix (concat wrap (if (stringp existing-wrap) existing-wrap "")))))))

(defun appkit-ui--append-wrap-prefix-span (start end wrap-prefix-str)
  "Append a soft-wrap prefix to START..END.

WRAP-PREFIX-STR is appended after any existing `wrap-prefix'.  The physical
line's `line-prefix' is deliberately left untouched."
  (when (< start end)
    (let ((wrap (or wrap-prefix-str ""))
          (existing-wrap (get-text-property start 'wrap-prefix)))
      (add-text-properties
       start end
       (list
        'wrap-prefix
        (concat (if (stringp existing-wrap) existing-wrap "") wrap))))))

(defun appkit-ui--source-character-display (fragment)
  "Return the display replacement represented by one-character FRAGMENT.

A presentation fragment may itself use a `display' property, as
`appkit-ui-vbar-string' does for a graphical SVG bar.  Materialize that
property directly on the source character: nested display strings are not
redisplayed recursively by Emacs.  Otherwise preserve FRAGMENT and its face
properties as the replacement string."
  (or (and (> (length fragment) 0)
           (get-text-property 0 'display fragment))
      fragment))

(defun appkit-ui--apply-source-character-displays
    (source-start source-end presentation properties)
  "Display PRESENTATION one-for-one over SOURCE-START..SOURCE-END.

The source characters remain in the buffer.  PRESENTATION must contain exactly
one character for each source character; each presentation character becomes
the `display' replacement of its corresponding source character.  A display
property carried by that presentation character is materialized directly
instead of being nested inside another display string.  PROPERTIES are added
to every source character."
  (let ((source-length (- source-end source-start)))
    (unless (= source-length (length presentation))
      (error "Appkit source presentation length must match source span"))
    (dotimes (offset source-length)
      (let ((position (+ source-start offset))
            ;; Keep one distinct display interval per source character.  This
            ;; preserves its visual column and prevents adjacent replacements
            ;; from becoming one cursor-hostile display run.
            (fragment (copy-sequence
                       (substring presentation offset (1+ offset)))))
        (add-text-properties
         position (1+ position)
         (append
          (list 'display (appkit-ui--source-character-display fragment)
                'appkit-ui-source-line-marker t
                'rear-nonsticky
                '(display appkit-ui-source-line-marker))
          properties))))))

(cl-defun appkit-ui-apply-source-line-prefix
    (line-start line-end source-start source-end prefix
                &key continuation-prefix properties)
  "Present a copyable source marker with PREFIX on one line.

LINE-START..LINE-END is the complete line span.  SOURCE-START..SOURCE-END is a
literal source marker that remains in the buffer and in copied text.  PREFIX
must have exactly one character for each source character; its characters are
displayed one-for-one over that source span, so normal cursor motion retains
real visual columns.  CONTINUATION-PREFIX defaults to PREFIX and is appended to
any existing `wrap-prefix' for soft-wrapped continuations.  The physical line's
existing `line-prefix' is preserved.  PROPERTIES are added to the source marker
span.  Return (LINE-START . LINE-END)."
  (unless (and (integer-or-marker-p line-start)
               (integer-or-marker-p line-end)
               (integer-or-marker-p source-start)
               (integer-or-marker-p source-end)
               (<= (point-min) line-start source-start source-end line-end)
               (<= line-end (point-max)))
    (error "Appkit source line prefix bounds are invalid"))
  (unless (stringp prefix)
    (signal 'wrong-type-argument (list 'stringp prefix)))
  (when (and continuation-prefix (not (stringp continuation-prefix)))
    (signal 'wrong-type-argument (list 'stringp continuation-prefix)))
  (unless (or (null properties) (listp properties))
    (signal 'wrong-type-argument (list 'listp properties)))
  (appkit-ui--apply-source-character-displays
   source-start source-end prefix properties)
  (when (< line-start line-end)
    (appkit-ui--append-wrap-prefix-span
     line-start line-end (or continuation-prefix prefix)))
  (cons line-start line-end))

(defun appkit-ui-apply-line-prefix (start end prefix)
  "Apply PREFIX as display prefix for region START..END.

PREFIX can be a string or a mutable prefix-state created by
`appkit-ui-make-prefix-state'."
  (when (< start end)
    (if (appkit-ui-prefix-state-p prefix)
        (let* ((pos start)
               (first-prefix (appkit-ui-prefix-state-consume prefix))
               (rest-prefix (or (appkit-ui-prefix-state-rest prefix)
                                first-prefix))
               (line-prefix first-prefix))
          (save-excursion
            (goto-char start)
            (while (< pos end)
              (goto-char pos)
              (let* ((line-end (line-end-position))
                     (next-pos (if (< line-end end)
                                   (1+ line-end)
                                 end)))
                ;; Telega-like behavior: wrapped continuations use rest-prefix,
                ;; so avatar/image prefix is not repeated on visual wraps.
                (appkit-ui--apply-line-prefix-span pos next-pos line-prefix rest-prefix)
                (setq line-prefix rest-prefix)
                (setq pos next-pos)))))
      (let ((pos start)
            (line-prefix (appkit-ui-prefix-string prefix nil "")))
        (save-excursion
          (while (< pos end)
            (goto-char pos)
            (let* ((line-end (line-end-position))
                   (next-pos (if (< line-end end) (1+ line-end) end)))
              (appkit-ui--apply-line-prefix-span
               pos next-pos line-prefix line-prefix)
              (setq pos next-pos))))))))

;;; ── High-level inserters ───────────────────────────────────────────

(cl-defun appkit-ui-insert-prefixed-lines (prefix text &key face properties)
  "Insert TEXT as newline-separated lines using display-only PREFIX.

PREFIX can be a prefix string or prefix state.  FACE and PROPERTIES are applied
per inserted line so copied text stays clean."
  (dolist (line (split-string (or text "") "\n" nil))
    (let ((start (point)))
      (insert line "\n")
      (when (or face properties)
        (add-text-properties
         start
         (point)
         (append properties
                 (when face
                   (list 'face face)))))
      (appkit-ui-apply-line-prefix start (point) prefix))))

(defun appkit-ui-append-face (start end face)
  "Append FACE to region START..END."
  (when (and face (< start end))
    (add-face-text-property start end face 'append)))

;;; ── Bounded one-line previews ──────────────────────────────────────

(cl-defstruct
    (appkit-ui-one-line-preview
     (:constructor appkit-ui-one-line-preview-create))
  "Protocol-neutral content projected into one bounded physical line.

TEXT is ordinary user-visible body content.  LABEL is an optional semantic
leading label styled by LABEL-FACE; SEPARATOR is unstyled client-owned chrome
between LABEL and content.  VISUAL is an optional atomic, propertized display
string whose underlying text is its terminal fallback.  VISUAL-COLUMNS is the
graphical display width reserved for VISUAL."
  text
  label
  separator
  visual
  visual-columns
  label-face)

(defun appkit-ui-one-line-text (text)
  "Return TEXT with physical line-breaking whitespace collapsed.

  Text properties on retained characters are preserved."
  (string-trim
   (replace-regexp-in-string "[\t\n\r ]+" " " (or text "") nil t)))

(defun appkit-ui--one-line-preview-default-elide (text width _face)
  "Return TEXT right-elided to WIDTH columns."
  (let ((limit (max 0 (or width 0))))
    (if (= limit 0)
        ""
      (truncate-string-to-width text limit nil nil "…"))))

(defun appkit-ui--one-line-preview-display-p (text)
  "Return non-nil when TEXT contains a non-nil `display' property."
  (and (> (length text) 0)
       (text-property-not-all 0 (length text) 'display nil text)))

(defun appkit-ui--one-line-preview-fallback (visual)
  "Return a copy of VISUAL with display projections removed."
  (let ((fallback (copy-sequence visual)))
    (remove-list-of-text-properties 0 (length fallback) '(display) fallback)
    fallback))

(defun appkit-ui--one-line-preview-place-visual
    (text visual head-length)
  "Place VISUAL after TEXT's HEAD-LENGTH characters.

TEXT and VISUAL are already styled and bounded by the caller."
  (let ((offset (min (max 0 head-length) (length text))))
    (cond
     ((string-empty-p text) visual)
     ((= offset 0) (concat visual " " text))
     (t
      (let ((head (substring text 0 offset))
            (remainder (string-trim-left (substring text offset))))
        (concat head " " visual
                (unless (string-empty-p remainder)
                  (concat " " remainder))))))))

(defun appkit-ui--one-line-preview-style
    (text base-face label-length label-face)
  "Return a styled copy of TEXT for one-line preview rendering."
  (let ((styled (copy-sequence text)))
    (when (and base-face (> (length styled) 0))
      (add-face-text-property 0 (length styled) base-face 'append styled))
    (when (and label-face
               (integerp label-length)
               (> label-length 0)
               (> (length styled) 0))
      (add-face-text-property
       0 (min label-length (length styled)) label-face nil styled))
    styled))

(cl-defun appkit-ui-render-one-line-preview
    (preview width &key face elide-function)
  "Render PREVIEW as a property-preserving fragment bounded to WIDTH columns.

PREVIEW must be an `appkit-ui-one-line-preview'.  FACE is appended to all
textual fragments; LABEL-FACE takes precedence only on LABEL.  ELIDE-FUNCTION,
when non-nil, receives (TEXT WIDTH FACE) and supplies container-specific text
measurement.  The returned string contains no newline and has no trailing
newline."
  (unless (appkit-ui-one-line-preview-p preview)
    (signal 'wrong-type-argument
            (list 'appkit-ui-one-line-preview-p preview)))
  (let* ((limit (max 0 (or width 0)))
         (elide (or elide-function
                    #'appkit-ui--one-line-preview-default-elide))
         (body
          (appkit-ui-one-line-text
           (appkit-ui-one-line-preview-text preview)))
         (label
          (appkit-ui-one-line-text
           (appkit-ui-one-line-preview-label preview)))
         (separator
          (or (appkit-ui-one-line-preview-separator preview) ""))
         (head (if (string-empty-p label)
                   ""
                 (concat label separator)))
         (head-length (length head))
         (text
          (appkit-ui--one-line-preview-style
           (cond
            ((and (not (string-empty-p head))
                  (not (string-empty-p body)))
             (concat head " " body))
            ((not (string-empty-p head)) head)
            (t body))
           face
           (length label)
           (appkit-ui-one-line-preview-label-face preview)))
         (visual
          (or (appkit-ui-one-line-preview-visual preview) ""))
         (display-p (appkit-ui--one-line-preview-display-p visual))
         (visual-columns
          (appkit-ui-one-line-preview-visual-columns preview)))
    (when (string-match-p "[\n\r]" separator)
      (error "One-line preview separator contains a line break"))
    (when (string-match-p "[\n\r]" visual)
      (error "One-line preview visual contains a line break"))
    (when (and display-p
               (not (and (integerp visual-columns)
                         (> visual-columns 0))))
      (error "Displayed one-line preview visual needs positive columns"))
    (let* ((graphical-visual-p
            (and display-p
                 (display-graphic-p)
                 (display-images-p)))
           (visual-width
            (if graphical-visual-p
                visual-columns
              (string-width visual)))
           (styled-visual
            (appkit-ui--one-line-preview-style visual face nil nil))
           (text-present-p (not (string-empty-p text))))
      (cond
       ((= limit 0) "")
       ((string-empty-p visual)
        (funcall elide text limit face))
       ((> visual-width limit)
        (if text-present-p
            (funcall elide text limit face)
          (funcall
           elide
           (appkit-ui--one-line-preview-style
            (appkit-ui--one-line-preview-fallback visual) face nil nil)
           limit face)))
       ((not text-present-p)
        styled-visual)
       (t
        (let ((text-width (- limit visual-width 1)))
          (if (<= text-width 0)
              styled-visual
            (appkit-ui--one-line-preview-place-visual
             (funcall elide text text-width face)
             styled-visual
             head-length))))))))

(cl-defun appkit-ui-render-list-view (&key title summary loading-note
                                           items item-inserter empty-text
                                           footer-lines)
  "Render a simple list view block in current buffer.

  TITLE, SUMMARY and LOADING-NOTE are optional header lines.
  ITEMS are rendered by ITEM-INSERTER when present; otherwise EMPTY-TEXT is
  inserted (defaults to `(empty)').  FOOTER-LINES is an optional list of lines
  printed after the list with an extra separating blank line."
  (when title
    (insert title "\n"))
  (when summary
    (insert summary "\n"))
  (when loading-note
    (insert loading-note "\n"))
  (insert "\n")
  (if (and items (functionp item-inserter))
      (dolist (item items)
        (funcall item-inserter item))
    (insert (or empty-text "(empty)") "\n"))
  (when footer-lines
    (insert "\n")
    (dolist (line footer-lines)
      (insert line "\n"))))

(provide 'appkit-ui)

;;; appkit-ui.el ends here
