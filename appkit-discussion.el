;;; appkit-discussion.el --- Threaded discussion presentation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Protocol-neutral rows for post details, comments, reviews, and similar
;; threaded discussions.  Applications own identities, bodies, and actions;
;; Appkit owns the shared avatar, nesting, chain connector, heading, footer,
;; and navigation geometry.

;;; Code:

(require 'cl-lib)
(require 'fringe)
(require 'seq)
(require 'subr-x)
(require 'appkit-chat-avatar)
(require 'appkit-chat-ins)
(require 'appkit-ui)

(cl-defstruct (appkit-discussion-entry
               (:constructor appkit-discussion-entry-create))
  key
  parent-key
  depth
  avatar
  avatar-fallback
  avatar-action
  avatar-help-echo
  context
  context-inserter
  context-face
  heading
  heading-inserter
  heading-face
  heading-line-face
  time
  time-face
  body-inserter
  footer
  footer-face
  connector
  properties)

(defconst appkit-discussion-key-property 'appkit-discussion-key
  "Text property carrying an opaque discussion entry key.")

(defconst appkit-discussion-parent-key-property 'appkit-discussion-parent-key
  "Text property carrying an opaque parent discussion entry key.")

(defconst appkit-discussion-depth-property 'appkit-discussion-depth
  "Text property carrying a discussion entry's visual nesting depth.")

(defface appkit-discussion-connector
  '((t :inherit shadow))
  "Face used for a linear discussion chain connector."
  :group 'appkit)

(defface appkit-discussion-fringe-connector
  '((t :inherit line-number-current-line))
  "Face used for discussion connectors drawn in a graphical fringe.

The face foreground draws set bitmap pixels; its background draws unset
pixels.  Keep the background aligned with the fringe for a narrow bar, or use
an accent background for a filled, diff-hl-like strip."
  :group 'appkit)

(defcustom appkit-discussion-connector-style 'fringe
  "How to draw linear discussion chain connectors.

`fringe' draws a configurable bar in the left fringe on graphical frames and
falls back to `text' when the current window has no usable fringe.  Its face,
generated geometry, and optional custom bitmap are controlled by
`appkit-discussion-fringe-connector',
`appkit-discussion-fringe-bar-width',
`appkit-discussion-fringe-bar-position', and
`appkit-discussion-fringe-bitmap'.  `text' draws \"│ \" in the line prefix and
reserves two text columns.  `none' hides the connector and reserves no columns.

Changes take effect the next time an entry is rendered."
  :type '(choice
          (const :tag "Left fringe (text fallback)" fringe)
          (const :tag "Text prefix" text)
          (const :tag "Hidden" none))
  :group 'appkit)

(defcustom appkit-discussion-fringe-bar-width 2
  "Width in pixels of Appkit's generated fringe connector bar.

The actual width is capped at the current fringe width and Emacs's 16-pixel
fringe bitmap limit.  This option is ignored when
`appkit-discussion-fringe-bitmap' is non-nil."
  :type '(integer :tag "Pixels")
  :group 'appkit)

(defcustom appkit-discussion-fringe-bar-position 'outer
  "Horizontal position of Appkit's generated fringe connector bar.

`outer' is nearest the window edge or divider, `inner' is nearest the buffer
text, and `center' centers the bar.  This option is ignored when
`appkit-discussion-fringe-bitmap' is non-nil."
  :type '(choice
          (const :tag "Outer edge" outer)
          (const :tag "Centered" center)
          (const :tag "Inner edge" inner))
  :group 'appkit)

(defcustom appkit-discussion-fringe-bitmap nil
  "Optional fringe bitmap symbol for Appkit discussion connectors.

When nil, Appkit generates a solid periodic bar from
`appkit-discussion-fringe-bar-width' and
`appkit-discussion-fringe-bar-position'.  Set this to a bitmap registered by
`define-fringe-bitmap' for complete control over connector shape and pattern."
  :type '(choice
          (const :tag "Generated solid bar" nil)
          (symbol :tag "Custom bitmap"))
  :group 'appkit)

(defvar appkit-discussion--fringe-spec-cache (make-hash-table :test #'eq)
  "Cached fringe connector strings keyed by bitmap or generated geometry.")

(defun appkit-discussion--validate-entry (entry)
  "Require a complete, internally consistent discussion ENTRY."
  (unless (appkit-discussion-entry-p entry)
    (error "Appkit discussion entry is not an entry object"))
  (unless (appkit-discussion-entry-key entry)
    (error "Appkit discussion entry has no stable key"))
  (let ((depth (or (appkit-discussion-entry-depth entry) 0)))
    (unless (and (integerp depth) (>= depth 0))
      (error "Appkit discussion depth must be a non-negative integer"))
    (when (and (> depth 0)
               (null (appkit-discussion-entry-parent-key entry)))
      (error "Nested Appkit discussion entry has no parent key")))
  (let ((connector (appkit-discussion-entry-connector entry)))
    (unless (memq connector '(nil continue end))
      (error "Appkit discussion connector must be nil, continue, or end")))
  (when (and (appkit-discussion-entry-context entry)
             (appkit-discussion-entry-context-inserter entry))
    (error "Appkit discussion entry has two context sources"))
  (when (and (appkit-discussion-entry-heading entry)
             (appkit-discussion-entry-heading-inserter entry))
    (error "Appkit discussion entry has two heading sources"))
  (dolist (slot '(context-inserter heading-inserter body-inserter avatar-action))
    (when-let* ((value (pcase slot
                         ('context-inserter
                          (appkit-discussion-entry-context-inserter entry))
                         ('heading-inserter
                          (appkit-discussion-entry-heading-inserter entry))
                         ('body-inserter
                          (appkit-discussion-entry-body-inserter entry))
                         ('avatar-action
                          (appkit-discussion-entry-avatar-action entry)))))
      (unless (functionp value)
        (error "Appkit discussion %s is not callable" slot))))
  entry)

(defun appkit-discussion--entry-properties (entry)
  "Return standard and application-owned text properties for ENTRY."
  (let ((cursor (appkit-discussion-entry-properties entry))
        (reserved (list appkit-discussion-key-property
                        appkit-discussion-parent-key-property
                        appkit-discussion-depth-property
                        'rear-nonsticky))
        custom
        custom-rear)
    (while cursor
      (unless (cdr cursor)
        (error "Appkit discussion entry properties are not a valid plist"))
      (let ((property (pop cursor))
            (value (pop cursor)))
        (if (eq property 'rear-nonsticky)
            (setq custom-rear value)
          (unless (memq property reserved)
            (setq custom (append custom (list property value)))))))
    (unless (or (null custom-rear)
                (eq custom-rear t)
                (listp custom-rear))
      (error "Appkit discussion rear-nonsticky must be t or a property-name list"))
    (append
     (list appkit-discussion-key-property
           (appkit-discussion-entry-key entry)
           appkit-discussion-parent-key-property
           (appkit-discussion-entry-parent-key entry)
           appkit-discussion-depth-property
           (or (appkit-discussion-entry-depth entry) 0)
           'rear-nonsticky
           (if (eq custom-rear t)
               t
             (delete-dups
              (append
               (list appkit-discussion-key-property
                     appkit-discussion-parent-key-property
                     appkit-discussion-depth-property)
               (and (listp custom-rear) custom-rear)))))
     custom)))

(defun appkit-discussion--avatar-properties (entry)
  "Return interaction properties for ENTRY's avatar prefixes."
  (when-let* ((action (appkit-discussion-entry-avatar-action entry)))
    (let ((map (make-sparse-keymap))
          (command
           (lambda (&optional _event)
             (interactive)
             (funcall action))))
      (define-key map (kbd "RET") command)
      (define-key map [mouse-1] command)
      (list 'keymap map
            'mouse-face 'highlight
            'help-echo
            (or (appkit-discussion-entry-avatar-help-echo entry)
                "Open avatar")))))

(defun appkit-discussion--decorate-prefix (prefix properties)
  "Return a copy of PREFIX carrying PROPERTIES."
  (let ((copy (copy-sequence (or prefix ""))))
    (when (and properties (> (length copy) 0))
      (add-text-properties 0 (length copy) properties copy))
    copy))

(defun appkit-discussion--insert-context (entry prefix properties)
  "Insert ENTRY's optional pre-heading context using PREFIX and PROPERTIES."
  (when (or (appkit-discussion-entry-context entry)
            (appkit-discussion-entry-context-inserter entry))
    (let ((start (point)))
      (if-let* ((inserter (appkit-discussion-entry-context-inserter entry)))
          (funcall inserter)
        (insert (or (appkit-discussion-entry-context entry) "")))
      (when (< start (point))
        (when-let* ((face (appkit-discussion-entry-context-face entry)))
          (add-face-text-property start (point) face 'append))
        (insert "\n")
        (appkit-ui-apply-line-prefix start (point) prefix)
        (add-text-properties start (point) properties)))))

(defun appkit-discussion--fringe-marker (bitmap cache-key)
  "Return a cached connector marker for BITMAP under CACHE-KEY."
  (or (gethash cache-key appkit-discussion--fringe-spec-cache)
      (let ((marker
             (propertize
              " "
              'display
              `((left-fringe ,bitmap appkit-discussion-fringe-connector))
              'appkit-discussion-fringe-marker t)))
        (puthash cache-key marker appkit-discussion--fringe-spec-cache)
        marker)))

(defun appkit-discussion--fringe-connector (width)
  "Return a periodic fringe connector for a WIDTH-pixel left fringe."
  (if appkit-discussion-fringe-bitmap
      (let ((bitmap appkit-discussion-fringe-bitmap))
        (unless (and (symbolp bitmap) (fringe-bitmap-p bitmap))
          (error "Invalid Appkit discussion fringe bitmap: %S" bitmap))
        (appkit-discussion--fringe-marker bitmap bitmap))
    (let ((configured-width appkit-discussion-fringe-bar-width)
          (position appkit-discussion-fringe-bar-position))
      (unless (and (integerp configured-width) (> configured-width 0))
        (error "Appkit discussion fringe bar width must be a positive integer"))
      (unless (memq position '(outer center inner))
        (error "Invalid Appkit discussion fringe bar position: %S" position))
      (let* ((bitmap-width (max 1 (min 16 width)))
             (bar-width (min bitmap-width configured-width))
             (position-index
              (pcase position
                ('outer 0)
                ('center 1)
                ('inner 2)))
             (bit-offset
              (pcase position
                ('outer (- bitmap-width bar-width))
                ('center (/ (- bitmap-width bar-width) 2))
                ('inner 0)))
             (bits (ash (1- (ash 1 bar-width)) bit-offset))
             (cache-key
              (logior bitmap-width
                      (ash bar-width 5)
                      (ash position-index 10))))
        (or (gethash cache-key appkit-discussion--fringe-spec-cache)
            (let ((bitmap
                   (intern
                    (format "appkit-discussion--connector-%d-%d-%s"
                            bitmap-width bar-width position))))
              (define-fringe-bitmap
                bitmap (vector bits)
                1 bitmap-width '(top t))
              (appkit-discussion--fringe-marker bitmap cache-key)))))))

(defun appkit-discussion--connector-presentation ()
  "Return the effective connector presentation for the current window.

The result is `text', `none', or (`fringe' . MARKER), where MARKER is a
display-only fringe string for the current window."
  (let ((style appkit-discussion-connector-style))
    (cond
     ((eq style 'fringe)
      (let* ((window (or (get-buffer-window (current-buffer) t)
                         (selected-window)))
             (fringes (and (window-live-p window)
                           (window-fringes window)))
             (window-width (car-safe fringes))
             (frame (and (window-live-p window) (window-frame window)))
             (frame-width (and frame
                               (frame-parameter frame 'left-fringe)))
             (width
              (cond
               ((integerp window-width) window-width)
               ((integerp frame-width) frame-width)
               (t 0))))
        (if (and frame (display-graphic-p frame) (> width 0))
            (cons 'fringe (appkit-discussion--fringe-connector width))
          'text)))
     ((memq style '(text none)) style)
     (t
      (error "Unknown Appkit discussion connector style: %S" style)))))

(defun appkit-discussion--connector-column
    (connector line presentation)
  "Return the prefix column for CONNECTOR on LINE using PRESENTATION.

CONNECTOR is nil, `continue', or `end'.  LINE is `context', `header',
`first-body', `rest-body', or `separator'.  PRESENTATION is the value from
`appkit-discussion--connector-presentation'."
  (let ((active-p
         (or (eq connector 'continue)
             (and (eq connector 'end)
                  (memq line '(context header first-body))))))
    (cond
     ((eq presentation 'text)
      (cond
       (active-p
        (propertize "│ " 'face 'appkit-discussion-connector))
       (connector "  ")
       (t "")))
     ((eq presentation 'none) "")
     ((and (consp presentation) (eq (car presentation) 'fringe))
      (if active-p
          (cdr presentation)
        ""))
     (t
      (error "Invalid Appkit discussion connector presentation: %S"
             presentation)))))

(defun appkit-discussion--prefix-width (prefix)
  "Return PREFIX's width in text-area columns.

A fringe connector is carried by one source space but consumes no text-area
column."
  (- (string-width prefix)
     (if (text-property-any
          0 (length prefix) 'appkit-discussion-fringe-marker t prefix)
         1
       0)))

(cl-defun appkit-discussion-insert-entry
    (entry &key width avatar-pixel-size (indent-width 4) (separate-p t)
           (avatar-p t))
  "Insert one threaded discussion ENTRY and return its buffer span.

WIDTH is the common right edge used by the timestamp.  AVATAR-PIXEL-SIZE
defaults to a two-line chat avatar.  INDENT-WIDTH is multiplied by ENTRY's
depth.  AVATAR-P controls whether the shared two-line avatar prefix is
reserved; when nil, only nesting indentation is applied.  ENTRY's connector
is nil, `continue', or `end': a continue mark draws a spine through the row
and its trailing separator, and an end mark stops that spine after the first
body row.  `appkit-discussion-connector-style' selects its presentation.
When SEPARATE-P is non-nil, append one blank line.

ENTRY's context inserter is called with no arguments and may insert zero or
more pre-heading lines without a trailing newline; empty output is ignored.
Appkit applies nesting, connector, face, and row properties to that block.
The heading inserter is called with no arguments and inserts one heading
without a newline; Appkit reserves the avatar and timestamp columns and may
elide that heading.  The body inserter receives the mutable, display-only body
prefix state and complete row properties.  It should apply that state through
Appkit prefix helpers instead of inserting it into buffer text."
  (appkit-discussion--validate-entry entry)
  (unless (and (integerp indent-width) (>= indent-width 0))
    (error "Appkit discussion indent width must be a non-negative integer"))
  (let* ((depth (or (appkit-discussion-entry-depth entry) 0))
         (connector (appkit-discussion-entry-connector entry))
         (connector-presentation
          (appkit-discussion--connector-presentation))
         (indent (make-string (* depth indent-width) ?\s))
         (pixel-size (and avatar-p
                          (or avatar-pixel-size
                              (appkit-chat-avatar-two-line-pixel-size))))
         (avatar-properties
          (and avatar-p
               (appkit-discussion--avatar-properties entry)))
         (avatar-prefixes
          (and avatar-p
               (appkit-chat-avatar-prefixes
                (appkit-discussion-entry-avatar entry)
                (or (appkit-discussion-entry-avatar-fallback entry) "@")
                :pixel-size pixel-size
                :resize t)))
         (context-prefix
          (concat (appkit-discussion--connector-column
                   connector 'context connector-presentation)
                  indent))
         (header-prefix
          (concat (appkit-discussion--connector-column
                   connector 'header connector-presentation)
                  indent
                  (when avatar-prefixes
                    (appkit-discussion--decorate-prefix
                     (plist-get avatar-prefixes :header)
                     avatar-properties))))
         (first-body-prefix
          (concat (appkit-discussion--connector-column
                   connector 'first-body connector-presentation)
                  indent
                  (when avatar-prefixes
                    (appkit-discussion--decorate-prefix
                     (plist-get avatar-prefixes :first-body)
                     avatar-properties))))
         (rest-body-prefix
          (concat (appkit-discussion--connector-column
                   connector 'rest-body connector-presentation)
                  indent
                  (or (plist-get avatar-prefixes :rest-body) "")))
         (body-prefix
          (appkit-ui-make-prefix-state first-body-prefix rest-body-prefix))
         (properties (appkit-discussion--entry-properties entry))
         (start (point)))
    (appkit-discussion--insert-context entry context-prefix properties)
    (let ((header-start (point)))
      (if-let* ((inserter (appkit-discussion-entry-heading-inserter entry)))
          (funcall inserter)
        (insert (or (appkit-discussion-entry-heading entry) "")))
      (let* ((heading-end (point))
             (time (appkit-discussion-entry-time entry))
             (target-width (or width 80))
             (prefix-width (appkit-discussion--prefix-width header-prefix))
             (heading-limit
              (and (stringp time)
                   (not (string-empty-p time))
                   (max 0 (- target-width prefix-width
                             (string-width time) 2)))))
        ;; Like Telega's message heading, reserve the trailing timestamp and
        ;; elide an overlong heading instead of inserting a third avatar row.
        (when (and heading-limit
                   (> (string-width
                       (buffer-substring header-start heading-end))
                      heading-limit))
          (let ((heading
                 (truncate-string-to-width
                  (buffer-substring header-start heading-end)
                  heading-limit nil nil "…")))
            (delete-region header-start heading-end)
            (insert heading)
            (setq heading-end (point))))
        (when-let* ((face (appkit-discussion-entry-heading-face entry)))
          (add-face-text-property header-start heading-end face 'append))
        (when (and (stringp time) (not (string-empty-p time)))
          (appkit-chat-ins-insert-right-aligned-text
           time target-width
           :face (or (appkit-discussion-entry-time-face entry) 'shadow)
           :left-prefix-width prefix-width
           :right-edge-margin 0
           :overflow-newline-p nil))
        (insert "\n")
        (appkit-ui-apply-line-prefix
         header-start (point)
         (appkit-ui-make-prefix-state header-prefix rest-body-prefix))
        (when-let* ((face (appkit-discussion-entry-heading-line-face entry)))
          (add-face-text-property header-start (point) face 'append))
        (add-text-properties header-start (point) properties)))
    (if-let* ((body-inserter (appkit-discussion-entry-body-inserter entry)))
        (funcall body-inserter body-prefix properties)
      ;; Keep the lower avatar slice visible for body-less entries.
      (appkit-ui-insert-prefixed-lines body-prefix "" :properties properties))
    (when-let* ((footer (appkit-discussion-entry-footer entry)))
      (unless (string-empty-p footer)
        (appkit-ui-insert-prefixed-lines
         body-prefix footer
         :face (or (appkit-discussion-entry-footer-face entry) 'shadow)
         :properties properties)))
    (when separate-p
      (let ((separator-start (point)))
        (insert "\n")
        (when (eq connector 'continue)
          (appkit-ui-apply-line-prefix
           separator-start (point)
           (appkit-discussion--connector-column
            connector 'separator connector-presentation)))))
    (add-text-properties start (point) properties)
    (cons start (point))))

(defun appkit-discussion-key-at-point (&optional position)
  "Return the opaque discussion key at POSITION or point."
  (let ((probe (or position (point))))
    (when (and (integer-or-marker-p probe)
               (<= (point-min) probe)
               (<= probe (point-max)))
      (or (and (< probe (point-max))
               (get-text-property probe appkit-discussion-key-property))
          (save-excursion
            (goto-char probe)
            (get-text-property (line-beginning-position)
                               appkit-discussion-key-property))))))

(defun appkit-discussion--entry-positions ()
  "Return start positions of discussion entries in the current buffer."
  (let ((position (point-min))
        (limit (point-max))
        positions
        previous-key)
    (while (< position limit)
      (let* ((key (get-text-property position appkit-discussion-key-property))
             (next (next-single-property-change
                    position appkit-discussion-key-property nil limit)))
        (when (and key (not (equal key previous-key)))
          (push position positions))
        (setq previous-key key
              position (if (> next position) next (1+ position)))))
    (nreverse positions)))

(defun appkit-discussion-next-position (&optional position)
  "Return the next discussion entry position after POSITION or point."
  (let ((probe (or position (point))))
    (seq-find (lambda (candidate) (> candidate probe))
              (appkit-discussion--entry-positions))))

(defun appkit-discussion-previous-position (&optional position)
  "Return the previous discussion entry position before POSITION or point."
  (let* ((probe (or position (point)))
         (current-key (appkit-discussion-key-at-point probe))
         (current-start
          (and current-key
               (seq-find
                (lambda (candidate)
                  (equal current-key
                         (get-text-property
                          candidate appkit-discussion-key-property)))
                (appkit-discussion--entry-positions))))
         (boundary (or current-start probe))
         previous)
    (dolist (candidate (appkit-discussion--entry-positions) previous)
      (when (< candidate boundary)
        (setq previous candidate)))))

(defun appkit-discussion-next-entry ()
  "Move point to the next discussion entry."
  (interactive)
  (if-let* ((position (appkit-discussion-next-position)))
      (goto-char position)
    (message "Appkit: no next discussion entry")))

(defun appkit-discussion-previous-entry ()
  "Move point to the previous discussion entry."
  (interactive)
  (if-let* ((position (appkit-discussion-previous-position)))
      (goto-char position)
    (message "Appkit: no previous discussion entry")))

(define-derived-mode appkit-discussion-mode special-mode "Appkit-Discussion"
  "Base mode for read-only discussion views with renderer-owned prefixes.

Enable native soft wrapping by default, preserving avatar and reply
indentation on continuation lines."
  (appkit-ui-set-soft-wrap t))

(provide 'appkit-discussion)

;;; appkit-discussion.el ends here
