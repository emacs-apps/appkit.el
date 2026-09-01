;;; appkit-markup-ui.el --- Native insertion for Appkit markup -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;;; Commentary:

;; Deterministic native Emacs presentation for `appkit-markup-document' values.
;; Shared nodes receive shared geometry and faces.  Interactive provider objects
;; remain client-owned callbacks; non-interactive rendering always traverses
;; semantic fallback and never invokes client code.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-markup)
(require 'appkit-ui)

(defface appkit-markup-heading-face
  '((t :inherit bold))
  "Base face for semantic headings."
  :group 'appkit)

(defface appkit-markup-link-face
  '((t :inherit link))
  "Face for semantic links."
  :group 'appkit)

(defface appkit-markup-code-face
  '((t :inherit fixed-pitch :background unspecified))
  "Face for inline semantic code."
  :group 'appkit)

(defface appkit-markup-preformatted-face
  '((t :inherit fixed-pitch))
  "Face for preformatted semantic blocks."
  :group 'appkit)

(defface appkit-markup-quote-face
  '((t :inherit shadow))
  "Face for quote presentation prefixes."
  :group 'appkit)

(defface appkit-markup-object-fallback-face
  '((t :inherit default))
  "Face for visible provider-object fallback content."
  :group 'appkit)

(defconst appkit-markup-ui--reserved-properties
  (list 'face 'font-lock-face 'display 'line-prefix 'wrap-prefix 'keymap
        'local-map 'mouse-face 'pointer 'help-echo
        appkit-ui-action-property)
  "Text properties owned by the markup renderer rather than outer rows.")

(defconst appkit-markup-ui--heading-heights
  [1.35 1.25 1.18 1.12 1.06 1.0]
  "Relative display heights for heading levels one through six.")

(defun appkit-markup-ui--validate-properties (properties)
  "Validate outer row PROPERTIES without exposing property values."
  (unless (proper-list-p properties)
    (signal 'wrong-type-argument '(proper-list-p properties)))
  (let ((cursor properties))
    (while cursor
      (unless (consp (cdr cursor))
        (error "Appkit markup row properties must be a plist"))
      (when (memq (car cursor) appkit-markup-ui--reserved-properties)
        (error "Appkit markup row properties contain renderer-owned property %S"
               (car cursor)))
      (setq cursor (cddr cursor))))
  properties)

(defun appkit-markup-ui--style-face (style)
  "Return the native face used for semantic STYLE."
  (pcase style
    ('bold 'bold)
    ('italic 'italic)
    ('underline 'underline)
    ('strike '(:strike-through t))
    ('code 'appkit-markup-code-face)
    (_ nil)))

(defun appkit-markup-ui--apply-styles (start end styles)
  "Append semantic STYLES to START..END."
  (dolist (style styles)
    (when-let* ((face (appkit-markup-ui--style-face style)))
      (add-face-text-property start end face 'append))))

(defun appkit-markup-ui--call-inserter (inserter node)
  "Call client INSERTER for NODE inside one append-only boundary."
  (let ((buffer (current-buffer))
        (start (point)))
    (save-restriction
      ;; A zero-width restriction grows with insertion but exposes none of the
      ;; caller's existing suffix to a client renderer.
      (narrow-to-region start start)
      (funcall inserter node)
      (unless (eq (current-buffer) buffer)
        (error "Appkit markup inserter changed the current buffer"))
      (unless (= (point) (point-max))
        (error "Appkit markup inserter did not finish after its insertion")))))

(defun appkit-markup-ui--call-link-action (function url)
  "Call link action factory FUNCTION for URL without permitting buffer edits."
  (let ((buffer (current-buffer))
        (position (point))
        (minimum (point-min))
        (maximum (point-max))
        (tick (buffer-modified-tick))
        action)
    (setq action (funcall function url))
    (unless (and (eq (current-buffer) buffer)
                 (= (point) position)
                 (= (point-min) minimum)
                 (= (point-max) maximum)
                 (= (buffer-modified-tick) tick))
      (error "Appkit markup link action factory mutated renderer state"))
    action))

(cl-defun appkit-markup-ui--insert-inlines
    (children &key interactive-p link-action object-inserter)
  "Insert inline CHILDREN under the current rendering policy."
  (dolist (node children)
    (cond
     ((appkit-markup-text-p node)
      (let ((start (point)))
        (insert (appkit-markup-text-text node))
        (appkit-markup-ui--apply-styles
         start (point) (appkit-markup-text-styles node))))
     ((appkit-markup-line-break-p node)
      (insert "\n"))
     ((appkit-markup-link-p node)
      (let ((start (point)))
        (appkit-markup-ui--insert-inlines
         (appkit-markup-link-children node)
         :interactive-p nil)
        (when (< start (point))
          (add-face-text-property start (point) 'appkit-markup-link-face 'append)
          (when (and interactive-p (functionp link-action))
            (when-let* ((action
                         (appkit-markup-ui--call-link-action
                          link-action (appkit-markup-link-url node))))
              (appkit-ui-add-action
               start (point) action
               :help-echo (appkit-markup-link-url node)
               :face 'appkit-markup-link-face))))))
     ((appkit-markup-object-p node)
      (let ((start (point)))
        (if (and interactive-p (functionp object-inserter))
            (appkit-markup-ui--call-inserter object-inserter node)
          (appkit-markup-ui--insert-inlines
           (appkit-markup-object-fallback node)
           :interactive-p nil))
        (appkit-markup-ui--apply-styles
         start (point) (appkit-markup-object-styles node))))
     (t (error "Unsupported normalized Appkit inline node")))))

(defun appkit-markup-ui--ensure-terminator ()
  "Insert one block terminator and return its position."
  (prog1 (point) (insert "\n")))

(cl-defun appkit-markup-ui--insert-blocks
    (blocks &key interactive-p link-action object-inserter
            preformatted-inserter)
  "Insert normalized BLOCKS under the current rendering policy."
  (dolist (block blocks)
    (cond
     ((appkit-markup-paragraph-p block)
      (appkit-markup-ui--insert-inlines
       (appkit-markup-paragraph-children block)
       :interactive-p interactive-p
       :link-action link-action
       :object-inserter object-inserter)
      (appkit-markup-ui--ensure-terminator))
     ((appkit-markup-heading-p block)
      (let ((start (point))
            (level (appkit-markup-heading-level block)))
        (appkit-markup-ui--insert-inlines
         (appkit-markup-heading-children block)
         :interactive-p interactive-p
         :link-action link-action
         :object-inserter object-inserter)
        (add-face-text-property
         start (point)
         (list :inherit 'appkit-markup-heading-face
               :height (aref appkit-markup-ui--heading-heights (1- level)))
         'append)
        (appkit-markup-ui--ensure-terminator)))
     ((appkit-markup-quote-p block)
      (let ((start (point)))
        (appkit-markup-ui--insert-blocks
         (appkit-markup-quote-blocks block)
         :interactive-p interactive-p
         :link-action link-action
         :object-inserter object-inserter
         :preformatted-inserter preformatted-inserter)
        (appkit-ui-apply-line-prefix
         start (point) (propertize "│ " 'face 'appkit-markup-quote-face))))
     ((appkit-markup-list-p block)
      (let ((number (or (appkit-markup-list-start block) 1)))
        (dolist (item (appkit-markup-list-items block))
          (let* ((start (point))
                 (marker
                  (if (eq (appkit-markup-list-style block) 'ordered)
                      (prog1 (format "%d. " number)
                        (setq number (1+ number)))
                    "• ")))
            (appkit-markup-ui--insert-blocks
             (appkit-markup-list-item-blocks item)
             :interactive-p interactive-p
             :link-action link-action
             :object-inserter object-inserter
             :preformatted-inserter preformatted-inserter)
            ;; Empty list items retain one visible, selectable row.
            (when (= start (point))
              (appkit-markup-ui--ensure-terminator))
            (appkit-ui-apply-line-prefix
             start (point)
             (appkit-ui-make-prefix-state
              marker (make-string (string-width marker) ?\s)))))))
     ((appkit-markup-preformatted-p block)
      (let ((start (point)))
        (if (and interactive-p (functionp preformatted-inserter))
            (appkit-markup-ui--call-inserter preformatted-inserter block)
          (insert (appkit-markup-preformatted-text block))
          (add-face-text-property
           start (point) 'appkit-markup-preformatted-face 'append))
        (appkit-markup-ui--ensure-terminator)))
     ((appkit-markup-object-block-p block)
      (let ((start (point)))
        (if (and interactive-p (functionp object-inserter))
            (appkit-markup-ui--call-inserter object-inserter block)
          (appkit-markup-ui--insert-blocks
           (appkit-markup-object-block-fallback block)
           :interactive-p nil)
          (add-face-text-property
           start (point) 'appkit-markup-object-fallback-face 'append))
        (when (or (= start (point)) (not (bolp)))
          (appkit-markup-ui--ensure-terminator))))
     (t (error "Unsupported normalized Appkit block node")))))

(cl-defun appkit-markup-ui-insert-document
    (document &key prefix properties (final-newline-p t) interactive-p
              link-action object-inserter preformatted-inserter)
  "Insert semantic DOCUMENT at point and return its exact buffer bounds.

PREFIX is applied through `appkit-ui-apply-line-prefix'.  PROPERTIES are outer
row metadata and may not contain renderer-owned presentation/action properties.
When FINAL-NEWLINE-P is nil, remove only the renderer's final block terminator.

INTERACTIVE-P permits LINK-ACTION, OBJECT-INSERTER, and
PREFORMATTED-INSERTER.  With nil INTERACTIVE-P no client callback runs: links
are inert, objects traverse their stored fallback, and code remains fixed-pitch.
LINK-ACTION receives a URL and returns a zero-argument action or nil.  The two
inserters receive their complete semantic node.  Their callback runs in an
empty restriction that grows only through forward insertion and must finish at
the end of that insertion.

Insertion is atomic.  Errors leave no partial document behind."
  (let ((document (appkit-markup-normalize document))
        (buffer (current-buffer))
        (start (point))
        final-terminator)
    (appkit-markup-ui--validate-properties properties)
    (atomic-change-group
      (save-restriction
        (narrow-to-region start (point-max))
        (appkit-markup-ui--insert-blocks
         (appkit-markup-document-blocks document)
         :interactive-p interactive-p
         :link-action link-action
         :object-inserter object-inserter
         :preformatted-inserter preformatted-inserter)
        (unless (eq (current-buffer) buffer)
          (error "Appkit markup renderer changed the current buffer"))
        (when (and (not final-newline-p)
                   (> (point) start)
                   (eq (char-before) ?\n))
          ;; Every top-level block renderer owns its final terminator.
          (setq final-terminator (1- (point)))
          (delete-region final-terminator (point)))
        (when properties
          (add-text-properties start (point) properties))
        (when (and prefix (< start (point)))
          (appkit-ui-apply-line-prefix start (point) prefix))))
    (cons start (point))))

(provide 'appkit-markup-ui)

;;; appkit-markup-ui.el ends here
