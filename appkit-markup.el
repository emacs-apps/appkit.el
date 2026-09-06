;;; appkit-markup.el --- Protocol-neutral semantic markup values -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;;; Commentary:

;; Immutable-by-contract semantic document values shared by Appkit clients.
;; Provider objects remain opaque; all visible fallback content is represented
;; by ordinary markup nodes.  Public constructors own containers and strip text
;; properties from every semantic string.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(define-error 'appkit-markup-invalid "Invalid Appkit markup document")

(defconst appkit-markup-style-order '(bold italic underline strike code)
  "Canonical order for inline markup styles.")

(cl-defstruct (appkit-markup-document
               (:constructor appkit-markup--document-create)
               (:copier nil))
  (blocks nil :read-only t))

(cl-defstruct (appkit-markup-paragraph
               (:constructor appkit-markup--paragraph-create)
               (:copier nil))
  (children nil :read-only t))

(cl-defstruct (appkit-markup-heading
               (:constructor appkit-markup--heading-create)
               (:copier nil))
  (level nil :read-only t)
  (children nil :read-only t))

(cl-defstruct (appkit-markup-quote
               (:constructor appkit-markup--quote-create)
               (:copier nil))
  (blocks nil :read-only t))

(cl-defstruct (appkit-markup-list
               (:constructor appkit-markup--list-create)
               (:copier nil))
  (style nil :read-only t)
  (start nil :read-only t)
  (items nil :read-only t))

(cl-defstruct (appkit-markup-list-item
               (:constructor appkit-markup--list-item-create)
               (:copier nil))
  (blocks nil :read-only t))

(cl-defstruct (appkit-markup-preformatted
               (:constructor appkit-markup--preformatted-create)
               (:copier nil))
  (text nil :read-only t)
  (language nil :read-only t))

(cl-defstruct (appkit-markup-object-block
               (:constructor appkit-markup--object-block-create)
               (:copier nil))
  (value nil :read-only t)
  (fallback nil :read-only t))

(cl-defstruct (appkit-markup-text
               (:constructor appkit-markup--text-create)
               (:copier nil))
  (text nil :read-only t)
  (styles nil :read-only t))

(cl-defstruct (appkit-markup-link
               (:constructor appkit-markup--link-create)
               (:copier nil))
  (url nil :read-only t)
  (children nil :read-only t))

(cl-defstruct (appkit-markup-object
               (:constructor appkit-markup--object-create)
               (:copier nil))
  (value nil :read-only t)
  (fallback nil :read-only t)
  (styles nil :read-only t))

(cl-defstruct (appkit-markup-line-break
               (:constructor appkit-markup--line-break-create)
               (:copier nil)))

(defconst appkit-markup--line-break
  (appkit-markup--line-break-create)
  "Shared immutable explicit line-break value.")

(defun appkit-markup--invalid (kind &optional path)
  "Signal a markup error of KIND at structural PATH.

Neither semantic text nor opaque provider values enter the condition data."
  (signal 'appkit-markup-invalid (list kind (or path 'root))))

(defun appkit-markup--proper-list (value kind)
  "Return a shallow owned copy of proper list VALUE or signal KIND."
  (unless (proper-list-p value)
    (appkit-markup--invalid kind))
  (copy-sequence value))

(defun appkit-markup--string (value kind &optional allow-nil)
  "Return property-free string VALUE or signal KIND.

When ALLOW-NIL is non-nil, preserve nil."
  (cond
   ((and allow-nil (null value)) nil)
   ((stringp value) (substring-no-properties value))
   (t (appkit-markup--invalid kind))))

(defun appkit-markup--styles (styles)
  "Return validated, canonical owned STYLES."
  (let ((styles (appkit-markup--proper-list styles 'invalid-styles)))
    (dolist (style styles)
      (unless (memq style appkit-markup-style-order)
        (appkit-markup--invalid 'unknown-style)))
    (seq-filter (lambda (style) (memq style styles))
                appkit-markup-style-order)))

(defun appkit-markup-paragraph (children)
  "Return a paragraph with owned inline CHILDREN."
  (appkit-markup--paragraph-create
   :children (appkit-markup--proper-list children 'invalid-inline-children)))

(defun appkit-markup-heading (level children)
  "Return a level LEVEL heading with owned inline CHILDREN."
  (unless (and (integerp level) (<= 1 level 6))
    (appkit-markup--invalid 'invalid-heading-level))
  (appkit-markup--heading-create
   :level level
   :children (appkit-markup--proper-list children 'invalid-inline-children)))

(defun appkit-markup-quote (blocks)
  "Return a quote with owned child BLOCKS."
  (appkit-markup--quote-create
   :blocks (appkit-markup--proper-list blocks 'invalid-blocks)))

(cl-defun appkit-markup-list (style items &key start)
  "Return a list of STYLE containing owned ITEMS.

STYLE is `ordered' or `unordered'.  START is a positive integer when present
and is meaningful only for ordered lists."
  (unless (memq style '(ordered unordered))
    (appkit-markup--invalid 'invalid-list-style))
  (when (and start (not (and (eq style 'ordered)
                             (integerp start) (> start 0))))
    (appkit-markup--invalid 'invalid-list-start))
  (appkit-markup--list-create
   :style style :start start
   :items (appkit-markup--proper-list items 'invalid-list-items)))

(defun appkit-markup-list-item (blocks)
  "Return a list item with owned child BLOCKS."
  (appkit-markup--list-item-create
   :blocks (appkit-markup--proper-list blocks 'invalid-blocks)))

(defun appkit-markup-preformatted (text &optional language)
  "Return a preformatted block containing exact TEXT and optional LANGUAGE."
  (when (and language
             (or (not (stringp language))
                 (string-empty-p language)))
    (appkit-markup--invalid 'invalid-language))
  (appkit-markup--preformatted-create
   :text (appkit-markup--string text 'invalid-preformatted-text)
   :language (appkit-markup--string language 'invalid-language t)))

(defun appkit-markup-object-block (value fallback)
  "Return a provider VALUE block with owned visible FALLBACK blocks."
  (appkit-markup--object-block-create
   :value value
   :fallback (appkit-markup--proper-list fallback 'invalid-blocks)))

(defun appkit-markup-text (text &optional styles)
  "Return exact inline TEXT with canonical STYLES."
  (let ((text (appkit-markup--string text 'invalid-text)))
    (when (string-match-p "[\r\n]" text)
      (appkit-markup--invalid 'inline-text-contains-newline))
    (appkit-markup--text-create
     :text text :styles (appkit-markup--styles styles))))

(defun appkit-markup-link (url children)
  "Return a link to URL with owned styled-text label CHILDREN.

Link labels intentionally contain text nodes only.  This prevents nested action
spans; provider adapters flatten richer labels to their visible fallback."
  (appkit-markup--link-create
   :url (appkit-markup--string url 'invalid-url)
   :children (appkit-markup--proper-list children 'invalid-link-label)))

(defun appkit-markup-object (value fallback &optional styles)
  "Return an inline provider VALUE with visible FALLBACK and STYLES."
  (appkit-markup--object-create
   :value value
   :fallback (appkit-markup--proper-list fallback 'invalid-inline-children)
   :styles (appkit-markup--styles styles)))

(defun appkit-markup-line-break ()
  "Return an explicit inline line break."
  appkit-markup--line-break)

(defun appkit-markup--enter (node active path)
  "Mark NODE active while validating PATH, rejecting graph cycles."
  (when (gethash node active)
    (appkit-markup--invalid 'cyclic-node path))
  (puthash node t active))

(defun appkit-markup--leave (node active)
  "Remove NODE from ACTIVE validation stack."
  (remhash node active))

(defun appkit-markup--normalize-inlines (children active path &optional link-label-p)
  "Normalize inline CHILDREN under PATH using ACTIVE cycle state.

When LINK-LABEL-P is non-nil, accept styled text nodes only."
  (unless (proper-list-p children)
    (appkit-markup--invalid 'invalid-inline-children path))
  (let (result previous)
    (cl-loop for child in children
             for index from 0
             for normalized = (appkit-markup--normalize-inline
                               child active (append path (list index)))
             when normalized
             do
             (when (and link-label-p
                        (not (appkit-markup-text-p normalized)))
               (appkit-markup--invalid 'invalid-link-label path))
             (if (and previous
                      (appkit-markup-text-p previous)
                      (appkit-markup-text-p normalized)
                      (equal (appkit-markup-text-styles previous)
                             (appkit-markup-text-styles normalized)))
                 (let ((merged
                        (appkit-markup--text-create
                         :text (concat (appkit-markup-text-text previous)
                                       (appkit-markup-text-text normalized))
                         :styles (appkit-markup-text-styles previous))))
                   (setcar result merged)
                   (setq previous merged))
               (push normalized result)
               (setq previous normalized)))
    (nreverse result)))

(defun appkit-markup--normalize-inline (node active path)
  "Return normalized inline NODE using ACTIVE cycle state and PATH."
  (unless (or (appkit-markup-text-p node)
              (appkit-markup-link-p node)
              (appkit-markup-object-p node)
              (appkit-markup-line-break-p node))
    (appkit-markup--invalid 'invalid-inline-node path))
  (appkit-markup--enter node active path)
  (unwind-protect
      (cond
       ((appkit-markup-text-p node)
        (let ((text (appkit-markup--string
                     (appkit-markup-text-text node) 'invalid-text)))
          (when (string-match-p "[\r\n]" text)
            (appkit-markup--invalid 'inline-text-contains-newline path))
          (unless (string-empty-p text)
            (appkit-markup--text-create
             :text text
             :styles (appkit-markup--styles
                      (appkit-markup-text-styles node))))))
       ((appkit-markup-link-p node)
        (let ((children
               (appkit-markup--normalize-inlines
                (appkit-markup-link-children node) active
                (append path '(label)) t)))
          (when children
            (appkit-markup--link-create
             :url (appkit-markup--string
                   (appkit-markup-link-url node) 'invalid-url)
             :children children))))
       ((appkit-markup-object-p node)
        (appkit-markup--object-create
         :value (appkit-markup-object-value node)
         :fallback
         (appkit-markup--normalize-inlines
          (appkit-markup-object-fallback node) active
          (append path '(fallback)))
         :styles (appkit-markup--styles
                  (appkit-markup-object-styles node))))
       (t appkit-markup--line-break))
    (appkit-markup--leave node active)))

(defun appkit-markup--normalize-list-item (item active path)
  "Return normalized list ITEM using ACTIVE cycle state and PATH."
  (unless (appkit-markup-list-item-p item)
    (appkit-markup--invalid 'invalid-list-item path))
  (appkit-markup--enter item active path)
  (unwind-protect
      (appkit-markup--list-item-create
       :blocks (appkit-markup--normalize-blocks
                (appkit-markup-list-item-blocks item) active
                (append path '(blocks))))
    (appkit-markup--leave item active)))

(defun appkit-markup--normalize-blocks (blocks active path)
  "Normalize BLOCKS under PATH using ACTIVE cycle state."
  (unless (proper-list-p blocks)
    (appkit-markup--invalid 'invalid-blocks path))
  (let (result)
    (cl-loop for block in blocks
             for index from 0
             for normalized = (appkit-markup--normalize-block
                               block active (append path (list index)))
             when normalized do (push normalized result))
    (nreverse result)))

(defun appkit-markup--normalize-block (node active path)
  "Return normalized block NODE using ACTIVE cycle state and PATH."
  (unless (or (appkit-markup-paragraph-p node)
              (appkit-markup-heading-p node)
              (appkit-markup-quote-p node)
              (appkit-markup-list-p node)
              (appkit-markup-preformatted-p node)
              (appkit-markup-object-block-p node))
    (appkit-markup--invalid 'invalid-block-node path))
  (appkit-markup--enter node active path)
  (unwind-protect
      (cond
       ((appkit-markup-paragraph-p node)
        (when-let* ((children
                     (appkit-markup--normalize-inlines
                      (appkit-markup-paragraph-children node) active
                      (append path '(children)))))
          (appkit-markup--paragraph-create :children children)))
       ((appkit-markup-heading-p node)
        (let ((level (appkit-markup-heading-level node)))
          (unless (and (integerp level) (<= 1 level 6))
            (appkit-markup--invalid 'invalid-heading-level path))
          (when-let* ((children
                       (appkit-markup--normalize-inlines
                        (appkit-markup-heading-children node) active
                        (append path '(children)))))
            (appkit-markup--heading-create
             :level level :children children))))
       ((appkit-markup-quote-p node)
        (when-let* ((blocks
                     (appkit-markup--normalize-blocks
                      (appkit-markup-quote-blocks node) active
                      (append path '(blocks)))))
          (appkit-markup--quote-create :blocks blocks)))
       ((appkit-markup-list-p node)
        (let ((style (appkit-markup-list-style node))
              (start (appkit-markup-list-start node))
              items)
          (unless (memq style '(ordered unordered))
            (appkit-markup--invalid 'invalid-list-style path))
          (when (and start
                     (not (and (eq style 'ordered)
                               (integerp start) (> start 0))))
            (appkit-markup--invalid 'invalid-list-start path))
          (unless (proper-list-p (appkit-markup-list-items node))
            (appkit-markup--invalid 'invalid-list-items path))
          (cl-loop for item in (appkit-markup-list-items node)
                   for index from 0
                   do (push (appkit-markup--normalize-list-item
                             item active (append path (list 'items index)))
                            items))
          (when items
            (appkit-markup--list-create
             :style style :start start :items (nreverse items)))))
       ((appkit-markup-preformatted-p node)
        (let ((language (appkit-markup-preformatted-language node)))
          (when (and language
                     (or (not (stringp language))
                         (string-empty-p language)))
            (appkit-markup--invalid 'invalid-language path))
          (appkit-markup--preformatted-create
           :text (appkit-markup--string
                  (appkit-markup-preformatted-text node)
                  'invalid-preformatted-text)
           :language (appkit-markup--string language 'invalid-language t))))
       (t
        (appkit-markup--object-block-create
         :value (appkit-markup-object-block-value node)
         :fallback
         (appkit-markup--normalize-blocks
          (appkit-markup-object-block-fallback node) active
          (append path '(fallback))))))
    (appkit-markup--leave node active)))

(defun appkit-markup-document (blocks)
  "Return a normalized semantic document containing BLOCKS."
  (appkit-markup--document-create
   :blocks (appkit-markup--normalize-blocks
            (appkit-markup--proper-list blocks 'invalid-blocks)
            (make-hash-table :test #'eq) '(blocks))))

(defun appkit-markup-normalize (document)
  "Return a normalized owned copy of markup DOCUMENT."
  (unless (appkit-markup-document-p document)
    (appkit-markup--invalid 'invalid-document))
  (appkit-markup-document (appkit-markup-document-blocks document)))

(defun appkit-markup-validate (document)
  "Validate DOCUMENT and return non-nil without exposing its contents."
  (appkit-markup-normalize document)
  t)

(defun appkit-markup--node-equal-p (left right object-equal)
  "Compare markup nodes LEFT and RIGHT using OBJECT-EQUAL for opaque values."
  (cond
   ((and (appkit-markup-document-p left) (appkit-markup-document-p right))
    (appkit-markup--sequence-equal-p
     (appkit-markup-document-blocks left)
     (appkit-markup-document-blocks right) object-equal))
   ((and (appkit-markup-paragraph-p left) (appkit-markup-paragraph-p right))
    (appkit-markup--sequence-equal-p
     (appkit-markup-paragraph-children left)
     (appkit-markup-paragraph-children right) object-equal))
   ((and (appkit-markup-heading-p left) (appkit-markup-heading-p right))
    (and (= (appkit-markup-heading-level left)
            (appkit-markup-heading-level right))
         (appkit-markup--sequence-equal-p
          (appkit-markup-heading-children left)
          (appkit-markup-heading-children right) object-equal)))
   ((and (appkit-markup-quote-p left) (appkit-markup-quote-p right))
    (appkit-markup--sequence-equal-p
     (appkit-markup-quote-blocks left)
     (appkit-markup-quote-blocks right) object-equal))
   ((and (appkit-markup-list-p left) (appkit-markup-list-p right))
    (and (eq (appkit-markup-list-style left)
             (appkit-markup-list-style right))
         (equal (appkit-markup-list-start left)
                (appkit-markup-list-start right))
         (appkit-markup--sequence-equal-p
          (appkit-markup-list-items left)
          (appkit-markup-list-items right) object-equal)))
   ((and (appkit-markup-list-item-p left) (appkit-markup-list-item-p right))
    (appkit-markup--sequence-equal-p
     (appkit-markup-list-item-blocks left)
     (appkit-markup-list-item-blocks right) object-equal))
   ((and (appkit-markup-preformatted-p left)
         (appkit-markup-preformatted-p right))
    (and (equal (appkit-markup-preformatted-text left)
                (appkit-markup-preformatted-text right))
         (equal (appkit-markup-preformatted-language left)
                (appkit-markup-preformatted-language right))))
   ((and (appkit-markup-object-block-p left)
         (appkit-markup-object-block-p right))
    (and (funcall object-equal
                  (appkit-markup-object-block-value left)
                  (appkit-markup-object-block-value right))
         (appkit-markup--sequence-equal-p
          (appkit-markup-object-block-fallback left)
          (appkit-markup-object-block-fallback right) object-equal)))
   ((and (appkit-markup-text-p left) (appkit-markup-text-p right))
    (and (equal (appkit-markup-text-text left)
                (appkit-markup-text-text right))
         (equal (appkit-markup-text-styles left)
                (appkit-markup-text-styles right))))
   ((and (appkit-markup-link-p left) (appkit-markup-link-p right))
    (and (equal (appkit-markup-link-url left)
                (appkit-markup-link-url right))
         (appkit-markup--sequence-equal-p
          (appkit-markup-link-children left)
          (appkit-markup-link-children right) object-equal)))
   ((and (appkit-markup-object-p left) (appkit-markup-object-p right))
    (and (funcall object-equal
                  (appkit-markup-object-value left)
                  (appkit-markup-object-value right))
         (equal (appkit-markup-object-styles left)
                (appkit-markup-object-styles right))
         (appkit-markup--sequence-equal-p
          (appkit-markup-object-fallback left)
          (appkit-markup-object-fallback right) object-equal)))
   ((and (appkit-markup-line-break-p left)
         (appkit-markup-line-break-p right)) t)
   (t nil)))

(defun appkit-markup--sequence-equal-p (left right object-equal)
  "Compare node sequences LEFT and RIGHT using OBJECT-EQUAL."
  (and (= (length left) (length right))
       (cl-every (lambda (a b)
                   (appkit-markup--node-equal-p a b object-equal))
                 left right)))

(defun appkit-markup-equal-p (left right &optional object-equal)
  "Return non-nil when documents LEFT and RIGHT are semantically equal.

OBJECT-EQUAL defaults to `eq' and compares provider-owned opaque values."
  (appkit-markup--node-equal-p
   (appkit-markup-normalize left)
   (appkit-markup-normalize right)
   (or object-equal #'eq)))

(defun appkit-markup--plain-inlines (children)
  "Return semantic plain text for inline CHILDREN."
  (mapconcat
   (lambda (node)
     (cond
      ((appkit-markup-text-p node) (appkit-markup-text-text node))
      ((appkit-markup-link-p node)
       (appkit-markup--plain-inlines (appkit-markup-link-children node)))
      ((appkit-markup-object-p node)
       (appkit-markup--plain-inlines (appkit-markup-object-fallback node)))
      ((appkit-markup-line-break-p node) "\n")
      (t "")))
   children ""))

(defun appkit-markup--prefix-lines (text first rest)
  "Prefix TEXT's first line with FIRST and later lines with REST."
  (let ((lines (split-string text "\n" nil)))
    (mapconcat (lambda (line)
                 (prog1 (concat first line)
                   (setq first rest)))
               lines "\n")))

(defun appkit-markup--plain-block (block)
  "Return semantic plain text for one normalized BLOCK."
  (cond
   ((appkit-markup-paragraph-p block)
    (appkit-markup--plain-inlines (appkit-markup-paragraph-children block)))
   ((appkit-markup-heading-p block)
    (appkit-markup--plain-inlines (appkit-markup-heading-children block)))
   ((appkit-markup-quote-p block)
    (appkit-markup--prefix-lines
     (appkit-markup--plain-blocks (appkit-markup-quote-blocks block))
     "> " "> "))
   ((appkit-markup-list-p block)
    (let ((number (or (appkit-markup-list-start block) 1))
          result)
      (dolist (item (appkit-markup-list-items block))
        (let* ((marker (if (eq (appkit-markup-list-style block) 'ordered)
                           (prog1 (format "%d. " number)
                             (setq number (1+ number)))
                         "- "))
               (body (appkit-markup--plain-blocks
                      (appkit-markup-list-item-blocks item))))
          (push (appkit-markup--prefix-lines
                 body marker (make-string (length marker) ?\s))
                result)))
      (mapconcat #'identity (nreverse result) "\n")))
   ((appkit-markup-preformatted-p block)
    (appkit-markup-preformatted-text block))
   ((appkit-markup-object-block-p block)
    (appkit-markup--plain-blocks
     (appkit-markup-object-block-fallback block)))
   (t "")))

(defun appkit-markup--plain-blocks (blocks)
  "Return semantic plain text for normalized BLOCKS."
  (mapconcat #'appkit-markup--plain-block blocks "\n"))

(defun appkit-markup-plain-text (document)
  "Return a property-free semantic plain-text export of DOCUMENT."
  (appkit-markup--plain-blocks
   (appkit-markup-document-blocks (appkit-markup-normalize document))))

(provide 'appkit-markup)

;;; appkit-markup.el ends here
