;;; appkit-markup-codecs.el --- Built-in safe chat markup codecs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;;; Commentary:

;; Hook-free adapters and canonical printers for plain text, real Org Element
;; semantics, and the pinned GNU Emacs Markdown Tree-sitter grammars.  Parsing
;; never enables a major mode, Font Lock, embedded language mode, or user hook.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-element)
(require 'subr-x)
(require 'appkit-markup)
(require 'appkit-markup-codec)
(require 'appkit-markup-markdown-ts)

(defun appkit-markup-codecs--add-style (children style)
  "Return inline CHILDREN with STYLE added to every style-bearing node."
  (mapcar
   (lambda (node)
     (cond
      ((appkit-markup-text-p node)
       (appkit-markup-text
        (appkit-markup-text-text node)
        (append (appkit-markup-text-styles node) (list style))))
      ((appkit-markup-link-p node)
       (appkit-markup-link
        (appkit-markup-link-url node)
        (appkit-markup-codecs--add-style
         (appkit-markup-link-children node) style)))
      ((appkit-markup-object-p node)
       (appkit-markup-object
        (appkit-markup-object-value node)
        (appkit-markup-object-fallback node)
        (append (appkit-markup-object-styles node) (list style))))
      (t node)))
   children))

(defun appkit-markup-codecs--source-lines (source)
  "Split normalized SOURCE while preserving every trailing empty line."
  (split-string source "\n" nil))

(defun appkit-markup-codecs--plain-parse (source _context)
  "Parse literal SOURCE without interpreting formatting syntax."
  (let* ((plain (substring-no-properties source))
         (plain (replace-regexp-in-string "\r\n?" "\n" plain t t))
         children)
    (cl-loop for part in (appkit-markup-codecs--source-lines plain)
             for first = t then nil
             unless first do (push (appkit-markup-line-break) children)
             unless (string-empty-p part)
             do (push (appkit-markup-text part) children))
    (appkit-markup-parse-result
     (appkit-markup-document
      (if children
          (list (appkit-markup-paragraph (nreverse children)))
        nil)))))

(defun appkit-markup-codecs--escape-matches (text regexp)
  "Backslash every REGEXP match in TEXT."
  (replace-regexp-in-string
   regexp (lambda (match) (concat "\\" match)) text t t))

(defun appkit-markup-codecs--escape (text kind)
  "Escape literal TEXT for built-in codec KIND."
  (appkit-markup-codecs--escape-matches
   text (if (eq kind 'org) "[][\\*_/+=~]" "[][\\`*_~]")))

(defun appkit-markup-codecs--markdown-escape-block-start (text)
  "Escape a Markdown block marker at the start of literal TEXT."
  (let ((position
         (cond
          ((string-match-p
            "\\`\\(?:#\\{1,6\\}[ \t]+\\|>[ \t]?\\|```\\)" text)
           0)
          ((string-match "\\`\\( *\\)[-+*][ \t]+" text)
           (length (match-string 1 text)))
          ((string-match "\\` *[0-9]+\\([.)]\\)[ \t]+" text)
           (match-beginning 1)))))
    (if position
        (concat (substring text 0 position) "\\"
                (substring text position))
      text)))

(defun appkit-markup-codecs--markdown-escape-url (url)
  "Escape Markdown destination delimiters in URL."
  (let ((position 0)
        (start 0)
        (finish (length url))
        pieces)
    (while (< position finish)
      (let ((character (aref url position)))
        (when (memq character '(?\\ ?\( ?\)))
          (when (< start position)
            (push (substring url start position) pieces))
          (push (string ?\\ character) pieces)
          (setq start (1+ position))))
      (cl-incf position))
    (if (null pieces)
        url
      (when (< start finish)
        (push (substring url start) pieces))
      (apply #'concat (nreverse pieces)))))

(defun appkit-markup-codecs--style-delimiter (kind style)
  "Return delimiter for KIND STYLE, or nil when unsupported."
  (alist-get style
             (if (eq kind 'org)
                 '((bold . "*") (italic . "/") (underline . "_")
                   (strike . "+") (code . "~"))
               '((bold . "**") (italic . "*") (strike . "~~")
                 (code . "`")))))

(defun appkit-markup-codecs--print-inlines (children kind path losses)
  "Print inline CHILDREN for KIND at PATH, mutating LOSSES list cell."
  (let ((line-start-p t)
        active
        result)
    (cl-labels
        ((emit
          (text)
          (unless (string-empty-p text)
            (push text result)))
         (desired-styles
          (node)
          (let ((text (appkit-markup-text-text node))
                desired)
            (dolist (style (appkit-markup-text-styles node))
              (if-let* ((delimiter
                         (appkit-markup-codecs--style-delimiter kind style)))
                  (if (and (eq style 'code)
                           (string-match-p (regexp-quote delimiter) text))
                      (push (appkit-markup-loss style path) (car losses))
                    (push style desired))
                (push (appkit-markup-loss style path) (car losses))))
            (nreverse desired)))
         (transition
          (desired)
          (let ((common 0)
                (left active)
                (right desired))
            (while (and left right (eq (car left) (car right)))
              (cl-incf common)
              (setq left (cdr left)
                    right (cdr right)))
            (dolist (style (reverse (nthcdr common active)))
              (emit (appkit-markup-codecs--style-delimiter kind style)))
            (dolist (style (nthcdr common desired))
              (emit (appkit-markup-codecs--style-delimiter kind style)))
            (setq active desired)))
         (encode-special
          (node)
          (cond
           ((appkit-markup-link-p node)
            (let ((label
                   (appkit-markup-codecs--print-inlines
                    (appkit-markup-link-children node) kind
                    (append path '(label)) losses))
                  (url (appkit-markup-link-url node)))
              (if (eq kind 'org)
                  (format "[[%s][%s]]"
                          (appkit-markup-codecs--escape-matches url "[]\\]")
                          label)
                (format "[%s](%s)" label
                        (appkit-markup-codecs--markdown-escape-url url)))))
           ((appkit-markup-object-p node)
            (push (appkit-markup-loss 'object path) (car losses))
            (appkit-markup-codecs--print-inlines
             (appkit-markup-object-fallback node) kind
             (append path '(fallback)) losses))
           ((appkit-markup-line-break-p node) "\n")
           (t ""))))
      (dolist (node children)
        (if (appkit-markup-text-p node)
            (let* ((desired (desired-styles node))
                   (text (appkit-markup-text-text node))
                   (encoded
                    (if (memq 'code desired)
                        text
                      (appkit-markup-codecs--escape text kind))))
              (transition desired)
              (when (and line-start-p (eq kind 'markdown) (null desired))
                (setq encoded
                      (appkit-markup-codecs--markdown-escape-block-start
                       encoded)))
              (emit encoded)
              (setq line-start-p nil))
          (transition nil)
          (let ((encoded (encode-special node)))
            (emit encoded)
            (setq line-start-p
                  (if (string-empty-p encoded)
                      line-start-p
                    (= (aref encoded (1- (length encoded))) ?\n))))))
      (transition nil))
    (apply #'concat (nreverse result))))

(defun appkit-markup-codecs--prefix-lines (text first rest)
  "Prefix TEXT with FIRST and subsequent lines with REST."
  (let ((lines (split-string text "\n" nil)) first-line)
    (mapconcat
     (lambda (line)
       (prog1 (concat (if first-line rest first) line)
         (setq first-line t)))
     lines "\n")))

(defun appkit-markup-codecs--print-blocks (blocks kind path losses)
  "Print BLOCKS for KIND at PATH, mutating LOSSES list cell."
  (let (result)
    (cl-loop for block in blocks
             for index from 0
             for node-path = (append path (list index))
             do
             (push
              (cond
               ((appkit-markup-paragraph-p block)
                (appkit-markup-codecs--print-inlines
                 (appkit-markup-paragraph-children block)
                 kind (append node-path '(children)) losses))
               ((appkit-markup-heading-p block)
                (concat
                 (make-string (appkit-markup-heading-level block)
                              (if (eq kind 'org) ?* ?#))
                 " "
                 (appkit-markup-codecs--print-inlines
                  (appkit-markup-heading-children block)
                  kind (append node-path '(children)) losses)))
               ((appkit-markup-quote-p block)
                (let ((body
                       (appkit-markup-codecs--print-blocks
                        (appkit-markup-quote-blocks block) kind
                        (append node-path '(blocks)) losses)))
                  (if (eq kind 'org)
                      (concat "#+begin_quote\n" body "\n#+end_quote")
                    (appkit-markup-codecs--prefix-lines body "> " "> "))))
               ((appkit-markup-list-p block)
                (let ((number (or (appkit-markup-list-start block) 1))
                      items)
                  (cl-loop
                   for item in (appkit-markup-list-items block)
                   for item-index from 0
                   do
                   (let* ((marker
                           (if (eq (appkit-markup-list-style block) 'ordered)
                               (prog1 (format "%d. " number) (cl-incf number))
                             "- "))
                          (body
                           (appkit-markup-codecs--print-blocks
                            (appkit-markup-list-item-blocks item) kind
                            (append node-path (list 'items item-index 'blocks))
                            losses)))
                     (push (appkit-markup-codecs--prefix-lines
                            body marker (make-string (length marker) ?\s))
                           items)))
                  (mapconcat #'identity (nreverse items) "\n")))
               ((appkit-markup-preformatted-p block)
                (let ((text (appkit-markup-preformatted-text block))
                      (language (appkit-markup-preformatted-language block)))
                  (if (eq kind 'org)
                      (if (string-match-p
                           "^[ \t]*#[+]end_src[ \t]*$" (downcase text))
                          (progn
                            (push (appkit-markup-loss 'preformatted node-path)
                                  (car losses))
                            text)
                        (concat "#+begin_src"
                                (if language (concat " " language) "")
                                "\n" text "\n#+end_src"))
                    (let* ((runs
                            (mapcar #'length
                                    (split-string text "[^`]+" t)))
                           (width (max 3 (1+ (if runs (apply #'max runs) 0))))
                           (fence (make-string width ?`)))
                      (concat fence (or language "") "\n"
                              text "\n" fence)))))
               ((appkit-markup-object-block-p block)
                (push (appkit-markup-loss 'object-block node-path)
                      (car losses))
                (appkit-markup-codecs--print-blocks
                 (appkit-markup-object-block-fallback block) kind
                 (append node-path '(fallback)) losses))
               (t ""))
              result))
    (mapconcat #'identity (nreverse result) "\n\n")))

(defun appkit-markup-codecs--print (document kind)
  "Print DOCUMENT canonically with built-in codec KIND."
  (let ((losses (list nil)))
    (appkit-markup-print-result
     (appkit-markup-codecs--print-blocks
      (appkit-markup-document-blocks document) kind '(blocks) losses)
     (nreverse (car losses)))))

(defun appkit-markup-codecs--plain-losses (document)
  "Return losses incurred by plain export of DOCUMENT."
  (let (losses)
    (cl-labels
        ((inlines
          (nodes path)
          (cl-loop for node in nodes for index from 0
                   for here = (append path (list index)) do
                   (cond
                    ((appkit-markup-text-p node)
                     (dolist (style (appkit-markup-text-styles node))
                       (push (appkit-markup-loss style here) losses)))
                    ((appkit-markup-link-p node)
                     (push (appkit-markup-loss 'link here) losses)
                     (inlines (appkit-markup-link-children node)
                              (append here '(label))))
                    ((appkit-markup-object-p node)
                     (push (appkit-markup-loss 'object here) losses)
                     (inlines (appkit-markup-object-fallback node)
                              (append here '(fallback)))))))
         (blocks
          (nodes path)
          (when (cdr nodes)
            (push (appkit-markup-loss 'block-boundary path) losses))
          (cl-loop for node in nodes for index from 0
                   for here = (append path (list index)) do
                   (cond
                    ((appkit-markup-paragraph-p node)
                     (inlines (appkit-markup-paragraph-children node)
                              (append here '(children))))
                    ((appkit-markup-heading-p node)
                     (push (appkit-markup-loss 'heading here) losses)
                     (inlines (appkit-markup-heading-children node)
                              (append here '(children))))
                    ((appkit-markup-quote-p node)
                     (push (appkit-markup-loss 'quote here) losses)
                     (blocks (appkit-markup-quote-blocks node)
                             (append here '(blocks))))
                    ((appkit-markup-list-p node)

                     (push (appkit-markup-loss 'list here) losses))
                    ((appkit-markup-preformatted-p node)
                     (push (appkit-markup-loss 'preformatted here) losses))
                    ((appkit-markup-object-block-p node)
                     (push (appkit-markup-loss 'object-block here) losses))))))
      (blocks (appkit-markup-document-blocks document) '(blocks)))
    (nreverse losses)))

(defun appkit-markup-codecs--plain-print (document _context)
  "Print DOCUMENT as semantic plain text with explicit loss records."
  (appkit-markup-print-result
   (appkit-markup-plain-text document)
   (appkit-markup-codecs--plain-losses document)))

(defvar appkit-markup-codecs--org-diagnostics nil
  "Dynamically collected hook-free Org parse diagnostics.")

(defun appkit-markup-codecs--org-position (node property)
  "Return zero-based source PROPERTY position for Org NODE."
  (max 0 (1- (or (org-element-property property node) 1))))

(defun appkit-markup-codecs--org-diagnostic (kind node)
  "Record unsupported Org NODE as diagnostic KIND."
  (push (appkit-markup-diagnostic
         kind 'warning
         (appkit-markup-codecs--org-position node :begin)
         (appkit-markup-codecs--org-position node :end))
        appkit-markup-codecs--org-diagnostics))

(defun appkit-markup-codecs--org-string-inlines (text)
  "Convert Org plain TEXT to semantic inline nodes."
  (let ((position 0)
        (finish (length text))
        result)
    (while (< position finish)
      (let ((newline (string-match "\n" text position)))
        (if newline
            (progn
              (when (< position newline)
                (push (appkit-markup-text (substring text position newline))
                      result))
              (push (appkit-markup-line-break) result)
              (setq position (1+ newline)))
          (push (appkit-markup-text (substring text position)) result)
          (setq position finish))))
    (nreverse result)))

(defun appkit-markup-codecs--org-inlines (contents)
  "Convert Org inline CONTENTS to semantic inline nodes."
  (let (result)
    (dolist (node contents (nreverse result))
      (cond
       ((stringp node)
        (dolist (inline (appkit-markup-codecs--org-string-inlines
                         (substring-no-properties node)))
          (push inline result)))
       ((memq (org-element-type node)
              '(bold italic underline strike-through))
        (let ((style (pcase (org-element-type node)
                       ('strike-through 'strike)
                       (other other))))
          (dolist
              (inline
               (appkit-markup-codecs--add-style
                (appkit-markup-codecs--org-inlines
                 (org-element-contents node))
                style))
            (push inline result))))
       ((memq (org-element-type node) '(code verbatim))
        (push (appkit-markup-text
               (or (org-element-property :value node) "") '(code))
              result))
       ((eq (org-element-type node) 'link)
        (let* ((url (or (org-element-property :raw-link node)
                        (org-element-property :path node)
                        ""))
               (description
                (appkit-markup-codecs--org-inlines
                 (org-element-contents node)))
               (description
                (if (and description
                         (cl-every #'appkit-markup-text-p description))
                    description
                  (list (appkit-markup-text url)))))
          (push (appkit-markup-link url description) result)))
       ((eq (org-element-type node) 'line-break)
        (push (appkit-markup-line-break) result))
       (t
        (appkit-markup-codecs--org-diagnostic
         'unsupported-org-inline node)
        (let ((visible (substring-no-properties
                        (org-element-interpret-data node))))
          (unless (string-empty-p visible)
            (push (appkit-markup-text
                   (replace-regexp-in-string "[\r\n]+" " " visible))
                  result)))))
      (when (and (not (stringp node))
                 (> (or (org-element-property :post-blank node) 0) 0))
        (push (appkit-markup-text
               (make-string (org-element-property :post-blank node) ?\s))
              result)))))
(defun appkit-markup-codecs--org-paragraph (node)
  "Convert Org paragraph NODE."
  (let ((children
         (appkit-markup-codecs--org-inlines
          (org-element-contents node))))
    ;; Org includes the source line terminator in paragraph strings.  The block
    ;; boundary owns one terminator; explicit internal newlines remain.
    (when (and children (appkit-markup-line-break-p (car (last children))))
      (setq children (butlast children)))
    (appkit-markup-paragraph children)))

(defun appkit-markup-codecs--org-literal-block (node)
  "Return literal paragraph fallback for unsupported Org NODE."
  (appkit-markup-codecs--org-diagnostic 'unsupported-org-block node)
  (let* ((begin (or (org-element-property :begin node) 1))
         (end (or (org-element-property :end node) begin))
         (text (buffer-substring-no-properties begin (min end (point-max))))
         (text (string-remove-suffix "\n" text)))
    (appkit-markup-paragraph
     (appkit-markup-codecs--org-string-inlines text))))

(defun appkit-markup-codecs--org-blocks (contents)
  "Convert Org block CONTENTS to semantic blocks."
  (let (blocks)
    (dolist (node contents (nreverse blocks))
      (pcase (org-element-type node)
        ('section
         (dolist (block
                  (appkit-markup-codecs--org-blocks
                   (org-element-contents node)))
           (push block blocks)))
        ('headline
         (push (appkit-markup-heading
                (min 6 (org-element-property :level node))
                (appkit-markup-codecs--org-inlines
                 (org-element-property :title node)))
               blocks)
         (dolist (block
                  (appkit-markup-codecs--org-blocks
                   (org-element-contents node)))
           (push block blocks)))
        ('paragraph
         (push (appkit-markup-codecs--org-paragraph node) blocks))
        ('quote-block
         (push (appkit-markup-quote
                (appkit-markup-codecs--org-blocks
                 (org-element-contents node)))
               blocks))
        ('plain-list
         (let* ((style
                 (if (eq (org-element-property :type node) 'ordered)
                     'ordered
                   'unordered))
                (items
                 (mapcar
                  (lambda (item)
                    (appkit-markup-list-item
                     (appkit-markup-codecs--org-blocks
                      (org-element-contents item))))
                  (seq-filter
                   (lambda (child)
                     (and (not (stringp child))
                          (eq (org-element-type child) 'item)))
                   (org-element-contents node))))
                (bullet
                 (and items
                      (org-element-property
                       :bullet
                       (seq-find
                        (lambda (child)
                          (and (not (stringp child))
                               (eq (org-element-type child) 'item)))
                        (org-element-contents node)))))
                (start
                 (and (eq style 'ordered)
                      (stringp bullet)
                      (string-match "[0-9]+" bullet)
                      (string-to-number (match-string 0 bullet)))))
           (push (appkit-markup-list style items :start start) blocks)))
        ('src-block
         (push (appkit-markup-preformatted
                (string-remove-suffix
                 "\n" (or (org-element-property :value node) ""))
                (org-element-property :language node))
               blocks))
        ((or 'org-data 'item)
         (dolist (block
                  (appkit-markup-codecs--org-blocks
                   (org-element-contents node)))
           (push block blocks)))
        (_
         (push (appkit-markup-codecs--org-literal-block node) blocks))))))

(defun appkit-markup-codecs--org-element-parse (source)
  "Parse SOURCE through hook-free built-in Org Element semantics."
  (let (appkit-markup-codecs--org-diagnostics)
    (with-temp-buffer
      (insert (substring-no-properties source))
      (let ((org-element-use-cache nil)
            (org-inhibit-startup t)
            (org-use-property-inheritance nil))
        (let ((document
               (appkit-markup-document
                (appkit-markup-codecs--org-blocks
                 (org-element-contents (org-element-parse-buffer))))))
          (appkit-markup-parse-result
           document
           :diagnostics
           (nreverse appkit-markup-codecs--org-diagnostics)))))))
(defun appkit-markup-codecs--org-parse (source _context)
  "Parse Org SOURCE with hook-free built-in Org Element semantics."
  (appkit-markup-codecs--org-element-parse source))

(defun appkit-markup-codecs--org-print (document _context)
  "Print DOCUMENT as canonical safe Org chat source."
  (appkit-markup-codecs--print document 'org))

(defun appkit-markup-codecs--markdown-print (document _context)
  "Print DOCUMENT as canonical chat Markdown source."
  (appkit-markup-codecs--print document 'markdown))

(appkit-markup-register-codec
 'plain
 :label "Plain text"
 :parse #'appkit-markup-codecs--plain-parse
 :print #'appkit-markup-codecs--plain-print)

(appkit-markup-register-codec
 'org
 :label "Org"
 :parse #'appkit-markup-codecs--org-parse
 :print #'appkit-markup-codecs--org-print)

(appkit-markup-register-codec
 'markdown
 :label "Markdown"
 :parse #'appkit-markup-markdown-ts--parse
 :print #'appkit-markup-codecs--markdown-print)

(provide 'appkit-markup-codecs)

;;; appkit-markup-codecs.el ends here
