;;; appkit-markup-markdown-ts.el --- Markdown Tree-sitter adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;;; Commentary:

;; One-shot, hook-free adaptation from the pinned GNU Emacs Markdown and
;; Markdown-inline Tree-sitter grammars to Appkit semantic Documents.  This
;; module never enables `markdown-ts-mode', Font Lock, embedded language modes,
;; or grammar installation during parsing.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'treesit)
(require 'appkit-markup)
(require 'appkit-markup-codec)

(defcustom appkit-markup-markdown-ts-node-limit 20000
  "Maximum named Tree-sitter nodes visited by one Markdown parse."
  :type 'integer
  :group 'appkit)

(defcustom appkit-markup-markdown-ts-depth-limit 128
  "Maximum Tree-sitter nesting depth adapted by one Markdown parse."
  :type 'integer
  :group 'appkit)

(defconst appkit-markup-markdown-ts--grammar-commit
  "413285231ce8fa8b11e7074bbe265b48aa7277f9"
  "Pinned tree-sitter-markdown commit tested by GNU Emacs markdown-ts-mode.")

(defconst appkit-markup-markdown-ts--grammar-sources
  `((markdown
     "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
     :commit ,appkit-markup-markdown-ts--grammar-commit
     :source-dir "tree-sitter-markdown/src")
    (markdown-inline
     "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
     :commit ,appkit-markup-markdown-ts--grammar-commit
     :source-dir "tree-sitter-markdown-inline/src"))
  "Pinned grammar installation recipes for Appkit Markdown composition.")

(defconst appkit-markup-markdown-ts--escapable-punctuation
  "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
  "ASCII punctuation that Markdown permits after a backslash escape.")

(defvar appkit-markup-markdown-ts--diagnostics nil)
(defvar appkit-markup-markdown-ts--diagnostic-count 0)
(defvar appkit-markup-markdown-ts--node-count 0)
(defvar appkit-markup-markdown-ts--inline-ranges nil)
(defvar appkit-markup-markdown-ts--context nil)
(defvar appkit-markup-markdown-ts--references nil)
(defvar appkit-markup-markdown-ts--table-cell-p nil)

(defun appkit-markup-markdown-ts-ready-p ()
  "Return non-nil when both pinned-shape Markdown grammars are available."
  (and (treesit-available-p)
       (treesit-language-available-p 'markdown)
       (treesit-language-available-p 'markdown-inline)))

;;;###autoload
(defun appkit-markup-markdown-ts-install-grammars ()
  "Explicitly install Appkit's pinned Markdown Tree-sitter grammars."
  (interactive)
  (unless (treesit-available-p)
    (user-error "This Emacs was built without Tree-sitter support"))
  (dolist (source appkit-markup-markdown-ts--grammar-sources)
    (let ((language (car source)))
      (unless (treesit-language-available-p language)
        (let ((treesit-language-source-alist
               (cons source
                     (cl-remove language treesit-language-source-alist
                                :key #'car))))
          (treesit-install-language-grammar language)))))
  (unless (appkit-markup-markdown-ts-ready-p)
    (user-error "Markdown Tree-sitter grammar installation did not complete"))
  (message "Appkit Markdown Tree-sitter grammars are ready"))

(defun appkit-markup-markdown-ts--ensure-ready ()
  "Signal a content-free codec error unless both grammars are available."
  (unless (appkit-markup-markdown-ts-ready-p)
    (signal 'appkit-markup-codec-error
            '(markdown-tree-sitter-grammars-unavailable))))

(defun appkit-markup-markdown-ts--visit (depth)
  "Account for one node at DEPTH or reject an excessive syntax tree."
  (cl-incf appkit-markup-markdown-ts--node-count)
  (unless (and (integerp appkit-markup-markdown-ts-node-limit)
               (> appkit-markup-markdown-ts-node-limit 0)
               (integerp appkit-markup-markdown-ts-depth-limit)
               (> appkit-markup-markdown-ts-depth-limit 0)
               (<= appkit-markup-markdown-ts--node-count
                   appkit-markup-markdown-ts-node-limit)
               (<= depth appkit-markup-markdown-ts-depth-limit))
    (signal 'appkit-markup-codec-error '(markdown-tree-too-complex))))

(defun appkit-markup-markdown-ts--children (node)
  "Return NODE's named direct children in source order."
  (cl-loop for index below (treesit-node-child-count node t)
           collect (treesit-node-child node index t)))

(defun appkit-markup-markdown-ts--child (node type)
  "Return NODE's first named direct child of TYPE."
  (cl-find type (appkit-markup-markdown-ts--children node)
           :key #'treesit-node-type :test #'string=))

(defun appkit-markup-markdown-ts--children-of-type (node type)
  "Return NODE's named direct children of TYPE."
  (cl-remove-if-not
   (lambda (child) (string= (treesit-node-type child) type))
   (appkit-markup-markdown-ts--children node)))

(defun appkit-markup-markdown-ts--node-source (node)
  "Return property-free exact source covered by NODE."
  (buffer-substring-no-properties
   (treesit-node-start node) (treesit-node-end node)))

(defun appkit-markup-markdown-ts--diagnostic (kind node)
  "Record bounded diagnostic KIND at NODE's zero-based source bounds."
  (when (< appkit-markup-markdown-ts--diagnostic-count
           appkit-markup-codec-diagnostic-limit)
    (cl-incf appkit-markup-markdown-ts--diagnostic-count)
    (push
     (appkit-markup-diagnostic
      kind 'warning
      (max 0 (1- (treesit-node-start node)))
      (max 0 (1- (treesit-node-end node))))
     appkit-markup-markdown-ts--diagnostics)))

(defun appkit-markup-markdown-ts--ranges-excluding (node excluded-type)
  "Return NODE ranges excluding direct children of EXCLUDED-TYPE."
  (let ((cursor (treesit-node-start node))
        ranges)
    (dolist (child (appkit-markup-markdown-ts--children node))
      (when (string= (treesit-node-type child) excluded-type)
        (when (< cursor (treesit-node-start child))
          (push (cons cursor (treesit-node-start child)) ranges))
        (setq cursor (max cursor (treesit-node-end child)))))
    (when (< cursor (treesit-node-end node))
      (push (cons cursor (treesit-node-end node)) ranges))
    (nreverse ranges)))

(defun appkit-markup-markdown-ts--ranges-text (ranges)
  "Return property-free source concatenated from RANGES."
  (mapconcat
   (lambda (range)
     (buffer-substring-no-properties (car range) (cdr range)))
   ranges ""))

(defun appkit-markup-markdown-ts--inline-literal (start end)
  "Return included inline source between START and END."
  (let (pieces)
    (dolist (range appkit-markup-markdown-ts--inline-ranges)
      (let ((left (max start (car range)))
            (right (min end (cdr range))))
        (when (< left right)
          (push (buffer-substring-no-properties left right) pieces))))
    (apply #'concat (nreverse pieces))))

(defun appkit-markup-markdown-ts--text-nodes (text &optional styles)
  "Return semantic inline nodes for exact TEXT with optional STYLES."
  (let (nodes first)
    (dolist (line (split-string text "\n" nil) (nreverse nodes))
      (when first
        (push (appkit-markup-line-break) nodes))
      (setq first t)
      (unless (string-empty-p line)
        (push (appkit-markup-text line styles) nodes)))))

(defun appkit-markup-markdown-ts--add-style (nodes style)
  "Return NODES with STYLE added to every semantic text node."
  (mapcar
   (lambda (node)
     (if (appkit-markup-text-p node)
         (appkit-markup-text
          (appkit-markup-text-text node)
          (append (appkit-markup-text-styles node) (list style)))
       node))
   nodes))

(defun appkit-markup-markdown-ts--unescape (text)
  "Resolve CommonMark backslash escapes in TEXT."
  (let ((position 0)
        (finish (length text))
        characters)
    (while (< position finish)
      (let ((character (aref text position)))
        (if (and (= character ?\\)
                 (< (1+ position) finish)
                 (string-match-p
                  (regexp-quote
                   (char-to-string (aref text (1+ position))))
                  appkit-markup-markdown-ts--escapable-punctuation))
            (progn
              (push (aref text (1+ position)) characters)
              (setq position (+ position 2)))
          (push character characters)
          (cl-incf position))))
    (apply #'string (nreverse characters))))

(defun appkit-markup-markdown-ts--reference-label (label)
  "Normalize reference LABEL using CommonMark whitespace and case folding."
  ;; Upper-then-lower casing expands sharp S and ligatures.  Unicode's two
  ;; Turkic I exceptions must not acquire the ASCII I equivalence class.
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     "[^İı]+" (lambda (text) (downcase (upcase text)))
     (replace-regexp-in-string
      "İ" "i̇"
      (string-trim
       (replace-regexp-in-string "[ \t\r\n]+" " " label) " " " ")
      t t)
     t t)))

(defun appkit-markup-markdown-ts--destination (node)
  "Return the unescaped destination of link destination NODE."
  (let ((text (appkit-markup-markdown-ts--node-source node)))
    (when (and (> (length text) 1)
               (= (aref text 0) ?<)
               (= (aref text (1- (length text))) ?>))
      (setq text (substring text 1 -1)))
    (appkit-markup-markdown-ts--unescape text)))

(defun appkit-markup-markdown-ts--collect-references (node depth)
  "Index definitions in the existing block NODE tree at DEPTH.
Only block containers are descended, never code or inline source.  The first
definition wins, including definitions after the links which use them."
  (appkit-markup-markdown-ts--visit depth)
  (pcase (treesit-node-type node)
    ("link_reference_definition"
     (let* ((label (appkit-markup-markdown-ts--child node "link_label"))
            (destination
             (appkit-markup-markdown-ts--child node "link_destination"))
            (key (and label
                      (appkit-markup-markdown-ts--reference-label
                       (substring
                        (appkit-markup-markdown-ts--node-source label) 1 -1)))))
       (when (and key destination
                  (not (gethash key appkit-markup-markdown-ts--references)))
         (puthash key (appkit-markup-markdown-ts--destination destination)
                  appkit-markup-markdown-ts--references))))
    ((or "document" "section" "block_quote" "list" "list_item")
     (dolist (child (appkit-markup-markdown-ts--children node))
       (appkit-markup-markdown-ts--collect-references child (1+ depth))))))

(defun appkit-markup-markdown-ts--inline-children (node depth)
  "Convert NODE's inline contents at DEPTH, preserving literal gaps."
  (let ((cursor (treesit-node-start node))
        result)
    (dolist (child (appkit-markup-markdown-ts--children node))
      (when (< cursor (treesit-node-start child))
        (dolist
            (text-node
             (appkit-markup-markdown-ts--text-nodes
              (appkit-markup-markdown-ts--inline-literal
               cursor (treesit-node-start child))))
          (push text-node result)))
      (dolist (converted
               (appkit-markup-markdown-ts--inline-node child (1+ depth)))
        (push converted result))
      (setq cursor (treesit-node-end child)))
    (when (< cursor (treesit-node-end node))
      (dolist
          (text-node
           (appkit-markup-markdown-ts--text-nodes
            (appkit-markup-markdown-ts--inline-literal
             cursor (treesit-node-end node))))
        (push text-node result)))
    (nreverse result)))

(defun appkit-markup-markdown-ts--code-span (node)
  "Convert inline code span NODE."
  (let* ((delimiters
          (appkit-markup-markdown-ts--children-of-type
           node "code_span_delimiter"))
         (start (and delimiters (treesit-node-end (car delimiters))))
         (end (and delimiters
                   (treesit-node-start (car (last delimiters))))))
    (if (not (and start end (<= start end)))
        (progn
          (appkit-markup-markdown-ts--diagnostic
           'unsupported-markdown-inline node)
          (appkit-markup-markdown-ts--text-nodes
           (appkit-markup-markdown-ts--node-source node)))
      (let ((text
             (replace-regexp-in-string
              "[\r\n]+" " "
              (buffer-substring-no-properties start end))))
        (when appkit-markup-markdown-ts--table-cell-p
          (setq text (replace-regexp-in-string "\\\\|" "|" text t t)))
        (when (and (> (length text) 1)
                   (= (aref text 0) ?\s)
                   (= (aref text (1- (length text))) ?\s)
                   (string-match-p "[^ ]" text))
          (setq text (substring text 1 -1)))
        (list (appkit-markup-text text '(code)))))))

(defun appkit-markup-markdown-ts--link-label (nodes fallback)
  "Return text-only link label NODES, using FALLBACK when empty."
  (let (label)
    (dolist (node nodes)
      (cond
       ((appkit-markup-text-p node) (push node label))
       ((appkit-markup-line-break-p node)
        (push (appkit-markup-text " ") label))))
    (or (nreverse label) (list (appkit-markup-text fallback)))))

(defun appkit-markup-markdown-ts--inline-link (node depth &optional image-p)
  "Convert link or IMAGE-P image NODE at DEPTH, resolving references."
  (let* ((label-node
          (appkit-markup-markdown-ts--child
           node (if image-p "image_description" "link_text")))
         (reference-node (appkit-markup-markdown-ts--child node "link_label"))
         (destination-node
          (appkit-markup-markdown-ts--child node "link_destination"))
         (direct-p
          (or (string= (treesit-node-type node) "inline_link")
              (and image-p
                   (cl-loop for index below (treesit-node-child-count node)
                            thereis
                            (string= (treesit-node-type
                                      (treesit-node-child node index)) "(")))))
         (destination
          (if direct-p
              (if destination-node
                  (appkit-markup-markdown-ts--destination destination-node)
                "")
            (gethash
             (appkit-markup-markdown-ts--reference-label
              (cond
               (reference-node
                (substring
                 (appkit-markup-markdown-ts--node-source reference-node) 1 -1))
               (label-node
                (appkit-markup-markdown-ts--node-source label-node))
               (t "")))
             appkit-markup-markdown-ts--references)))
         (image (plist-get appkit-markup-markdown-ts--context :image)))
    (if (or (null destination) (and image-p (null image)))
        (progn
          (when image-p
            (appkit-markup-markdown-ts--diagnostic
             'unsupported-markdown-inline node))
          (appkit-markup-markdown-ts--text-nodes
           (appkit-markup-markdown-ts--node-source node)))
      (let ((label (and label-node
                        (appkit-markup-markdown-ts--inline-node
                         label-node (1+ depth))))
            (link (plist-get appkit-markup-markdown-ts--context :link)))
        (list
         (cond
          (image-p
           (funcall image destination (appkit-markup--plain-inlines label)))
          (link (funcall link destination label))
          (t
           (appkit-markup-link
            destination
            (appkit-markup-markdown-ts--link-label label destination)))))))))

(defun appkit-markup-markdown-ts--autolink (node email-p)
  "Convert autolink NODE, prepending mailto when EMAIL-P."
  (let* ((raw (appkit-markup-markdown-ts--node-source node))
         (label
          (if (and (> (length raw) 1)
                   (= (aref raw 0) ?<)
                   (= (aref raw (1- (length raw))) ?>))
              (substring raw 1 -1)
            raw))
         (url (if email-p (concat "mailto:" label) label))
         (children (list (appkit-markup-text label)))
         (link (plist-get appkit-markup-markdown-ts--context :link)))
    (list (if link
              (funcall link url children)
            (appkit-markup-link url children)))))

(defun appkit-markup-markdown-ts--inline-node (node depth)
  "Convert Markdown-inline NODE at DEPTH."
  (appkit-markup-markdown-ts--visit depth)
  (pcase (treesit-node-type node)
    ((or "inline" "link_text" "image_description")
     (appkit-markup-markdown-ts--inline-children node depth))
    ("strong_emphasis"
     (appkit-markup-markdown-ts--add-style
      (appkit-markup-markdown-ts--inline-children node depth) 'bold))
    ("emphasis"
     (appkit-markup-markdown-ts--add-style
      (appkit-markup-markdown-ts--inline-children node depth) 'italic))
    ("strikethrough"
     (appkit-markup-markdown-ts--add-style
      (appkit-markup-markdown-ts--inline-children node depth) 'strike))
    ("code_span" (appkit-markup-markdown-ts--code-span node))
    ("backslash_escape"
     (appkit-markup-markdown-ts--text-nodes
      (appkit-markup-markdown-ts--unescape
       (appkit-markup-markdown-ts--node-source node))))
    ("hard_line_break" (list (appkit-markup-line-break)))
    ((or "inline_link" "full_reference_link"
         "collapsed_reference_link" "shortcut_link")
     (appkit-markup-markdown-ts--inline-link node depth))
    ("image" (appkit-markup-markdown-ts--inline-link node depth t))
    ("uri_autolink" (appkit-markup-markdown-ts--autolink node nil))
    ("email_autolink" (appkit-markup-markdown-ts--autolink node t))
    ((or "emphasis_delimiter" "code_span_delimiter") nil)
    ((or "entity_reference" "numeric_character_reference")
     (appkit-markup-markdown-ts--text-nodes
      (appkit-markup-markdown-ts--node-source node)))
    (_
     (appkit-markup-markdown-ts--diagnostic
      'unsupported-markdown-inline node)
     (appkit-markup-markdown-ts--text-nodes
      (appkit-markup-markdown-ts--node-source node)))))

(defun appkit-markup-markdown-ts--parse-inline (node depth)
  "Parse block grammar inline NODE at DEPTH through markdown-inline."
  (let* ((appkit-markup-markdown-ts--table-cell-p
          (string= (treesit-node-type node) "pipe_table_cell"))
         (ranges
          (if appkit-markup-markdown-ts--table-cell-p
              (save-excursion
                (goto-char (treesit-node-start node))
                (skip-chars-forward " \t" (treesit-node-end node))
                (let ((start (point)))
                  (goto-char (treesit-node-end node))
                  (skip-chars-backward " \t" start)
                  (when (< start (point)) (list (cons start (point))))))
            (appkit-markup-markdown-ts--ranges-excluding
             node "block_continuation"))))
    (if (null ranges)
        nil
      (let ((parser (treesit-parser-create 'markdown-inline)))
        (unwind-protect
            (progn
              (treesit-parser-set-included-ranges parser ranges)
              (let ((appkit-markup-markdown-ts--inline-ranges ranges))
                (appkit-markup-markdown-ts--inline-node
                 (treesit-parser-root-node parser) depth)))
          (treesit-parser-delete parser))))))

(defun appkit-markup-markdown-ts--literal-block (node kind)
  "Return literal visible fallback for unsupported block NODE and KIND."
  (appkit-markup-markdown-ts--diagnostic kind node)
  (let ((children
         (appkit-markup-markdown-ts--text-nodes
          (appkit-markup-markdown-ts--node-source node))))
    (when children (list (appkit-markup-paragraph children)))))

(defun appkit-markup-markdown-ts--heading (node setext-p depth)
  "Convert heading NODE at DEPTH; SETEXT-P selects underline syntax."
  (let* ((children (appkit-markup-markdown-ts--children node))
         (marker
          (cl-find-if
           (lambda (child)
             (if setext-p
                 (string-prefix-p "setext_h" (treesit-node-type child))
               (string-prefix-p "atx_h" (treesit-node-type child))))
           children))
         (paragraph
          (and setext-p
               (appkit-markup-markdown-ts--child node "paragraph")))
         (inline
           (appkit-markup-markdown-ts--child
            (or paragraph node) "inline"))
         (level
          (and marker
               (string-to-number
                (substring (treesit-node-type marker)
                           (if setext-p 8 5)
                           (if setext-p 9 6))))))
    (if (and inline (memq level '(1 2 3 4 5 6)))
        (list
         (appkit-markup-heading
          level
          (appkit-markup-markdown-ts--parse-inline inline (1+ depth))))
      (appkit-markup-markdown-ts--literal-block
       node 'unsupported-markdown-block))))

(defun appkit-markup-markdown-ts--blockquote (node depth)
  "Convert block quote NODE at DEPTH."
  (let ((blocks
         (apply
          #'append
          (mapcar
           (lambda (child)
             (unless (string= (treesit-node-type child) "block_quote_marker")
               (appkit-markup-markdown-ts--blocks child (1+ depth))))
           (appkit-markup-markdown-ts--children node)))))
    (when blocks
      (list (appkit-markup-quote blocks)))))

(defun appkit-markup-markdown-ts--list-marker-p (node)
  "Return non-nil when NODE is list presentation rather than content."
  (or (string-prefix-p "list_marker_" (treesit-node-type node))
      (string-prefix-p "task_list_marker_" (treesit-node-type node))))

(defun appkit-markup-markdown-ts--list (node depth)
  "Convert list NODE at DEPTH."
  (let* ((items
          (appkit-markup-markdown-ts--children-of-type node "list_item"))
         (task-p
          (cl-some
           (lambda (item)
             (cl-some
              (lambda (child)
                (string-prefix-p "task_list_marker_"
                                 (treesit-node-type child)))
              (appkit-markup-markdown-ts--children item)))
           items))
         (marker
          (and items
               (cl-find-if
                (lambda (child)
                  (string-prefix-p "list_marker_"
                                   (treesit-node-type child)))
                (appkit-markup-markdown-ts--children (car items)))))
         (ordered-p
          (and marker
               (member (treesit-node-type marker)
                       '("list_marker_dot" "list_marker_parenthesis"))))
         (start
          (and ordered-p
               (string-to-number
                (appkit-markup-markdown-ts--node-source marker)))))
    (if (or (null items) task-p (and ordered-p (not (> start 0))))
        (appkit-markup-markdown-ts--literal-block
         node 'unsupported-markdown-block)
      (list
       (appkit-markup-list
        (if ordered-p 'ordered 'unordered)
        (mapcar
         (lambda (item)
           (appkit-markup-list-item
            (apply
             #'append
             (mapcar
              (lambda (child)
                (unless (appkit-markup-markdown-ts--list-marker-p child)
                  (appkit-markup-markdown-ts--blocks child (1+ depth))))
              (appkit-markup-markdown-ts--children item)))))
         items)
        :start start)))))

(defun appkit-markup-markdown-ts--fenced-code (node)
  "Convert fenced code block NODE, including a closing fence at EOF."
  (let* ((delimiters
          (appkit-markup-markdown-ts--children-of-type
           node "fenced_code_block_delimiter"))
         (opener (and delimiters
                      (appkit-markup-markdown-ts--node-source
                       (car delimiters))))
         (content
          (appkit-markup-markdown-ts--child node "code_fence_content"))
         (ranges
          (and content
               (appkit-markup-markdown-ts--ranges-excluding
                content "block_continuation")))
         (text (if ranges
                   (appkit-markup-markdown-ts--ranges-text ranges)
                 ""))
         (info (appkit-markup-markdown-ts--child node "info_string"))
         (language
          (and info (appkit-markup-markdown-ts--child info "language"))))
    ;; The grammar pinned by GNU Emacs may absorb a valid closing fence at EOF
    ;; into `code_fence_content' when the source has no final newline.
    (when (and opener (= (length delimiters) 1)
               (string-match
                (format "\n[ ]\\{0,3\\}%c\\{%d,\\}[ \t]*\\'"
                        (aref opener 0) (length opener))
                text))
      (setq text (substring text 0 (match-beginning 0))))
    (list
     (appkit-markup-preformatted
      (string-remove-suffix "\n" text)
      (and language
           (appkit-markup-markdown-ts--unescape
            (appkit-markup-markdown-ts--node-source language)))))))

(defun appkit-markup-markdown-ts--indented-code (node)
  "Convert indented code block NODE."
  (let ((text (appkit-markup-markdown-ts--node-source node)))
    (setq text
          (mapconcat
           (lambda (line)
             (if (string-match "\\` \\{1,4\\}" line)
                 (substring line (min 4 (length (match-string 0 line))))
               line))
           (split-string text "\n" nil) "\n"))
    (list
     (appkit-markup-preformatted
      (string-trim-right text "\n+")))))

(defun appkit-markup-markdown-ts--table (node depth)
  "Convert pipe table NODE at DEPTH through the optional table callback."
  (let ((table (plist-get appkit-markup-markdown-ts--context :table)))
    (if (null table)
        (appkit-markup-markdown-ts--literal-block
         node 'unsupported-markdown-block)
      (let (rows width)
        (dolist (row (appkit-markup-markdown-ts--children node))
          (appkit-markup-markdown-ts--visit (1+ depth))
          (pcase (treesit-node-type row)
            ("pipe_table_delimiter_row" (push nil rows))
            ((or "pipe_table_header" "pipe_table_row")
             (let ((cells (appkit-markup-markdown-ts--children-of-type
                           row "pipe_table_cell"))
                   converted)
               (when (string= (treesit-node-type row) "pipe_table_header")
                 (setq width (length cells)))
               ;; GFM pads short rows and ignores cells beyond the header.
               (dotimes (_ width)
                 (let ((cell (pop cells)))
                   (when cell
                     (appkit-markup-markdown-ts--visit (+ depth 2)))
                   (push (and cell
                              (appkit-markup-markdown-ts--parse-inline
                               cell (+ depth 3)))
                         converted)))
               (push (nreverse converted) rows)))))
        (list (funcall table (nreverse rows)))))))

(defun appkit-markup-markdown-ts--blocks (node depth)
  "Convert block grammar NODE at DEPTH to a block list."
  (appkit-markup-markdown-ts--visit depth)
  (pcase (treesit-node-type node)
    ((or "document" "section")
     (apply
      #'append
      (mapcar
       (lambda (child)
         (appkit-markup-markdown-ts--blocks child (1+ depth)))
       (appkit-markup-markdown-ts--children node))))
    ("paragraph"
     (let ((inline (appkit-markup-markdown-ts--child node "inline")))
       (if inline
           (list
            (appkit-markup-paragraph
             (appkit-markup-markdown-ts--parse-inline inline (1+ depth))))
         (appkit-markup-markdown-ts--literal-block
          node 'unsupported-markdown-block))))
    ("atx_heading"
     (appkit-markup-markdown-ts--heading node nil depth))
    ("setext_heading"
     (appkit-markup-markdown-ts--heading node t depth))
    ("block_quote"
     (appkit-markup-markdown-ts--blockquote node depth))
    ("list" (appkit-markup-markdown-ts--list node depth))
    ("fenced_code_block"
     (appkit-markup-markdown-ts--fenced-code node))
    ("indented_code_block"
     (appkit-markup-markdown-ts--indented-code node))
    ("pipe_table" (appkit-markup-markdown-ts--table node depth))
    ("link_reference_definition" nil)
    ((or "block_quote_marker" "block_continuation"
         "list_marker_dot" "list_marker_parenthesis"
         "list_marker_minus" "list_marker_plus" "list_marker_star")
     nil)
    (_
     (appkit-markup-markdown-ts--literal-block
      node 'unsupported-markdown-block))))

(defun appkit-markup-markdown-ts--parse (source context)
  "Parse property-free protected Markdown SOURCE into an Appkit result.
CONTEXT may supply :image (URL ALT) and :table (ROWS) callbacks returning one
semantic inline node or block respectively.  Table cells contain inline nodes;
a nil row denotes the header separator.  Missing callbacks preserve literals.
An optional :link (URL CHILDREN) callback receives semantic inline children
before the default text-only link-label conversion, preserving linked images."
  (appkit-markup-markdown-ts--ensure-ready)
  (let ((appkit-markup-markdown-ts--diagnostics nil)
        (appkit-markup-markdown-ts--diagnostic-count 0)
        (appkit-markup-markdown-ts--node-count 0)
        (appkit-markup-markdown-ts--context context)
        (appkit-markup-markdown-ts--references (make-hash-table :test #'equal)))
    (with-temp-buffer
      (insert source)
      (let ((parser (treesit-parser-create 'markdown)))
        (unwind-protect
            (let ((root (treesit-parser-root-node parser)))
              (appkit-markup-markdown-ts--collect-references root 0)
              (when (treesit-node-check root 'has-error)
                (appkit-markup-markdown-ts--diagnostic
                 'markdown-syntax-error root))
              (appkit-markup-parse-result
               (appkit-markup-document
                (appkit-markup-markdown-ts--blocks root 0))
               :diagnostics
               (nreverse appkit-markup-markdown-ts--diagnostics)))
          (treesit-parser-delete parser))))))

(provide 'appkit-markup-markdown-ts)

;;; appkit-markup-markdown-ts.el ends here
