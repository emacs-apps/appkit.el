;;; appkit-markup-codec-test.el --- Markup codec contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit-markup-compose)

(defun appkit-markup-codec-test--document ()
  "Return a document covered by both rich built-in codecs."
  (appkit-markup-document
   (list
    (appkit-markup-heading 2 (list (appkit-markup-text "Heading")))
    (appkit-markup-paragraph
     (list (appkit-markup-text "bold" '(bold))
           (appkit-markup-text " and ")
           (appkit-markup-text "italic" '(italic))
           (appkit-markup-text " plus ")
           (appkit-markup-link
            "https://example.test/a_(b)" (list (appkit-markup-text "link")))))
    (appkit-markup-quote
     (list (appkit-markup-paragraph
            (list (appkit-markup-text "quoted")
                  (appkit-markup-line-break)
                  (appkit-markup-text "line")))))
    (appkit-markup-list
     'ordered
     (list
      (appkit-markup-list-item
       (list
        (appkit-markup-paragraph (list (appkit-markup-text "first")))
        (appkit-markup-list
         'unordered
         (list
          (appkit-markup-list-item
           (list (appkit-markup-paragraph
                  (list (appkit-markup-text "nested")))))))))
      (appkit-markup-list-item
       (list (appkit-markup-paragraph (list (appkit-markup-text "second"))))))
     :start 3)
    (appkit-markup-preformatted "(message \"safe\")" "elisp"))))

(ert-deftest appkit-markup-builtins-round-trip-declared-rich-semantics ()
  (let ((document (appkit-markup-codec-test--document)))
    (dolist (codec '(org markdown))
      (let* ((printed (appkit-markup-print codec document))
             (parsed (appkit-markup-parse
                      codec (appkit-markup-print-result-source printed))))
        (should-not (appkit-markup-print-result-losses printed))
        (should
         (appkit-markup-equal-p
          document (appkit-markup-parse-result-document parsed)))))))

(ert-deftest appkit-markup-markdown-printer-preserves-literal-block-markers ()
  (dolist (text '("# title" "> quoted" "- item" "1. item" "```elisp"))
    (let* ((document
            (appkit-markup-document
             (list (appkit-markup-paragraph
                    (list (appkit-markup-text text))))))
           (printed (appkit-markup-print 'markdown document))
           (parsed
            (appkit-markup-parse
             'markdown (appkit-markup-print-result-source printed))))
      (should-not (appkit-markup-print-result-losses printed))
      (should
       (appkit-markup-equal-p
        document (appkit-markup-parse-result-document parsed))))))

(ert-deftest appkit-markup-markdown-printer-preserves-nested-style-runs ()
  (let* ((document
          (appkit-markup-parse-result-document
           (appkit-markup-parse 'markdown "**bold _italic_**")))
         (printed (appkit-markup-print 'markdown document))
         (parsed
          (appkit-markup-parse
           'markdown (appkit-markup-print-result-source printed))))
    (should (equal (appkit-markup-print-result-source printed)
                   "**bold *italic***"))
    (should
     (appkit-markup-equal-p
      document (appkit-markup-parse-result-document parsed)))))

(ert-deftest appkit-markup-markdown-printer-preserves-link-backslashes ()
  (let* ((document
          (appkit-markup-document
           (list
            (appkit-markup-paragraph
             (list
              (appkit-markup-link
               "https://example.test/a\\b"
               (list (appkit-markup-text "link"))))))))
         (printed (appkit-markup-print 'markdown document))
         (parsed
          (appkit-markup-parse
           'markdown (appkit-markup-print-result-source printed))))
    (should-not (appkit-markup-print-result-losses printed))
    (should
     (appkit-markup-equal-p
      document (appkit-markup-parse-result-document parsed)))))

(ert-deftest appkit-markup-plain-reports-paragraph-boundary-loss ()
  (let* ((document
          (appkit-markup-document
           (list
            (appkit-markup-paragraph (list (appkit-markup-text "one")))
            (appkit-markup-paragraph (list (appkit-markup-text "two"))))))
         (printed (appkit-markup-print 'plain document)))
    (should
     (seq-some
      (lambda (loss)
        (eq (appkit-markup-loss-kind loss) 'block-boundary))
      (appkit-markup-print-result-losses printed)))))

(ert-deftest appkit-markup-org-printer-detects-indented-source-terminator ()
  (let* ((document
          (appkit-markup-document
           (list
            (appkit-markup-preformatted
             "before\n  #+end_src\nafter" "text"))))
         (printed (appkit-markup-print 'org document)))
    (should
     (seq-some
      (lambda (loss)
        (eq (appkit-markup-loss-kind loss) 'preformatted))
      (appkit-markup-print-result-losses printed)))))

(ert-deftest appkit-markup-codecs-parse-nested-inline-syntax ()
  (let* ((markdown
          (appkit-markup-parse
           'markdown "**bold and _italic_** [label](https://example.test)"))
         (org
          (appkit-markup-parse
           'org "*bold and /italic/* [[https://example.test][label]]")))
    (dolist (result (list markdown org))
      (let* ((paragraph
              (car (appkit-markup-document-blocks
                    (appkit-markup-parse-result-document result))))
             (children (appkit-markup-paragraph-children paragraph)))
        (should (seq-some #'appkit-markup-link-p children))
        (should
         (seq-some
          (lambda (node)
            (and (appkit-markup-text-p node)
                 (memq 'italic (appkit-markup-text-styles node))))
          children))))))

(ert-deftest appkit-markup-unclosed-fence-is-semantic-code ()
  (let* ((result
          (appkit-markup-parse
           'markdown "```elisp\n(message \"not closed\")"))
         (block
          (car
           (appkit-markup-document-blocks
            (appkit-markup-parse-result-document result)))))
    (should (appkit-markup-preformatted-p block))
    (should (equal (appkit-markup-preformatted-language block) "elisp"))
    (should (equal (appkit-markup-preformatted-text block)
                   "(message \"not closed\")"))))

(ert-deftest appkit-markup-markdown-closing-fence-at-eof-is-not-content ()
  (let* ((result
          (appkit-markup-parse 'markdown "```text\nbody\n```"))
         (block
          (car
           (appkit-markup-document-blocks
            (appkit-markup-parse-result-document result)))))
    (should (appkit-markup-preformatted-p block))
    (should (equal (appkit-markup-preformatted-language block) "text"))
    (should (equal (appkit-markup-preformatted-text block) "body"))))

(ert-deftest appkit-markup-markdown-tree-sitter-grammars-are-required ()
  (cl-letf (((symbol-function 'appkit-markup-markdown-ts-ready-p)
             (lambda () nil)))
    (should-error
     (appkit-markup-parse 'markdown "body")
     :type 'appkit-markup-codec-error)))

(ert-deftest appkit-markup-markdown-tree-sitter-adapter-is-bounded ()
  (let ((appkit-markup-markdown-ts-node-limit 1))
    (should-error
     (appkit-markup-parse 'markdown "# heading\n")
     :type 'appkit-markup-codec-error)))

(ert-deftest appkit-markup-markdown-tree-sitter-block-semantics ()
  (let* ((result
          (appkit-markup-parse
           'markdown "Setext\n======\n\n    indented\n"))
         (blocks
          (appkit-markup-document-blocks
           (appkit-markup-parse-result-document result))))
    (should (appkit-markup-heading-p (nth 0 blocks)))
    (should (appkit-markup-preformatted-p (nth 1 blocks)))
    (should (equal (appkit-markup-preformatted-text (nth 1 blocks))
                   "indented"))))

(ert-deftest appkit-markup-markdown-unsupported-block-is-literal ()
  (dotimes (_ 2)
    (let* ((result (appkit-markup-parse 'markdown "---\n"))
           (diagnostics (appkit-markup-parse-result-diagnostics result))
           (diagnostic
            (seq-find
             (lambda (item)
               (eq (appkit-markup-diagnostic-kind item)
                   'unsupported-markdown-block))
             diagnostics)))
      (should (equal (appkit-markup-plain-text
                      (appkit-markup-parse-result-document result))
                     "---\n"))
      (should diagnostic)
      (should (= (appkit-markup-diagnostic-start diagnostic) 0))
      (should (= (appkit-markup-diagnostic-end diagnostic) 4)))))

(ert-deftest appkit-markup-plain-codec-reports-semantic-loss ()
  (let* ((document (appkit-markup-codec-test--document))
         (printed (appkit-markup-print 'plain document)))
    (should (string-match-p "Heading" (appkit-markup-print-result-source printed)))
    (should (seq-some
             (lambda (loss) (eq (appkit-markup-loss-kind loss) 'heading))
             (appkit-markup-print-result-losses printed)))))

(ert-deftest appkit-markup-structured-objects-remain-distinct-occurrences ()
  (let* ((payload (list :kind 'mention :id "7"))
         (source
          (concat (appkit-chatbuf-input-object-string "@Ada" payload)
                  (appkit-chatbuf-input-object-string "@Ada" payload)))
         (result (appkit-markup-parse 'markdown source))
         (paragraph
          (car (appkit-markup-document-blocks
                (appkit-markup-parse-result-document result))))
         (objects
          (seq-filter #'appkit-markup-object-p
                      (appkit-markup-paragraph-children paragraph))))
    (should (= (length objects) 2))
    (should (eq (appkit-markup-object-value (nth 0 objects)) payload))
    (should (eq (appkit-markup-object-value (nth 1 objects)) payload))))

(ert-deftest appkit-markup-side-channel-removes-whole-object-occurrence ()
  (let* ((payload (list :kind 'attachment :id "9"))
         (source (appkit-chatbuf-input-object-string "file" payload))
         (result
          (appkit-markup-parse
           'markdown source
           :object-classifier
           (lambda (_value _text) '(side-channel . attachments)))))
    (should-not
     (appkit-markup-document-blocks
      (appkit-markup-parse-result-document result)))
    (should (eq
             (appkit-markup-object-occurrence-value
              (car (alist-get 'attachments
                              (appkit-markup-parse-result-side-channels result))))
             payload))))

(ert-deftest appkit-markup-object-printer-restores-provider-source ()
  (let* ((payload (list :kind 'mention :wire "@**Ada|7**"))
         (source (appkit-chatbuf-input-object-string "@Ada" payload))
         (document
          (appkit-markup-parse-result-document
           (appkit-markup-parse 'markdown source)))
         (printed
          (appkit-markup-print
           'markdown document
           :object-printer
           (lambda (node)
             (plist-get (appkit-markup-object-value node) :wire)))))
    (should (equal (appkit-markup-print-result-source printed)
                   "@**Ada|7** "))
    (should-not (appkit-markup-print-result-losses printed))))

(ert-deftest appkit-markup-compose-capture-drives-preview-and-output ()
  (with-temp-buffer
    (insert "**bold**")
    (appkit-compose-setup)
    (appkit-markup-compose-setup
     :codecs '(markdown org plain)
     :active-codec 'markdown)
    (let* ((capture (appkit-markup-compose-capture))
           (generation (appkit-markup-compose-capture-generation capture))
           (output (appkit-markup-compose-output capture 'org)))
      (should (eq (appkit-markup-compose-capture-codec capture)
                  'markdown))
      (should (= generation (appkit-markup-compose-output-generation output)))
      (should (equal (appkit-markup-compose-output-source output) "*bold*"))
      (with-temp-buffer
        (appkit-markup-compose-preview capture :final-newline-p nil)
        (should (equal (buffer-string) "bold"))))))

(ert-deftest appkit-markup-compose-prefix-selection-matches-telega ()
  (with-temp-buffer
    (appkit-compose-setup)
    (appkit-markup-compose-setup
     :codecs '(markdown org plain)
     :active-codec 'markdown)
    (should (eq (appkit-markup-compose-select-codec nil) 'markdown))
    (should (eq (appkit-markup-compose-select-codec '(4)) 'org))
    (should (eq (appkit-markup-compose-select-codec '(16)) 'plain))
    (should-error (appkit-markup-compose-select-codec '(64))
                  :type 'user-error)))

(ert-deftest appkit-markup-org-parser-uses-org-element-without-mode-hooks ()
  (let ((org-mode-hook (list (lambda () (ert-fail "org-mode-hook ran"))))
        (org-babel-after-execute-hook
         (list (lambda () (ert-fail "Babel hook ran")))))
    (let* ((source "* Heading\n\n| a | b |\n| 1 | 2 |")
           (result (appkit-markup-parse 'org source))
           (document (appkit-markup-parse-result-document result)))
      (should (appkit-markup-heading-p
               (car (appkit-markup-document-blocks document))))
      (should (string-match-p
               (regexp-quote "| a | b |")
               (appkit-markup-plain-text document)))
      (should
       (seq-some
        (lambda (diagnostic)
          (eq (appkit-markup-diagnostic-kind diagnostic)
              'unsupported-org-block))
        (appkit-markup-parse-result-diagnostics result))))))

(ert-deftest appkit-markup-compose-active-codec-is-semantic-state ()
  (with-temp-buffer
    (appkit-compose-setup)
    (appkit-markup-compose-setup
     :codecs '(markdown org) :active-codec 'markdown)
    (let ((generation (appkit-compose-generation)))
      (appkit-markup-compose-set-active-codec 'org)
      (should (= (appkit-compose-generation) (1+ generation)))
      (appkit-markup-compose-set-active-codec 'org)
      (should (= (appkit-compose-generation) (1+ generation))))))

(ert-deftest appkit-markup-compose-freezes-context-at-capture ()
  (let (print-context)
    (appkit-markup-register-codec
     'appkit-test-context
     :parse (lambda (source _context)
              (appkit-markup-parse-result
               (appkit-markup-document
                (list (appkit-markup-paragraph
                       (list (appkit-markup-text source)))))))
     :print (lambda (document context)
              (setq print-context context)
              (appkit-markup-print-result
               (appkit-markup-plain-text document))))
    (with-temp-buffer
      (insert "body")
      (appkit-compose-setup)
      (appkit-markup-compose-setup
       :codecs '(markdown)
       :context-function (lambda () 'captured))
      (let ((capture (appkit-markup-compose-capture)))
        (setq-local appkit-markup-compose-context-function
                    (lambda () 'changed))
        (appkit-markup-compose-output capture 'appkit-test-context)
        (should (eq print-context 'captured))))))

(provide 'appkit-markup-codec-test)
;;; appkit-markup-codec-test.el ends here
