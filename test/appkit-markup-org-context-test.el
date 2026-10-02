;;; appkit-markup-org-context-test.el --- Org presentation semantics -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit-markup-codecs)

(ert-deftest appkit-markup-org-context-table-preserves-cell-semantics ()
  (let* ((source (concat "| *Name* | [[https://example.test][Site]] |\n"
                         "|--------+--------------------------------|\n"
                         "| /value/ | ~literal~ |\n"
                         "| | [[file:docs/guide.org][Guide]] |"))
         (document
          (appkit-markup-parse-result-document
           (appkit-markup-parse
            'org source
            :context (list :table
                           (lambda (rows)
                             (appkit-markup-object-block rows nil))))))
         (table (car (appkit-markup-document-blocks document))))
    (should (appkit-markup-object-block-p table))
    (should
     (equal
      (appkit-markup-object-block-value table)
      (list
       (list (list (appkit-markup-text "Name" '(bold)))
             (list (appkit-markup-link
                    "https://example.test" (list (appkit-markup-text "Site")))))
       nil
       (list (list (appkit-markup-text "value" '(italic)))
             (list (appkit-markup-text "literal" '(code))))
       (list nil
             (list (appkit-markup-link
                    "file:docs/guide.org" (list (appkit-markup-text "Guide"))))))))))

(ert-deftest appkit-markup-org-context-images-exclude-links-and-literal-code ()
  (let* ((source
          (concat "[[file:images/screen.png]]\n\n"
                  "[[http://example.test/icon.svg?raw=1]]\n\n"
                  "[[https://example.test/photo.jpg]]\n\n"
                  "[[file:images/screen.png][*Screenshot*]]\n\n"
                  "[[file:docs/guide.org]]\n\n"
                  "~[[file:images/code.png]]~\n\n"
                  "#+begin_src org\n[[file:images/source.png]]\n"
                  "| source | table |\n#+end_src"))
         (document
          (appkit-markup-parse-result-document
           (appkit-markup-parse
            'org source
            :context
            (list :image
                  (lambda (url alt)
                    (appkit-markup-object
                     url (list (appkit-markup-text alt))))
                  :table (lambda (_rows)
                           (ert-fail "Source table was interpreted"))))))
         (blocks (appkit-markup-document-blocks document)))
    (cl-loop for block in (seq-take blocks 3)
             for url in '("file:images/screen.png"
                          "http://example.test/icon.svg?raw=1"
                          "https://example.test/photo.jpg")
             do (let ((image (car (appkit-markup-paragraph-children block))))
                  (should (appkit-markup-object-p image))
                  (should (equal (appkit-markup-object-value image) url))
                  (should (equal (appkit-markup-object-fallback image)
                                 (list (appkit-markup-text url))))))
    (should
     (equal (appkit-markup-paragraph-children (nth 3 blocks))
            (list (appkit-markup-link
                   "file:images/screen.png"
                   (list (appkit-markup-text "Screenshot" '(bold)))))))
    (should
     (equal (appkit-markup-paragraph-children (nth 4 blocks))
            (list (appkit-markup-link
                   "file:docs/guide.org"
                   (list (appkit-markup-text "file:docs/guide.org"))))))
    (should
     (equal (appkit-markup-paragraph-children (nth 5 blocks))
            (list (appkit-markup-text "[[file:images/code.png]]" '(code)))))
    (should (appkit-markup-preformatted-p (nth 6 blocks)))
    (should (equal (appkit-markup-preformatted-text (nth 6 blocks))
                   "[[file:images/source.png]]\n| source | table |"))))

(ert-deftest appkit-markup-org-context-absence-retains-safe-source ()
  (let* ((source "[[file:image.png]]\n\n| *unparsed* | [[file:cell.png]] |")
         (result (appkit-markup-parse 'org source))
         (document (appkit-markup-parse-result-document result))
         (blocks (appkit-markup-document-blocks document)))
    (should (appkit-markup-link-p
             (car (appkit-markup-paragraph-children (car blocks)))))
    (should (equal (appkit-markup-plain-text document)
                   "file:image.png\n| *unparsed* | [[file:cell.png]] |"))
    (should (eq (appkit-markup-diagnostic-kind
                 (car (appkit-markup-parse-result-diagnostics result)))
                'unsupported-org-block))))

(ert-deftest appkit-markup-org-context-anchors-preserve-explicit-identifiers ()
  (let* ((document
          (appkit-markup-parse-result-document
           (appkit-markup-parse
            'org
            (concat "* *Heading*\n:PROPERTIES:\n:CUSTOM_ID: exact-ID\n:END:\n"
                    "Before <<local-target>> after.\n\n"
                    "~<<literal-target>>~")
            :context
            (list :anchor (lambda (id) (appkit-markup-object id nil))))))
         (blocks (appkit-markup-document-blocks document))
         (heading (appkit-markup-heading-children (car blocks)))
         (paragraphs (seq-filter #'appkit-markup-paragraph-p blocks))
         (target-paragraph
          (seq-find
           (lambda (block)
             (seq-some #'appkit-markup-object-p
                       (appkit-markup-paragraph-children block)))
           paragraphs)))
    (should (equal (appkit-markup-object-value (car heading)) "exact-ID"))
    (should (equal (cdr heading) (list (appkit-markup-text "Heading" '(bold)))))
    (should
     (equal
      (appkit-markup-paragraph-children target-paragraph)
      (list (appkit-markup-text "Before ")
            (appkit-markup-object "local-target" nil)
            (appkit-markup-text " after."))))
    (should
     (equal
      (appkit-markup-paragraph-children (car (last paragraphs)))
      (list (appkit-markup-text "<<literal-target>>" '(code)))))))

(provide 'appkit-markup-org-context-test)
;;; appkit-markup-org-context-test.el ends here
