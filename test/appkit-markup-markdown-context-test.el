;;; appkit-markup-markdown-context-test.el --- Markdown AST callbacks -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit-markup-codecs)

(defun appkit-markup-markdown-context-test--document (source &optional context)
  "Parse SOURCE with optional presentation CONTEXT."
  (appkit-markup-parse-result-document
   (appkit-markup-parse 'markdown source :context context)))

(defun appkit-markup-markdown-context-test--image (url alt)
  "Represent an image URL with visible ALT in the semantic document."
  (appkit-markup-object (list :image url)
                        (list (appkit-markup-text alt))))

(ert-deftest appkit-markup-markdown-references-normalize-and-first-definition-wins ()
  (let* ((document
          (appkit-markup-markdown-context-test--document
           (concat "[full][ STRASSE\t label ] [collapsed][] [shortcut]\n\n"
                   "> [Straße label]: <first\\(image\\).png>\n\n"
                   "[STRASSE label]: second.png\n"
                   "[collapsed]: collapse\n[shortcut]: short\n")))
         (paragraph (car (appkit-markup-document-blocks document)))
         (links (cl-remove-if-not #'appkit-markup-link-p
                                 (appkit-markup-paragraph-children paragraph))))
    (should (equal (mapcar #'appkit-markup-link-url links)
                   '("first(image).png" "collapse" "short")))
    (should (equal (mapcar (lambda (link)
                            (appkit-markup--plain-inlines
                             (appkit-markup-link-children link)))
                          links)
                   '("full" "collapsed" "shortcut")))
    (should (equal (appkit-markup-plain-text document)
                   "full collapsed shortcut"))))
 
(ert-deftest appkit-markup-markdown-reference-casefold-keeps-distinct-unicode-labels ()
  (let* ((document
          (appkit-markup-markdown-context-test--document
           "[I] [ı] [İ]\n\n[i]: ascii\n[ı]: dotless\n[i̇]: dotted\n"))
         (children (appkit-markup-paragraph-children
                    (car (appkit-markup-document-blocks document)))))
    (should (equal (mapcar #'appkit-markup-link-url
                          (cl-remove-if-not #'appkit-markup-link-p children))
                   '("ascii" "dotless" "dotted")))))

(ert-deftest appkit-markup-markdown-reference-looking-code-is-not-a-definition ()
  (let* ((document
          (appkit-markup-markdown-context-test--document
           (concat "[missing] ![missing] [valid]\n\n"
                   "`[valid] ![valid]`\n\n"
                   "```markdown\n[missing]: forbidden\n![valid]\n```\n\n"
                   "    [missing]: also-forbidden\n\n"
                   "[valid]: allowed\n")
           (list :image #'appkit-markup-markdown-context-test--image)))
         (blocks (appkit-markup-document-blocks document))
         (children (appkit-markup-paragraph-children (car blocks))))
    (should (equal (appkit-markup--plain-inlines children)
                   "[missing] ![missing] valid"))
    (should (equal (mapcar #'appkit-markup-link-url
                          (cl-remove-if-not #'appkit-markup-link-p children))
                   '("allowed")))
    (should-not (cl-some #'appkit-markup-object-p children))
    (let ((code (car (appkit-markup-paragraph-children (nth 1 blocks)))))
      (should (equal (appkit-markup-text-text code) "[valid] ![valid]"))
      (should (memq 'code (appkit-markup-text-styles code))))
    (should (equal (appkit-markup-preformatted-text (nth 2 blocks))
                   "[missing]: forbidden\n![valid]"))
    (should (equal (appkit-markup-preformatted-text (nth 3 blocks))
                   "[missing]: also-forbidden"))))

(ert-deftest appkit-markup-markdown-images-use-semantic-alt-and-reference-destinations ()
  (let* ((document
          (appkit-markup-markdown-context-test--document
           (concat "![a **bold** `code`](<direct.png>) ![full][PIC]\n"
                   "![collapsed][] ![shortcut] ![]()\n\n"
                   "[pic]: full.png\n[collapsed]: collapsed.png\n"
                   "[shortcut]: shortcut.png\n")
           (list :image #'appkit-markup-markdown-context-test--image)))
         (images
          (cl-remove-if-not
           #'appkit-markup-object-p
           (appkit-markup-paragraph-children
            (car (appkit-markup-document-blocks document))))))
    (should (equal (mapcar (lambda (image)
                            (plist-get (appkit-markup-object-value image) :image))
                          images)
                   '("direct.png" "full.png" "collapsed.png" "shortcut.png" "")))
    (should (equal (mapcar (lambda (image)
                            (appkit-markup--plain-inlines
                             (appkit-markup-object-fallback image)))
                          images)
                   '("a bold code" "full" "collapsed" "shortcut" "")))))

(ert-deftest appkit-markup-markdown-linked-images-retain-rich-labels ()
  (let* ((document
          (appkit-markup-markdown-context-test--document
           "[![Build][badge]][target]\n\n[badge]: badge.svg\n[target]: /status\n"
           (list :image #'appkit-markup-markdown-context-test--image
                 :link (lambda (url children)
                         (appkit-markup-object (list :link url :children children)
                                               children)))))
         (link (car (appkit-markup-paragraph-children
                     (car (appkit-markup-document-blocks document)))))
         (value (appkit-markup-object-value link))
         (image (car (plist-get value :children))))
    (should (equal (plist-get value :link) "/status"))
    (should (equal (appkit-markup-object-value image) '(:image "badge.svg")))
    (should (equal (appkit-markup-plain-text document) "Build"))))

(ert-deftest appkit-markup-markdown-pipe-tables-convert-inline-cells-and-row-boundaries ()
  (let* ((document
          (appkit-markup-markdown-context-test--document
           (concat "| **Name** | Value |\n| :--- | ---: |\n"
                   "| a \\| b | `c\\|d` | ignored |\n"
                   "| [site][id] | ![icon](icon.png) |\n"
                   "| short |\n| | |\n\n[id]: /site\n")
           (list :image #'appkit-markup-markdown-context-test--image
                 :table (lambda (rows)
                          (appkit-markup-object-block
                           rows (list (appkit-markup-paragraph
                                       (list (appkit-markup-text "table")))))))))
         (rows (appkit-markup-object-block-value
                (car (appkit-markup-document-blocks document)))))
    (should (equal (mapcar (lambda (row)
                            (and row (mapcar #'appkit-markup--plain-inlines row)))
                          rows)
                   '(("Name" "Value") nil ("a | b" "c|d")
                     ("site" "icon") ("short" "") ("" ""))))
    (should (memq 'bold (appkit-markup-text-styles (caaar rows))))
    (should (memq 'code (appkit-markup-text-styles (car (cadr (nth 2 rows))))))
    (should (equal (appkit-markup-link-url (car (car (nth 3 rows)))) "/site"))
    (should (equal (appkit-markup-object-value (car (cadr (nth 3 rows))))
                   '(:image "icon.png")))))

(ert-deftest appkit-markup-markdown-without-presentation-callbacks-preserves-literals ()
  (let* ((source "![alt](img.png)\n\n| h |\n| --- |\n| v |\n")
         (document (appkit-markup-markdown-context-test--document source))
         (blocks (appkit-markup-document-blocks document)))
    (should (equal (appkit-markup--plain-inlines
                    (appkit-markup-paragraph-children (car blocks)))
                   "![alt](img.png)"))
    (should (equal (appkit-markup--plain-inlines
                    (appkit-markup-paragraph-children (cadr blocks)))
                   "| h |\n| --- |\n| v |\n"))))

(ert-deftest appkit-markup-markdown-reference-index-and-table-cells-remain-bounded ()
  (let ((appkit-markup-markdown-ts-node-limit 5))
    (should-error
     (appkit-markup-markdown-context-test--document
      "[a]: /a\n[b]: /b\n[c]: /c\n[d]: /d\n")
     :type 'appkit-markup-codec-error))
  (let ((appkit-markup-markdown-ts-node-limit 15))
    (should-error
     (appkit-markup-markdown-context-test--document
      "| a | b | c | d |\n| --- | --- | --- | --- |\n| e | f | g | h |\n"
      (list :table (lambda (rows)
                     (appkit-markup-object-block
                      rows (list (appkit-markup-paragraph
                                  (list (appkit-markup-text "table"))))))))
     :type 'appkit-markup-codec-error)))

(provide 'appkit-markup-markdown-context-test)
;;; appkit-markup-markdown-context-test.el ends here
