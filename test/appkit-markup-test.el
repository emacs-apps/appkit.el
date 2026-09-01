;;; appkit-markup-test.el --- Semantic markup value tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit-markup)

(ert-deftest appkit-markup-normalizes-owned-text-and-styles ()
  (let* ((source (propertize "hello" 'secret "value"))
         (document
          (appkit-markup-document
           (list
            (appkit-markup-paragraph
             (list (appkit-markup-text "" '(bold))
                   (appkit-markup-text source '(code bold code))
                   (appkit-markup-text " world" '(bold code)))))))
         (paragraph (car (appkit-markup-document-blocks document)))
         (text (car (appkit-markup-paragraph-children paragraph))))
    (should (equal (appkit-markup-text-text text) "hello world"))
    (should (equal (appkit-markup-text-styles text) '(bold code)))
    (should-not
     (text-property-not-all
      0 (length (appkit-markup-text-text text)) 'secret nil
      (appkit-markup-text-text text)))
    (put-text-property 0 1 'changed t source)
    (should-not (get-text-property 0 'changed
                                   (appkit-markup-text-text text)))))

(ert-deftest appkit-markup-removes-empty-containers-and-keeps-empty-list-item ()
  (let* ((document
          (appkit-markup-document
           (list
            (appkit-markup-paragraph nil)
            (appkit-markup-quote nil)
            (appkit-markup-list 'unordered nil)
            (appkit-markup-list
             'unordered
             (list (appkit-markup-list-item nil))))))
         (blocks (appkit-markup-document-blocks document)))
    (should (= (length blocks) 1))
    (should (appkit-markup-list-p (car blocks)))
    (should (null (appkit-markup-list-item-blocks
                   (car (appkit-markup-list-items (car blocks))))))))

(ert-deftest appkit-markup-rejects-invalid-tree-with-secret-free-condition ()
  (let* ((secret "DO-NOT-REPORT")
         (object
          (appkit-markup-object
           secret (list (appkit-markup-text "fallback"))))
         (link
          (appkit-markup-link "https://example.test" (list object)))
         (paragraph
          (appkit-markup-paragraph
           (list (appkit-markup-text secret)
                 (appkit-markup-line-break)
                 link))))
    (condition-case condition
        (progn
          (appkit-markup-document (list paragraph))
          (ert-fail "Invalid link label was accepted"))
      (appkit-markup-invalid
       (should-not (string-match-p secret (format "%S" condition)))))))

(ert-deftest appkit-markup-values-are-read-only-and-cycles-still-rejected ()
  (let ((paragraph (appkit-markup-paragraph nil)))
    ;; Keep the invalid generalized-variable expansion at runtime so package
    ;; compilation can build this negative test before ERT asserts the error.
    (should-error
     (eval
      `(setf (appkit-markup-paragraph-children ',paragraph)
             (list (appkit-markup-text "changed"))))))
  ;; Deliberate representation corruption remains rejected at the public
  ;; document boundary even though public accessors expose no setters.
  (let* ((blocks (list nil))
         (quote (appkit-markup--quote-create :blocks blocks)))
    (setcar blocks quote)
    (should-error (appkit-markup-document (list quote))
                  :type 'appkit-markup-invalid)))

(ert-deftest appkit-markup-semantic-equality-controls-opaque-values ()
  (let* ((left-value (list :id 7))
         (right-value (copy-tree left-value))
         (left
          (appkit-markup-document
           (list
            (appkit-markup-paragraph
             (list
              (appkit-markup-object
               left-value (list (appkit-markup-text "Ada"))))))))
         (right
          (appkit-markup-document
           (list
            (appkit-markup-paragraph
             (list
              (appkit-markup-object
               right-value (list (appkit-markup-text "Ada")))))))))
    (should-not (appkit-markup-equal-p left right))
    (should (appkit-markup-equal-p left right #'equal))))

(ert-deftest appkit-markup-plain-text-exports-structure-and-fallbacks ()
  (let ((document
         (appkit-markup-document
          (list
           (appkit-markup-heading 2 (list (appkit-markup-text "Title")))
           (appkit-markup-quote
            (list (appkit-markup-paragraph
                   (list (appkit-markup-text "quoted")))))
           (appkit-markup-list
            'ordered
            (list
             (appkit-markup-list-item
              (list (appkit-markup-paragraph
                     (list (appkit-markup-text "first")))))
             (appkit-markup-list-item
              (list (appkit-markup-object-block
                     'provider
                     (list (appkit-markup-paragraph
                            (list (appkit-markup-text "fallback"))))))))
            :start 3)))))
    (should (equal (appkit-markup-plain-text document)
                   "Title\n> quoted\n3. first\n4. fallback"))))

(provide 'appkit-markup-test)

;;; appkit-markup-test.el ends here
