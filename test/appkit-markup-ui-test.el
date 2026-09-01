;;; appkit-markup-ui-test.el --- Native markup insertion tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit-markup-ui)

(defun appkit-markup-ui-test--document ()
  "Return one representative semantic document."
  (appkit-markup-document
   (list
    (appkit-markup-heading 2 (list (appkit-markup-text "Heading")))
    (appkit-markup-paragraph
     (list (appkit-markup-text "Bold" '(bold))
           (appkit-markup-text " and ")
           (appkit-markup-link
            "https://example.test" (list (appkit-markup-text "link")))))
    (appkit-markup-quote
     (list (appkit-markup-paragraph (list (appkit-markup-text "quote")))))
    (appkit-markup-list
     'unordered
     (list (appkit-markup-list-item
            (list (appkit-markup-paragraph
                   (list (appkit-markup-text "item")))))))
    (appkit-markup-preformatted "code" "elisp"))))

(ert-deftest appkit-markup-ui-inserts-native-faces-prefix-and-actions ()
  (with-temp-buffer
    (let ((called 0)
          (document (appkit-markup-ui-test--document)))
      (pcase-let ((`(,start . ,end)
                   (appkit-markup-ui-insert-document
                    document
                    :prefix "    "
                    :properties '(row-id 7)
                    :interactive-p t
                    :link-action
                    (lambda (url)
                      (should (equal url "https://example.test"))
                      (lambda () (setq called (1+ called)))))))
        (should (= start (point-min)))
        (should (= end (point-max)))
        (should (equal (buffer-string)
                       "Heading\nBold and link\nquote\nitem\ncode\n"))
        (should (equal (get-text-property start 'row-id) 7))
        (should (equal (get-text-property start 'line-prefix) "    "))
        (goto-char (point-min))
        (search-forward "Bold")
        (should (memq 'bold
                      (let ((face (get-text-property (1- (point)) 'face)))
                        (if (listp face) face (list face)))))
        (goto-char (point-min))
        (search-forward "link")
        (should (functionp
                 (get-text-property (1- (point)) appkit-ui-action-property)))
        (appkit-ui-activate-at (1- (point)))
        (should (= called 1))
        (goto-char (point-min))
        (search-forward "quote")
        (should (string-match-p
                 "│" (format "%s" (get-text-property
                                     (1- (point)) 'line-prefix))))
        (goto-char (point-min))
        (search-forward "item")
        (should (string-match-p
                 "•" (format "%s" (get-text-property
                                     (1- (point)) 'line-prefix))))))))

(ert-deftest appkit-markup-ui-preview-never-calls-client-callbacks ()
  (with-temp-buffer
    (let ((document
           (appkit-markup-document
            (list
             (appkit-markup-paragraph
              (list
               (appkit-markup-link
                "https://example.test" (list (appkit-markup-text "link")))
               (appkit-markup-text " ")
               (appkit-markup-object
                'secret (list (appkit-markup-text "object fallback")))))
             (appkit-markup-object-block
              'block-secret
              (list (appkit-markup-paragraph
                     (list (appkit-markup-text "block fallback")))))
             (appkit-markup-preformatted "code" "elisp")))))
      (appkit-markup-ui-insert-document
       document
       :interactive-p nil
       :link-action (lambda (&rest _) (ert-fail "Link callback ran"))
       :object-inserter (lambda (&rest _) (ert-fail "Object callback ran"))
       :preformatted-inserter
       (lambda (&rest _) (ert-fail "Preformatted callback ran")))
      (should (string-match-p "link object fallback" (buffer-string)))
      (should (string-match-p "block fallback" (buffer-string)))
      (should (string-match-p "code" (buffer-string)))
      (should-not (text-property-not-all
                   (point-min) (point-max) appkit-ui-action-property nil)))))

(ert-deftest appkit-markup-ui-object-fallback-adds-one-block-boundary ()
  (with-temp-buffer
    (let ((document
           (appkit-markup-document
            (list
             (appkit-markup-object-block
              'provider
              (list (appkit-markup-paragraph
                     (list (appkit-markup-text "fallback")))))
             (appkit-markup-paragraph
              (list (appkit-markup-text "after")))))))
      (appkit-markup-ui-insert-document document)
      (should (equal (buffer-string) "fallback\nafter\n")))))

(ert-deftest appkit-markup-ui-final-newline-and-row-properties-are-explicit ()
  (with-temp-buffer
    (let ((document
           (appkit-markup-document
            (list (appkit-markup-paragraph
                   (list (appkit-markup-text "body")))))))
      (appkit-markup-ui-insert-document
       document :final-newline-p nil :properties '(row-id 9))
      (should (equal (buffer-string) "body"))
      (should (eq (get-text-property (point-min) 'row-id) 9))
      (should-error
       (appkit-markup-ui-insert-document
        document :properties '(face bold))))))

(ert-deftest appkit-markup-ui-rolls-back-failed-client-inserter ()
  (with-temp-buffer
    (insert "before")
    (let ((document
           (appkit-markup-document
            (list (appkit-markup-object-block 'provider nil)))))
      (should-error
       (appkit-markup-ui-insert-document
        document
        :interactive-p t
        :object-inserter
        (lambda (_node)
          (insert "partial")
          (error "failed"))))
      (should (equal (buffer-string) "before")))))

(ert-deftest appkit-markup-ui-rejects-backward-client-mutation ()
  (with-temp-buffer
    (insert "protected")
    (let ((document
           (appkit-markup-document
            (list (appkit-markup-object-block 'provider nil)))))
      (should-error
       (appkit-markup-ui-insert-document
        document
        :interactive-p t
        :object-inserter
        (lambda (_node)
          (goto-char (point-min))
          (delete-char 1))))
      (should (equal (buffer-string) "protected")))))

(ert-deftest appkit-markup-ui-inserter-cannot-delete-existing-suffix ()
  (with-temp-buffer
    (insert "suffix")
    (goto-char (point-min))
    (let ((document
           (appkit-markup-document
            (list (appkit-markup-object-block 'provider nil)))))
      (should-error
       (appkit-markup-ui-insert-document
        document
        :interactive-p t
        :object-inserter
        (lambda (_node)
          (delete-char 1)
          (insert "object"))))
      (should (equal (buffer-string) "suffix")))))

(ert-deftest appkit-markup-ui-link-factory-cannot-mutate-render-buffer ()
  (with-temp-buffer
    (let ((document
           (appkit-markup-document
            (list
             (appkit-markup-paragraph
              (list
               (appkit-markup-link
                "https://example.test"
                (list (appkit-markup-text "link")))))))))
      (should-error
       (appkit-markup-ui-insert-document
        document
        :interactive-p t
        :link-action
        (lambda (_url)
          (insert "mutation")
          #'ignore)))
      (should (string-empty-p (buffer-string))))))

(ert-deftest appkit-markup-ui-link-factory-cannot-mutate-text-properties ()
  (with-temp-buffer
    (let ((document
           (appkit-markup-document
            (list
             (appkit-markup-paragraph
              (list
               (appkit-markup-link
                "https://example.test"
                (list (appkit-markup-text "link")))))))))
      (should-error
       (appkit-markup-ui-insert-document
        document
        :interactive-p t
        :link-action
        (lambda (_url)
          (put-text-property (point-min) (point-max) 'probe t)
          #'ignore)))
      (should (string-empty-p (buffer-string))))))

(provide 'appkit-markup-ui-test)

;;; appkit-markup-ui-test.el ends here
