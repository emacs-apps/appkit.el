;;; appkit-chat-emoji-test.el --- Tests for shared emoji completion -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'appkit-chat-emoji)
(require 'appkit-chatbuf)

(ert-deftest appkit-chat-completion-delimited-token-supports-closed-token ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "hello :rocket:")
    (should
     (equal (list :start (- (point) 8)
                  :end (point)
                  :trigger ?:
                  :raw ":rocket:"
                  :query "rocket")
            (appkit-chat-completion-delimited-token-bounds ?:)))))

(ert-deftest appkit-chat-completion-delimited-token-rejects-midword-and-time ()
  (dolist (input '("word:rocket" "meet at 12:30" "::"))
    (with-temp-buffer
      (appkit-chatbuf-install-prompt ">>> ")
      (insert input)
      (should-not (appkit-chat-completion-delimited-token-bounds ?:)))))

(ert-deftest appkit-chat-completion-delimited-token-is-safe-outside-composer ()
  (with-temp-buffer
    (insert ":")
    (should-not (appkit-chat-completion-delimited-token-bounds ?:))))

(ert-deftest appkit-chat-emoji-candidates-use-injected-name-table ()
  (let* ((appkit-chat-emoji--candidates nil)
         (appkit-chat-emoji--initialized-p nil)
         (source (make-hash-table :test #'equal))
         (appkit-chat-emoji-source-function (lambda () source)))
    (puthash "rocket" "🚀" source)
    (let ((candidate (car (appkit-chat-emoji-candidates))))
      (should (equal ":rocket:"
                     (appkit-chat-completion-candidate-label candidate)))
      (should (equal "🚀"
                     (appkit-chat-completion-candidate-insert candidate)))
      (should (member "rocket"
                      (appkit-chat-completion-candidate-search-terms
                       candidate))))))

(ert-deftest appkit-chat-emoji-candidates-preserve-category-order ()
  (let ((source (make-hash-table :test #'equal))
        (categories (make-hash-table :test #'equal))
        (order '("Unicode · Smileys" "Unicode · Travel & Places")))
    (puthash "airplane rocket" "🚀" source)
    (puthash "zany smile" "😀" source)
    (puthash "🚀" "Unicode · Travel & Places" categories)
    (puthash "😀" "Unicode · Smileys" categories)
    (let ((candidates
           (appkit-chat-emoji--build-candidates source categories order)))
      (should
       (equal order
              (mapcar
               #'appkit-chat-completion-candidate-group
               candidates))))))

(ert-deftest appkit-chat-emoji-cache-builds-once-until-reset ()
  (let ((appkit-chat-emoji--candidates nil)
        (appkit-chat-emoji--initialized-p nil)
        (source (make-hash-table :test #'equal))
        (calls 0))
    (puthash "rocket" "🚀" source)
    (let ((appkit-chat-emoji-source-function
           (lambda () (cl-incf calls) source)))
      (appkit-chat-emoji-candidates)
      (appkit-chat-emoji-candidates)
      (should (= 1 calls))
      (appkit-chat-emoji-reset-cache)
      (appkit-chat-emoji-candidates)
      (should (= 2 calls)))))

(ert-deftest appkit-chat-emoji-normalized-label-collisions-are-deterministic ()
  (let ((source (make-hash-table :test #'equal)))
    (puthash "foo_bar" "B" source)
    (puthash "foo bar" "A" source)
    (puthash "!!!" "ignored" source)
    (let ((candidates (appkit-chat-emoji--build-candidates source)))
      (should
       (equal '((":foo_bar:" . "foo bar")
                (":foo_bar_2:" . "foo_bar"))
              (mapcar
               (lambda (candidate)
                 (cons
                  (appkit-chat-completion-candidate-label candidate)
                  (plist-get
                   (appkit-chat-completion-candidate-value candidate)
                   :name)))
               candidates))))))

(ert-deftest appkit-chat-emoji-empty-source-is-cached-until-reset ()
  (let* ((appkit-chat-emoji--candidates nil)
         (appkit-chat-emoji--initialized-p nil)
         (calls 0)
         (appkit-chat-emoji-source-function
          (lambda () (cl-incf calls) nil)))
    (should-not (appkit-chat-emoji-candidates))
    (should-not (appkit-chat-emoji-candidates))
    (should (= 1 calls))
    (appkit-chat-emoji-reset-cache)
    (should-not (appkit-chat-emoji-candidates))
    (should (= 2 calls))))

(ert-deftest appkit-chat-emoji-source-error-does-not-poison-cache ()
  (let* ((appkit-chat-emoji--candidates nil)
         (appkit-chat-emoji--initialized-p nil)
         (source (make-hash-table :test #'equal))
         (calls 0)
         (appkit-chat-emoji-source-function
          (lambda ()
            (if (= (cl-incf calls) 1)
                (error "temporary failure")
              source))))
    (puthash "rocket" "🚀" source)
    (cl-letf (((symbol-function 'message) (lambda (&rest _args) nil)))
      (should-not (appkit-chat-emoji-candidates)))
    (should-not appkit-chat-emoji--initialized-p)
    (should (appkit-chat-emoji-candidates))
    (should appkit-chat-emoji--initialized-p)
    (should (= 2 calls))))

(ert-deftest appkit-chat-emoji-capf-commits-unicode-glyph ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert ":rocket:")
    (let ((appkit-chat-emoji--candidates
           (list
            (appkit-chat-completion-candidate-create
             :label ":rocket:"
             :insert "🚀"))))
      (let* ((capf (appkit-chat-emoji-capf))
             (exit-function (plist-get (nthcdr 3 capf) :exit-function)))
        (should capf)
        (funcall exit-function ":rocket:" 'finished)
        (should (equal "🚀" (appkit-chatbuf-input-string)))))))

(provide 'appkit-chat-emoji-test)

;;; appkit-chat-emoji-test.el ends here
