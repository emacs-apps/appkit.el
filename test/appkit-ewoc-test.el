;;; appkit-ewoc-test.el --- Tests for keyed EWOC reconciliation -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-ewoc)

(cl-defstruct (appkit-ewoc-test-entry
               (:constructor appkit-ewoc-test-entry-create))
  key
  text)

(defun appkit-ewoc-test--entry-keys (ewoc)
  "Return entry keys from EWOC in display order."
  (let ((node (ewoc-nth ewoc 0))
        keys)
    (while node
      (push (appkit-ewoc-test-entry-key (ewoc-data node)) keys)
      (setq node (ewoc-next ewoc node)))
    (nreverse keys)))

(ert-deftest appkit-ewoc-reconciles-with-stable-nodes ()
  (with-temp-buffer
    (let* ((prints (make-hash-table :test #'equal))
           (ewoc (ewoc-create
                  (lambda (entry)
                    (let ((key (appkit-ewoc-test-entry-key entry)))
                      (puthash key (1+ (gethash key prints 0)) prints)
                      (insert (appkit-ewoc-test-entry-text entry) "\n")))
                  nil nil t))
           (entry (lambda (key text)
                    (appkit-ewoc-test-entry-create :key key :text text)))
           (key-fn #'appkit-ewoc-test-entry-key)
           nodes a-node b-node)
      (setq nodes
            (appkit-ewoc-reconcile
             ewoc (list (funcall entry 'a "A")
                        (funcall entry 'b "B"))
             key-fn)
            a-node (gethash 'a nodes)
            b-node (gethash 'b nodes))
      (should (equal '(a b) (appkit-ewoc-test--entry-keys ewoc)))
      (setq nodes
            (appkit-ewoc-reconcile
             ewoc (list (funcall entry 'a "A")
                        (funcall entry 'b "B2"))
             key-fn))
      (should (eq a-node (gethash 'a nodes)))
      (should (eq b-node (gethash 'b nodes)))
      (should (= 1 (gethash 'a prints)))
      (should (= 2 (gethash 'b prints)))
      (setq nodes
            (appkit-ewoc-reconcile
             ewoc (list (funcall entry 'b "B2")
                        (funcall entry 'c "C"))
             key-fn))
      (should (equal '(b c) (appkit-ewoc-test--entry-keys ewoc)))
      (should-not (gethash 'a nodes))
      (should (equal "B2\nC\n" (buffer-string))))))

(ert-deftest appkit-ewoc-supports-explicit-invalidation ()
  (with-temp-buffer
    (let* ((prints 0)
           (ewoc (ewoc-create
                  (lambda (entry)
                    (cl-incf prints)
                    (insert (appkit-ewoc-test-entry-text entry) "\n"))
                  nil nil t))
           (entry (appkit-ewoc-test-entry-create :key 'row :text "row"))
           (nodes (appkit-ewoc-reconcile
                   ewoc (list entry) #'appkit-ewoc-test-entry-key)))
      (should (= 1 prints))
      (appkit-ewoc-reconcile
       ewoc (list entry) #'appkit-ewoc-test-entry-key)
      (should (= 1 prints))
      (should (appkit-ewoc-invalidate-key ewoc nodes 'row))
      (should (= 2 prints))
      (should-not (appkit-ewoc-invalidate-key ewoc nodes 'missing)))))

(ert-deftest appkit-ewoc-retains-node-after-deleted-prefix ()
  (with-temp-buffer
    (let* ((ewoc (ewoc-create
                  (lambda (entry)
                    (insert (appkit-ewoc-test-entry-text entry) "\n"))
                  nil nil t))
           (a (appkit-ewoc-test-entry-create :key 'a :text "A"))
           (b (appkit-ewoc-test-entry-create :key 'b :text "B"))
           (nodes (appkit-ewoc-reconcile
                   ewoc (list a b) #'appkit-ewoc-test-entry-key))
           (b-node (gethash 'b nodes)))
      (setq nodes
            (appkit-ewoc-reconcile
             ewoc (list b) #'appkit-ewoc-test-entry-key))
      (should (eq b-node (gethash 'b nodes)))
      (should (equal "B\n" (buffer-string))))))

(ert-deftest appkit-ewoc-rejects-duplicate-keys ()
  (with-temp-buffer
    (let ((ewoc (ewoc-create #'ignore nil nil t))
          (entry (lambda (text)
                   (appkit-ewoc-test-entry-create :key 'same :text text))))
      (should-error
       (appkit-ewoc-reconcile
        ewoc (list (funcall entry "one") (funcall entry "two"))
        #'appkit-ewoc-test-entry-key))
      (should-not (ewoc-nth ewoc 0)))))

(provide 'appkit-ewoc-test)

;;; appkit-ewoc-test.el ends here
