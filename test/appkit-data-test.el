;;; appkit-data-test.el --- Persistent model update regressions  -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit-data)

(ert-deftest appkit-plist-with-preserves-snapshots-and-shares-untouched-values ()
  (let* ((items (list 'one 'two))
         (query (list :order "newest"))
         (state (list :query query :items items
                      :status (list :phase 'ready :token 'old)))
         (saved (copy-tree state))
         (next (appkit-plist-with state
                 ((:status) :phase 'loading :token nil)
                 ((:page) :number 2 :total nil))))
    (should (equal state saved))
    (should (eq (plist-get next :query) query))
    (should (eq (plist-get next :items) items))
    (should-not (eq (plist-get next :status) (plist-get state :status)))
    (should (equal (plist-get next :status) '(:phase loading :token nil)))
    (should (equal (plist-get next :page) '(:number 2 :total nil)))
    (dotimes (index 20)
      (setq next (appkit-plist-with next
                   ((:status) :phase index :token nil))))
    (should (= (length next) 8))
    (should (= (length (plist-get next :status)) 4))
    (should (equal state saved))
    (should (eq state (appkit-plist-with state)))))

(ert-deftest appkit-plist-with-evaluates-inputs-once-in-source-order ()
  (let (events)
    (should
     (equal
      (appkit-plist-with (progn (push 'model events) nil)
        (((progn (push 'path events) :status))
         :phase (progn (push 'phase events) 'ready)
         :message (progn (push 'message events) nil))
        (() :items (progn (push 'items events) '(one))))
      '(:status (:phase ready :message nil) :items (one))))
    (should (equal (nreverse events) '(model path phase message items)))))

(ert-deftest appkit-plist-with-coalesces-separate-writes-to-one-branch ()
  (let* ((state '(:status (:phase loading :token request)))
         (next (appkit-plist-with state
                 ((:status) :phase 'ready)
                 (() :items '(one))
                 ((:status) :token nil))))
    (should (equal next '(:status (:phase ready :token nil) :items (one))))
    (should (equal state '(:status (:phase loading :token request))))))

(ert-deftest appkit-plist-with-rejects-conflicts-and-invalid-paths ()
  (let* ((state (list :status (list :phase 'ready)))
         (saved (copy-tree state))
         (component :status))
    (should-error
     (appkit-plist-with state
       ((:status) :phase 'loading)
       ((component) :phase 'error)))
    (should-error
     (appkit-plist-with state
       (() :status nil)
       ((:status) :phase 'loading)))
    (should-error (appkit-plist-with state ((:status :phase) :nested t)))
    (should-error (appkit-plist-with state (((identity 'status)) :phase 'loading)))
    (should (equal state saved)))
  (should-error (macroexpand '(appkit-plist-with nil ((:status) :phase))))
  (should-error (macroexpand '(appkit-plist-with nil ((:status) phase 'ready)))))

(provide 'appkit-data-test)

;;; appkit-data-test.el ends here
