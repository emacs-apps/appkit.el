;;; appkit-data-test.el --- Persistent model update regressions  -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit-data)

(ert-deftest appkit-data-with-preserves-snapshots-and-shares-untouched-values ()
  (let* ((items (list 'one 'two))
         (query (list :order "newest"))
         (state (list :query query :items items
                      :status (list :phase 'ready :token 'old)))
         (saved (copy-tree state))
         (next (appkit-data-with state
                 ((:status) :phase 'loading :token nil)
                 ((:page) :number 2 :total nil))))
    (should (equal state saved))
    (should (eq (plist-get next :query) query))
    (should (eq (plist-get next :items) items))
    (should-not (eq (plist-get next :status) (plist-get state :status)))
    (should (equal (plist-get next :status) '(:phase loading :token nil)))
    (should (equal (plist-get next :page) '(:number 2 :total nil)))
    (dotimes (index 20)
      (setq next (appkit-data-with next
                   ((:status) :phase index :token nil))))
    (should (= (length next) 8))
    (should (= (length (plist-get next :status)) 4))
    (should (equal state saved))
    (should (eq state (appkit-data-with state)))))

(ert-deftest appkit-data-with-evaluates-inputs-once-in-source-order ()
  (let (events)
    (should
     (equal
      (appkit-data-with (progn (push 'model events) nil)
        (((progn (push 'path events) :status))
         :phase (progn (push 'phase events) 'ready)
         :message (progn (push 'message events) nil))
        (() :items (progn (push 'items events) '(one))))
      '(:status (:phase ready :message nil) :items (one))))
    (should (equal (nreverse events) '(model path phase message items)))))

(ert-deftest appkit-data-with-coalesces-separate-writes-to-one-branch ()
  (let* ((state '(:status (:phase loading :token request)))
         (next (appkit-data-with state
                 ((:status) :phase 'ready)
                 (() :items '(one))
                 ((:status) :token nil))))
    (should (equal next '(:status (:phase ready :token nil) :items (one))))
    (should (equal state '(:status (:phase loading :token request))))))

(ert-deftest appkit-data-with-rejects-conflicts-and-invalid-paths ()
  (let* ((state (list :status (list :phase 'ready)))
         (saved (copy-tree state))
         (component :status))
    (should-error
     (appkit-data-with state
       ((:status) :phase 'loading)
       ((component) :phase 'error)))
    (should-error
     (appkit-data-with state
       (() :status nil)
       ((:status) :phase 'loading)))
    (should-error (appkit-data-with state ((:status :phase) :nested t)))
    (should-error (appkit-data-with state (((identity 'status)) :phase 'loading)))
    (should (equal state saved)))
  (should-error (macroexpand '(appkit-data-with nil ((:status) :phase))))
  (should-error (appkit-data-with nil ((:status) 'phase 'ready))))


(cl-defstruct (appkit-data-test-record (:constructor appkit-data-test-record-create)
                                      (:copier nil))
  (status nil :read-only t) (entities nil :read-only t) (query nil :read-only t))

(cl-defstruct (appkit-data-test-child (:include appkit-data-test-record)
                                     (:constructor appkit-data-test-child-create))
  (extra nil :read-only t))

(ert-deftest appkit-data-with-mixes-inherited-records-plists-and-persistent-maps ()
  (let* ((query (list :order "newest"))
         (entity '(:text "original" :read nil))
         (entities (appkit-data-map-set appkit-data-map-empty "one" entity))
         (before (appkit-data-test-child-create
                  :status '(:phase loading :token request) :entities entities :query query :extra 42))
         (key (copy-sequence "one"))
         (after (appkit-data-with before
                  ((:status) :phase 'ready :token nil)
                  ((:entities key) :text "updated" :read t))))
    (should (appkit-data-test-child-p after))
    (should (= 42 (appkit-data-test-child-extra after)))
    (should (eq query (appkit-data-test-record-query after)))
    (should (eq entity (appkit-data-map-get entities "one")))
    (should (equal '(:phase loading :token request) (appkit-data-test-record-status before)))
    (should (equal '(:text "updated" :read t)
                   (appkit-data-map-get (appkit-data-test-record-entities after) "one")))
    (should (eq before (appkit-data-with before (() :extra 42))))
    (should-error (appkit-data-with before (() :unknown t)) :type 'cl-struct-unknown-slot)
    (should-error (appkit-data-with before (() :query appkit-data-delete)))
    (should-error (appkit-data-with before
                    ((:entities) key 1 (copy-sequence key) 2)))
    (should (eq entity (appkit-data-map-get entities "one")))))

(ert-deftest appkit-data-with-deletes-keys-and-evaluates-dynamic-keys-once ()
  (let ((before (appkit-data-map-edit appkit-data-map-empty '((a . nil) (b . 2))))
        events)
    (let ((after (appkit-data-with (progn (push 'root events) before)
                   (() (progn (push 'key events) 'a) (progn (push 'value events) appkit-data-delete)
                       'c 3))))
      (should (equal '(root key value) (nreverse events)))
      (should (eq appkit-data-absent (appkit-data-map-get after 'a appkit-data-absent)))
      (should-not (appkit-data-map-get before 'a 'missing))
      (should (= 2 (appkit-data-map-count after))))
    (should (equal '(:other t)
                   (appkit-data-with '(:gone nil :other t) (() :gone appkit-data-delete))))))

(provide 'appkit-data-test)

;;; appkit-data-test.el ends here
