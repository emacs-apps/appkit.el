;;; appkit-view-test.el --- Buffer view behavior -*- lexical-binding: t; -*-
(require 'ert)
(require 'appkit-view)
(require 'appkit-test-helper)

(ert-deftest appkit-view-composes-controls-without-reordering-content ()
  (let* (calls
         (action (lambda (value) (push value calls) nil))
         (controls (appkit-view
                     (actions
                      (button :key 'details :action (list action 'inner) "Details"))))
         (view (appkit-view
                 (card :key 'record :action (list action 'outer)
                       (title "100% complete")
                       (expr controls)
                       (field :key 'status "Status" "ready")
                       (text "After the controls")))))
    (should-not calls)
    (with-temp-buffer
      (appkit-view-insert view)
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "100% complete\n[Details]\nStatus：ready\nAfter the controls\n\n"))
      (goto-char (point-min))
      (appkit-ui-activate)
      (search-forward "Details")
      (backward-char)
      (should (button-at (point)))
      (appkit-ui-activate)
      (should (equal (nreverse calls) '(outer inner)))
      (should-not (string-match-p (regexp-opt (quote ("appkit-view" "record"))) (appkit-view-text view))))))

(ert-deftest appkit-view-projection-preserves-controls-and-publishes-property-changes ()
  (appkit-test-with-surface
    (let* ((surface (appkit-current-surface))
           activated
           (action (lambda (value) (setq activated value)))
           (projection (appkit-with-content-update surface
                         (appkit-projection-create
                          (lambda (row) (appkit-view-insert (appkit-projection-row-payload row)))
                          'appkit-view-anchor :no-separator-p t)))
           (views (lambda (description help argument)
                    (appkit-view-project
                     (list
                      (appkit-view
                        (card :key 'record
                              (title "Record")
                              (text :key 'description description)
                              (field :key 'status "Status" (propertize "ready" 'help-echo help))
                              (actions
                               (button :key 'open :action (list action argument) "Open"))))
                      (appkit-view (card :key 'other (title "Unchanged"))))))))
      (appkit-projection-sync surface projection (funcall views nil "old" 1))
      (goto-char (point-min))
      (search-forward "Unchanged")
      (let ((untouched (copy-marker (- (point) (length "changed")))))
        (goto-char (point-min))
        (search-forward "Open")
        (backward-char)
        (let ((anchor (get-text-property (point) 'appkit-view-anchor)))
          (appkit-projection-sync surface projection (funcall views "New first line\nNew second line" "old" 2))
          (should (equal anchor (get-text-property (point) 'appkit-view-anchor)))
          (should (button-at (point)))
          (appkit-ui-activate)
          (should (equal activated 2))
          (appkit-projection-sync surface projection (funcall views "New first line\nNew second line" "new" 2))
          (goto-char untouched)
          (should (looking-at "changed"))
          (goto-char (point-min))
          (search-forward "ready")
          (should (equal (get-text-property (1- (point)) 'help-echo) "new")))
        (set-marker untouched nil)))))

(ert-deftest appkit-view-rejects-ambiguous-sibling-identities ()
  (should-error
   (appkit-view (card :key 'record (field :key 'same "A" 1) (field :key 'same "B" 2))))
  (should-error
   (appkit-view (card :key 'record
                      (group (field :key 'same "A" 1))
                      (group (field :key 'same "B" 2)))))
  (should-error (appkit-view-project (appkit-view (title "Unkeyed root")))))
