;;; appkit-chat-render-test.el --- Shared chat renderer regressions -*- lexical-binding: t; -*-
(require 'ert)
(require 'appkit-chat-render)
(require 'appkit-test-helper)

(ert-deftest appkit-chat-render-resource-passes-and-explicit-row-keys ()
  (appkit-test-with-surface
   (let* ((surface (appkit-current-surface))
          (prints (make-hash-table :test #'equal))
          (projects 0) (frames 0)
          (rows (cl-loop for key from 1 to 100
                         collect (appkit-chat-timeline-row-create
                                  :key key :payload key :dependencies (list (list :avatar key)))))
          (renderer
           (appkit-chat-renderer-create
            :ensure (lambda (&rest _)
                      (appkit-chat-timeline-ensure
                       :anchor-property 'message-key
                       :printer (lambda (row)
                                  (let ((key (appkit-chat-timeline-row-key row)))
                                    (puthash key (1+ (gethash key prints 0)) prints)
                                    (insert (propertize (format "%s\n" key) 'message-key key))))))
            :project (lambda (&rest _) (cl-incf projects) rows)
            :frame (lambda (&rest _)
                     (cl-incf frames)
                     (appkit-chat-timeline-set-footer
                      "footer\n" :composer-visible-p t
                      :bind-input-function
                      (lambda () (appkit-chatbuf-bind-input-region
                                  :visible-p t :prompt "> " :input-text "draft"))))))
          (render (appkit-generated-renderer-render renderer)))
     (funcall render surface nil nil (appkit-projection-change-create :full-p t))
     (goto-char (+ 2 (appkit-chatbuf-input-start-position)))
     (let ((node (appkit-chat-timeline-node 80))
           (point-before (point)))
       (clrhash prints)
       (funcall render surface nil nil (appkit-projection-change-create :resources '((:avatar absent))))
       (should (= 0 (hash-table-count prints)))
       (should (= projects 1))
       (should (= frames 1))
       (funcall render surface nil nil (appkit-projection-change-create :resources '((:avatar 80))))
       (should (= 1 (hash-table-count prints)))
       (should (= 1 (gethash 80 prints)))
       (should (= projects 1))
       (clrhash prints)
       (funcall render surface nil nil (appkit-projection-change-create :keys '(80)))
       (should (= projects 2))
       (should (= frames 1))
       (should (= 1 (hash-table-count prints)))
       (should (= 1 (gethash 80 prints)))
       (should (eq node (appkit-chat-timeline-node 80)))
       (should (= point-before (point)))
       (should (equal "draft" (appkit-chatbuf-input-state)))
       (clrhash prints)
       (funcall render surface nil nil (appkit-projection-change-create :geometry-p t))
       (should (= 100 (hash-table-count prints)))
       (should (= projects 2))
       (should (= frames 2))
       (funcall render surface nil nil (appkit-projection-change-create :position 80))
       (should (equal 80 (appkit-chat-timeline-key-at-point)))))))

(ert-deftest appkit-chat-render-promotes-outgoing-nodes-once ()
  (appkit-test-with-surface
   (let* ((surface (appkit-current-surface))
          (printed 0) (accepted nil)
          (renderer
           (appkit-chat-renderer-create
            :ensure (lambda (&rest _)
                      (appkit-chat-timeline-ensure
                       :anchor-property 'message-key
                       :printer (lambda (row)
                                  (cl-incf printed)
                                  (insert (propertize "message\n" 'message-key
                                                      (appkit-chat-timeline-row-key row))))))
            :project (lambda (&rest _)
                       (cl-loop for id from 1 to 20
                                collect (appkit-chat-timeline-row-create
                                         :key (if accepted id (list :send id)) :payload id)))
            :rekeys (lambda (&rest _)
                      (when accepted (cl-loop for id from 1 to 20 collect (cons (list :send id) id))))))
          (render (appkit-generated-renderer-render renderer)))
     (funcall render surface nil nil (appkit-projection-change-create :full-p t))
     (goto-char (ewoc-location (appkit-chat-timeline-node '(:send 10))))
     (let ((node (appkit-chat-timeline-node '(:send 10))))
       (setq accepted t printed 0)
       (funcall render surface nil nil (appkit-projection-change-create :full-p t))
       (should (= printed 20))
       (should (eq node (appkit-chat-timeline-node 10)))
       (should (equal 10 (appkit-chat-timeline-key-at-point)))
       (setq printed 0)
       (funcall render surface nil nil (appkit-projection-change-create :full-p t))
       (should (= printed 0))
       (funcall render surface nil nil (appkit-projection-change-create :resources '(unrelated)))
       (should (= printed 0))))))

(provide 'appkit-chat-render-test)
