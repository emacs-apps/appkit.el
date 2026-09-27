;;; appkit-translate-respond-test.el --- Respond translation boundaries -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-test-helper)
(require 'appkit-translate)

(ert-deftest appkit-translate-respond-malformed-completion-releases-queue ()
  "An invalid completed payload must fail its translation, not strand the batch."
  (skip-unless (locate-library "respond"))
  (require 'appkit-translate-respond)
  (appkit-test-with-surface
    (let* ((credentials (respond-auth-credentials-create
                         :access-token "test-token" :account-id "test-account"
                         :expires-at (+ (float-time) 3600)))
           (send (symbol-function 'respond-send))
           (requests (make-hash-table :test #'equal))
           (sources '((:key a :version 1 :text "First")
                      (:key b :version 1 :text "Second")
                      (:key c :version 1 :text "Third"))))
      (cl-letf (((symbol-function 'respond-auth-ensure)
                 (lambda (&rest options)
                   (funcall (plist-get options :on-success) credentials)
                   #'ignore))
                ((symbol-function 'respond-send)
                 (lambda (client body &rest options)
                   (let ((request (apply send client body options)))
                     (puthash (plist-get (aref (plist-get (aref (plist-get body :input) 0)
                                                            :content) 0)
                                         :text)
                              request requests)
                     request))))
        (let* ((states (appkit-translate-request-many
                        sources (appkit-translate-respond-backend) "zh" nil #'ignore))
               (queue (appkit-translate-context-queue
                       (gethash appkit-test-surface appkit-translate--contexts)))
               (first (gethash "First" requests)))
          (should (equal (mapcar (lambda (state) (plist-get state :status)) states)
                         '(running running queued)))
          ;; Native Respond has already retired this request before on-done
          ;; attempts to turn its malformed output into a translated string.
          (respond--event
           first
           '(:type "response.completed"
             :response (:id "first" :status "completed"
                        :output [(:type "message"
                                  :content [(:type "output_text" :text 123)])])))
          (should (eq (respond-request-status first) 'completed))
          (should (null (respond-request-timer first)))
          (should (respond-client-closed-p (respond-request-client first)))
          (should (equal (mapcar (lambda (state) (plist-get state :status)) states)
                         '(failed running running)))
          (should (= (appkit-task-queue-active-count queue) 2))
          (should (= (appkit-task-queue-queued-count queue) 0))
          (dolist (entry '(("Second" . "第二条") ("Third" . "第三条")))
            (respond--event
             (gethash (car entry) requests)
             (list :type "response.completed"
                   :response
                   (list :id (car entry) :status "completed"
                         :output (vector
                                  (list :type "message"
                                        :content (vector
                                                  (list :type "output_text"
                                                        :text (cdr entry)))))))))
          (should (equal (mapcar (lambda (state) (plist-get state :status)) states)
                         '(failed completed completed)))
          (should (equal (mapcar (lambda (state) (plist-get state :text)) states)
                         '("" "第二条" "第三条")))
          (should (= (appkit-task-queue-total-count queue) 0)))))))

(provide 'appkit-translate-respond-test)
;;; appkit-translate-respond-test.el ends here
