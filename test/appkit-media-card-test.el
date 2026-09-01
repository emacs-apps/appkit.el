;;; appkit-media-card-test.el --- Tests for media card actions -*- lexical-binding: t; -*-

(require 'ert)

(require 'appkit-media-card)

(ert-deftest appkit-media-card-context-prefers-exact-card-before-fallback ()
  (with-temp-buffer
    (let* ((opened nil)
           (fallback-calls 0)
           (exact
            (appkit-media-card-context-create
             :payload 'exact
             :kind 'image
             :open-action (lambda () (setq opened 'exact))))
           (fallback
            (appkit-media-card-context-create
             :payload 'fallback
             :kind 'video
             :open-action (lambda () (setq opened 'fallback))))
           (card-start (point)))
      (insert "exact card\n")
      (add-text-properties
       card-start (point)
       (list appkit-media-card-context-property exact))
      (insert "message text\n")
      (setq-local appkit-media-card-fallback-context-function
                  (lambda ()
                    (setq fallback-calls (1+ fallback-calls))
                    fallback))
      (goto-char card-start)
      (should (eq (plist-get (appkit-media-card-context-at-point) :payload)
                  'exact))
      (should (= fallback-calls 0))
      (appkit-media-card-open)
      (should (eq opened 'exact))
      (should (= fallback-calls 0))
      (forward-line 1)
      (should (eq (plist-get (appkit-media-card-context-at-point) :payload)
                  'fallback))
      (should (= fallback-calls 1))
      (appkit-media-card-open)
      (should (eq opened 'fallback))
      (should (= fallback-calls 2)))))

(ert-deftest appkit-media-card-context-finds-property-at-line-start ()
  (with-temp-buffer
    (let ((context
           (appkit-media-card-context-create :payload 'line-context)))
      (insert "prefix and body")
      (put-text-property
       (point-min) (1+ (point-min))
       appkit-media-card-context-property context)
      (goto-char (point-max))
      (should (eq context (appkit-media-card-context-at-point))))))

(ert-deftest appkit-media-card-context-outside-buffer-uses-fallback ()
  (with-temp-buffer
    (let ((fallback
           (appkit-media-card-context-create :payload 'fallback)))
      (setq-local appkit-media-card-fallback-context-function
                  (lambda () fallback))
      (should (eq fallback
                  (appkit-media-card-context-at-point
                   (1+ (point-max))))))))

(ert-deftest appkit-media-card-actions-dispatch-each-context-callback ()
  (with-temp-buffer
    (let* ((seen nil)
           (context
           (appkit-media-card-context-create
            :payload 'payload
            :kind 'video
            :title "Clip"
            :open-action (lambda () (push 'open seen))
            :download-action (lambda () (push 'download seen))
            :cancel-action (lambda () (push 'cancel seen))
            :save-as-action (lambda () (push 'save-as seen))
            :copy-url-action (lambda () (push 'copy-url seen)))))
      (insert "media")
      (add-text-properties
       (point-min) (point-max)
       (list appkit-media-card-context-property context))
      (goto-char (point-min))
      (should (eq (appkit-media-card-action-function 'open)
                  (plist-get context :open-action)))
      (appkit-media-card-open)
      (appkit-media-card-download)
      (appkit-media-card-cancel-download)
      (appkit-media-card-save-as)
      (appkit-media-card-copy-url)
      (should (equal (nreverse seen)
                     '(open download cancel save-as copy-url))))))

(ert-deftest appkit-media-card-action-availability-explains-failures ()
  (with-temp-buffer
    (should (equal "No media at point"
                   (appkit-media-card-action-inapt-reason 'open)))
    (should-error (appkit-media-card-call-action 'open)
                  :type 'user-error)
    (let ((context
           (appkit-media-card-context-create
            :payload 'payload
            :open-action #'ignore)))
      (should-not
       (appkit-media-card-action-inapt-reason 'open context))
      (should (equal "Download unavailable"
                     (appkit-media-card-action-inapt-reason
                      'download context)))
      (should-error (appkit-media-card-call-action 'download context)
                    :type 'user-error))))

(ert-deftest appkit-media-action-properties-wrap-zero-argument-callbacks ()
  (with-temp-buffer
    (let ((called 0))
      (insert "media")
      (appkit-media-add-action-properties
       (point-min) (point-max)
       (lambda () (setq called (1+ called)))
       "Open media")
      (let* ((map (get-text-property (point-min) 'keymap))
             (return-command (lookup-key map (kbd "RET")))
             (mouse-command (lookup-key map [mouse-1])))
        (should (commandp return-command))
        (should (eq return-command mouse-command))
        (should (eq (get-text-property (point-min) 'mouse-face) 'highlight))
        (should (equal (get-text-property (point-min) 'help-echo)
                       "Open media"))
        (call-interactively return-command)
        (should (= called 1))))))

(ert-deftest appkit-media-action-properties-ignore-invalid-actions ()
  (with-temp-buffer
    (insert "media")
    (appkit-media-add-action-properties
     (point-min) (point-max) nil "Unavailable")
    (should-not (get-text-property (point-min) 'keymap))
    (appkit-media-add-action-properties
     (point-min) (point-min) #'ignore "Empty")
    (should-not (get-text-property (point-min) 'keymap))))

(provide 'appkit-media-card-test)

;;; appkit-media-card-test.el ends here
