;;; appkit-chat-read-test.el --- Chat reading-state tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit-chat-read)
(require 'appkit-test-helper)

(ert-deftest appkit-chat-read-boundary-survives-receipts-and-history ()
  (appkit-test-with-surface
    (should (eq 'b (appkit-chat-read-update '(a b c) :first-unread-key 'b)))
    (should (eq 'b (appkit-chat-read-update '(a b c) :first-unread-key 'c)))
    (should (eq 'b (appkit-chat-read-update '(older a b c) :first-unread-key nil)))
    (appkit-chat-read-rebase)
    (should-not (appkit-chat-read-update '(older a b c) :first-unread-key nil))
    (should (eq 'd (appkit-chat-read-update '(a b c d) :first-unread-key 'd)))))

(ert-deftest appkit-chat-read-completion-ends-boundary-and-allows-new-unreads ()
  (appkit-test-with-surface
    (should (eq 'b (appkit-chat-read-update
                    '(a b c) :first-unread-key 'b :unread-count 2)))
    (should (eq 'b (appkit-chat-read-update
                    '(a b c) :first-unread-key 'c :unread-count 1)))
    ;; Empty or unknown loaded frontiers are not complete-read evidence.
    (should (eq 'b (appkit-chat-read-update '(a b c) :first-unread-key nil)))
    (should (eq 'b (appkit-chat-read-update '(a b c) :known-p nil)))
    ;; A zero message count wins over an obsolete first-unread field.
    (should-not (appkit-chat-read-update
                 '(a b c) :first-unread-key 'c :unread-count 0))
    (appkit-chat-read-rebase)
    (should-not (appkit-chat-read-update '(a b c) :known-p nil))
    ;; Marking the same message unread again begins a fresh boundary.
    (should (eq 'b (appkit-chat-read-update
                    '(a b c) :first-unread-key 'b :unread-count 2)))
    ;; Some protocols prove completion by cursor/latest rather than a count.
    (should (eq 'b (appkit-chat-read-update
                    '(a b c) :known-p nil :all-read-p :false)))
    (should-not (appkit-chat-read-update
                 '(a b c) :known-p nil :all-read-p t))
    (should (eq 'd (appkit-chat-read-update
                    '(a b c d) :first-unread-key 'd :unread-count 1)))))

(ert-deftest appkit-chat-read-clear-rejects-stale-frontier-relatch ()
  (appkit-test-with-surface
    (appkit-chat-read-update '(a b c) :first-unread-key 'b)
    (appkit-chat-read-clear)
    (should-not (appkit-chat-read-update '(a b c) :first-unread-key 'b))
    (should-not (appkit-chat-read-update '(a b c) :known-p nil))
    (should-not (appkit-chat-read-update '(a b c) :first-unread-key nil))
    (should (eq 'd (appkit-chat-read-update '(a b c d) :first-unread-key 'd)))))

(ert-deftest appkit-chat-read-missing-boundary-is-never-approximated ()
  (appkit-test-with-surface
    (should-not (appkit-chat-read-update '(b c) :first-unread-key 'b :known-p nil))
    (should-not (appkit-chat-read-update '(b c) :first-unread-key 'a))
    (should (eq 'a (appkit-chat-read-update '(a b c) :known-p nil)))
    (should-not (appkit-chat-read-update '(b c) :first-unread-key 'b))
    (appkit-chat-read-rebase)
    (should (eq 'b (appkit-chat-read-update '(b c) :known-p nil)))))

(ert-deftest appkit-chat-read-replacement-surface-does-not-inherit-boundary ()
  (appkit-test-with-surface
    (appkit-chat-read-update '(a b) :first-unread-key 'a)
    (appkit-surface-stop appkit-test-surface)
    ;; A reused chat mode may deliberately retain its buffer-local settings.
    (let ((type
           (appkit-surface-type-create
            :name 'read-replacement :mode #'ignore
            :init (appkit-surface-type-init appkit-test--surface-type)
            :update (appkit-surface-type-update appkit-test--surface-type)
            :renderer-factory
            (appkit-surface-type-renderer-factory appkit-test--surface-type))))
      (appkit-open-generated-surface
       type :app appkit-test-app :identity 'replacement :buffer (current-buffer)))
    (should-not (appkit-chat-read-update '(a b) :first-unread-key nil))
    (should (eq 'b (appkit-chat-read-update '(a b) :first-unread-key 'b)))))

(defun appkit-chat-read-test--print (row)
  "Render ROW with a real semantic separator and canonical row anchoring."
  (let ((start (point)))
    (when (plist-get (appkit-chat-timeline-row-context row) :unread)
      (appkit-chat-read-insert-divider 40))
    (insert (format "%s\n" (appkit-chat-timeline-row-key row)))
    (add-text-properties
     start (point)
     (list 'test-message-key (appkit-chat-timeline-row-key row)
           'read-only t 'rear-nonsticky t))))

(ert-deftest appkit-chat-read-target-excludes-dividers-and-unconfirmed-rows ()
  (appkit-test-with-surface
    (appkit-chatbuf-init-state)
    (appkit-chat-timeline-ensure
     :printer #'appkit-chat-read-test--print :anchor-property 'test-message-key)
    (appkit-chat-timeline-sync
     (list (appkit-chat-timeline-row-create :key 'a :payload 'message)
           (appkit-chat-timeline-row-create
            :key 'b :payload 'message :context '(:unread t))
           (appkit-chat-timeline-row-create :key 'pending :payload 'local)))
    (appkit-chat-timeline-set-frame
     "" "End of loaded history\n"
     :bind-input-function
     (lambda ()
       (appkit-chatbuf-bind-input-region
        :visible-p t :prompt ">>> " :input-text "draft stays here")))
    (let ((eligible (lambda (row)
                      (eq 'message (appkit-chat-timeline-row-payload row)))))
      (goto-char (appkit-chat-timeline-key-position 'b))
      (should-not (appkit-chat-read-target nil eligible))
      (forward-line)
      (should (eq 'b (appkit-chat-timeline-row-key
                      (appkit-chat-read-target nil eligible))))
      (goto-char (appkit-chat-timeline-key-position 'pending))
      (should-not (appkit-chat-read-target nil eligible))
      (goto-char (appkit-chat-timeline-footer-start-position))
      (should-not (appkit-chat-read-target nil eligible))
      (goto-char (appkit-chatbuf-prompt-start-position))
      (should-not (appkit-chat-read-target nil eligible))
      (goto-char (+ (appkit-chatbuf-input-start-position) 3))
      (let ((saved-point (point))
            (draft (appkit-chatbuf-input-string)))
        (should (eq 'b (appkit-chat-timeline-row-key
                        (appkit-chat-read-target nil eligible))))
        (should (= saved-point (point)))
        (should (equal draft (appkit-chatbuf-input-string)))))))

(provide 'appkit-chat-read-test)

;;; appkit-chat-read-test.el ends here
