;;; appkit-compose-test.el --- Tests for compose primitives -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit-compose)

(ert-deftest appkit-compose-generation-and-capture-follow-semantic-edits ()
  "Generated edits should not advance the client document generation."
  (with-temp-buffer
    (insert "draft")
    (appkit-compose-setup)
    (goto-char (point-max))
    (insert " one")
    (should (= 1 (appkit-compose-generation)))
    (appkit-compose-without-tracking
      (insert " generated"))
    (should (= 1 (appkit-compose-generation)))
    (should
     (equal '(:generation 1 :value "draft one generated")
            (appkit-compose-capture)))))

(ert-deftest appkit-compose-effect-owner-fences-stale-callbacks ()
  "Only the current effect owner may update or finish a compose session."
  (with-temp-buffer
    (appkit-compose-setup)
    (let ((first (appkit-compose-operation-begin
                  'submitting :label "Sending")))
      (should (appkit-compose-operation-finish first))
      (let ((second (appkit-compose-operation-begin
                     'submitting :label "Retrying")))
        (should-not (appkit-compose-operation-finish first))
        (should (appkit-compose-operation-current-p second))
        (should (equal "Retrying" (appkit-compose-status-text)))
        (should (appkit-compose-operation-finish second))
        (should-not (appkit-compose-operation-active-p))))))

(ert-deftest appkit-compose-cancel-is-a-request-not-an-outcome ()
  "Cancellation should keep effect ownership until client settlement."
  (with-temp-buffer
    (let (cancelled)
      (appkit-compose-setup)
      (let ((owner
             (appkit-compose-operation-begin
              'submitting
              :label "Uploading"
              :progress 0.5
              :cancel-function (lambda () (setq cancelled t)))))
        (should (string-match-p "50%" (appkit-compose-status-text)))
        (should (eq owner (appkit-compose-cancel-operation)))
        (should cancelled)
        (should (appkit-compose-operation-current-p owner))
        (should (appkit-compose-operation-cancel-requested-p))
        (should-error (appkit-compose-cancel-operation) :type 'user-error)
        (should (appkit-compose-operation-finish owner))
        (should-not (appkit-compose-operation-active-p))))))

(ert-deftest appkit-compose-reset-invalidates-an-old-effect-owner ()
  "Loading another document should retire previous view-local authority."
  (with-temp-buffer
    (let (cancelled)
      (appkit-compose-setup :generation 4)
      (let ((owner
             (appkit-compose-operation-begin
              'saving :cancel-function (lambda () (setq cancelled t)))))
        (appkit-compose-reset :generation 9)
        (should cancelled)
        (should (= 9 (appkit-compose-generation)))
        (should-not (appkit-compose-operation-current-p owner))))))


(provide 'appkit-compose-test)

;;; appkit-compose-test.el ends here
