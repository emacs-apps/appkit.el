;;; appkit-transaction-test.el --- Tests for appkit transactions -*- lexical-binding: t; -*-

(require 'ert)

(require 'appkit-transaction)
(require 'appkit-test-helper)

(ert-deftest appkit-content-update-is-generated-and-undo-free ()
  (appkit-test-with-surface
    (let ((view (appkit-current-surface)))
      (setq buffer-undo-list nil)
      (appkit-with-content-update view
        (insert "generated"))
      (should (equal (buffer-string) "generated"))
      (should-not buffer-undo-list))))

(ert-deftest appkit-property-update-rejects-text-mutation-in-strict-mode ()
  (appkit-test-with-surface
    (let ((view (appkit-current-surface))
          (appkit-strict-boundaries t))
      (insert "text")
      (should-error
       (appkit-with-property-update view
         (insert "invalid"))))))

(ert-deftest appkit-property-update-allows-display-properties ()
  (appkit-test-with-surface
    (let ((view (appkit-current-surface))
          (appkit-strict-boundaries t))
      (insert "text")
      (appkit-with-property-update view
        (put-text-property (point-min) (point-max) 'face 'bold))
      (should (eq (get-text-property (point-min) 'face) 'bold)))))

(provide 'appkit-transaction-test)

;;; appkit-transaction-test.el ends here
