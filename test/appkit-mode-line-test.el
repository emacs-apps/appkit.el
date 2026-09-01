;;; appkit-mode-line-test.el --- Tests for mode-line helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit-mode-line)

(defvar appkit-mode-line-test--cache nil)

(ert-deftest appkit-mode-line-install-is-idempotent ()
  (let ((mode-line-misc-info '(existing)))
    (appkit-mode-line-install 'provider)
    (appkit-mode-line-install 'provider)
    (should (equal '(existing provider) mode-line-misc-info))
    (appkit-mode-line-uninstall 'provider)
    (should (equal '(existing) mode-line-misc-info))))

(ert-deftest appkit-mode-line-indicator-is-clickable-with-optional-prefix ()
  (let ((text (appkit-mode-line-indicator
               "@2" :prefix " " :face 'warning
               :command #'ignore :help-echo "mentions")))
    (should (equal " @2" (substring-no-properties text)))
    (should (eq 'warning (get-text-property 1 'face text)))
    (should (keymapp (get-text-property 1 'local-map text)))
    (should (equal "mentions" (get-text-property 1 'help-echo text))))
  (should-not (appkit-mode-line-indicator nil :prefix " ")))

(ert-deftest appkit-mode-line-update-cache-formats-and-returns-value ()
  (let ((appkit-mode-line-test--cache nil)
        (redisplays 0))
    (cl-letf (((symbol-function 'format-mode-line)
               (lambda (format) (should (equal '("ready") format)) "ready"))
              ((symbol-function 'force-mode-line-update)
               (lambda (&rest _) (cl-incf redisplays))))
      (should (equal "ready"
                     (appkit-mode-line-update-cache
                      'appkit-mode-line-test--cache '("ready"))))
      (should (equal "ready" appkit-mode-line-test--cache))
      (should (zerop redisplays)))))

(provide 'appkit-mode-line-test)

;;; appkit-mode-line-test.el ends here
