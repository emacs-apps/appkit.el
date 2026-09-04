;;; appkit-test-helper.el --- Shared appkit test fixtures -*- lexical-binding: t; -*-

(require 'appkit-core)

(appkit-define-app-kind appkit-test)

(defmacro appkit-test-with-view (&rest body)
  "Evaluate BODY in a temporary buffer with a live appkit view."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let ((appkit-test-app
            (appkit-app-start 'appkit-test :id (make-symbol "app"))))
       (unwind-protect
           (progn
             (appkit-attach-view
              :app appkit-test-app
              :id (make-symbol "view")
              :mode major-mode)
             ,@body)
         (appkit-app-close appkit-test-app)))))

(provide 'appkit-test-helper)

;;; appkit-test-helper.el ends here
