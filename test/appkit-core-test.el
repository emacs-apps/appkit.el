;;; appkit-core-test.el --- Tests for App lifecycle handles -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'appkit-core)

(defun appkit-core-test--app ()
  "Return a minimal canonical App for handle tests."
  (appkit-app-start
   (appkit-app-type-create
    :name 'handle-test
    :init
    (lambda (_context _input)
      (appkit-next :model nil :render appkit-render-none))
    :update
    (lambda (_context model _message)
      (appkit-next :model model :render appkit-render-none)))))

(ert-deftest appkit-handle-retire-does-not-cancel ()
  (let ((app (appkit-core-test--app)) cancelled)
    (unwind-protect
        (let ((handle
               (appkit-register-handle
                app 'function 'owned
                (lambda (object) (push object cancelled)))))
          (should (memq handle (appkit-app-handles app)))
          (should (appkit-retire-handle handle))
          (should-not (appkit-handle-alive-p handle))
          (should-not (appkit-app-handles app))
          (should-not cancelled))
      (appkit-app-close app))))

(ert-deftest appkit-app-stop-cleans-all-handles-after-one-error ()
  (let ((app (appkit-core-test--app)) visited)
    (appkit-register-handle
     app 'function 'good (lambda (object) (push object visited)))
    (appkit-register-handle
     app 'function 'bad
     (lambda (object)
       (push object visited)
       (error "handle cleanup failed")))
    (should-error (appkit-app-close app) :type 'error)
    (should (equal (sort visited #'string-lessp) '(bad good)))
    (should-not (appkit-app-handles app))
    (should (eq (appkit-app-status app) 'stopped))))

(ert-deftest appkit-cleanup-items-continue-after-error-and-quit ()
  (let (conditions visited)
    (appkit--run-cleanup-items
     '(first failing quitting last)
     (lambda (item)
       (push item visited)
       (pcase item
         ('failing (error "cleanup failed"))
         ('quitting (signal 'quit nil))))
     (lambda (condition) (push condition conditions)))
    (should (equal (nreverse visited) '(first failing quitting last)))
    (should (equal (mapcar #'car (nreverse conditions)) '(error quit)))))

(ert-deftest appkit-cleanup-items-continue-during-throw ()
  (let (visited)
    (should
     (eq
      (catch 'escape
        (appkit--run-cleanup-items
         '(first escaping last)
         (lambda (item)
           (push item visited)
           (when (eq item 'escaping)
             (throw 'escape 'escaped)))
         #'ignore)
        'returned)
      'escaped))
    (should (equal (nreverse visited) '(first escaping last)))))

(provide 'appkit-core-test)

;;; appkit-core-test.el ends here
