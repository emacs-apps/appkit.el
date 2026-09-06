;;; appkit-cleanup.el --- Reliable lifecycle cleanup  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Small, Emacs-native helpers for cleanup that must continue across individual
;; failures.  `unwind-protect' handles arbitrary nonlocal exits; ordinary error
;; and quit conditions remain available to the owning lifecycle boundary.

;;; Code:

(require 'cl-generic)

(cl-defgeneric appkit-owner-live-p (owner)
  "Return non-nil when lifecycle OWNER can accept owned work.")

(cl-defgeneric appkit-owner-app (owner)
  "Return the canonical App containing OWNER, or nil.")

(cl-defgeneric appkit-owner-handles (owner)
  "Return lifecycle handles currently registered under OWNER.")

(cl-defgeneric appkit-owner-set-handles (owner handles)
  "Replace lifecycle HANDLES registered under OWNER.")

(cl-defmethod appkit-owner-live-p ((_owner t)) nil)

(cl-defmethod appkit-owner-app ((_owner t)) nil)

(cl-defmethod appkit-owner-handles ((owner t))
  (error "Invalid Appkit lifecycle owner: %S" owner))

(cl-defmethod appkit-owner-set-handles ((owner t) _handles)
  (error "Invalid Appkit lifecycle owner: %S" owner))

(defun appkit--run-cleanup-items (items function condition-function)
  "Apply FUNCTION to every element of ITEMS during lifecycle cleanup.

ERROR and QUIT conditions are passed to CONDITION-FUNCTION.  If FUNCTION exits
through another nonlocal transfer, remaining items still run during unwinding
before that transfer resumes."
  (let ((remaining items)
        complete-p)
    (unwind-protect
        (progn
          (while remaining
            (let ((item (pop remaining)))
              (condition-case condition
                  (funcall function item)
                ((error quit)
                 (funcall condition-function condition)))))
          (setq complete-p t))
      ;; Recursion occurs only during an arbitrary nonlocal transfer.  Normal
      ;; cleanup remains iterative and independent of `max-lisp-eval-depth'.
      (unless complete-p
        (appkit--run-cleanup-items
         remaining function condition-function)))))

(defmacro appkit--run-cleanup-forms (conditions &rest forms)
  "Evaluate all FORMS, pushing error and quit conditions onto CONDITIONS.

Nested `unwind-protect' forms ensure that an arbitrary nonlocal exit from one
form does not skip the forms after it."
  (declare (indent 1) (debug (symbolp body)))
  (when forms
    `(unwind-protect
         (condition-case condition
             ,(car forms)
           ((error quit)
            (push condition ,conditions)))
       (appkit--run-cleanup-forms ,conditions ,@(cdr forms)))))

(defun appkit--warn-cleanup-conditions (conditions category)
  "Report cleanup CONDITIONS as warnings under CATEGORY."
  (dolist (condition conditions)
    (display-warning
     category
     (format "Cleanup failed: %s" (error-message-string condition))
     :warning)))

(provide 'appkit-cleanup)

;;; appkit-cleanup.el ends here
