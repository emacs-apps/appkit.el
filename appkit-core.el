;;; appkit-core.el --- App lifecycle handles  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Generic lifecycle handles owned by the canonical App runtime.  Legacy App,
;; View, operation, event-bus, and request-table control planes are removed.

;;; Code:

(require 'cl-lib)
(require 'appkit-cleanup)

(defgroup appkit nil
  "Runtime primitives for stateful Emacs buffer applications."
  :group 'applications)

(defcustom appkit-debug nil
  "When non-nil, validate runtime ownership and mutation invariants."
  :type 'boolean
  :group 'appkit)

(defcustom appkit-strict-boundaries nil
  "When non-nil, signal errors for invalid runtime boundaries."
  :type 'boolean
  :group 'appkit)

(cl-defstruct (appkit-handle
               (:constructor appkit-handle--create)
               (:copier nil))
  type
  object
  cancel
  owner
  alive-p)

(defun appkit--default-handle-cancel (type object)
  "Cancel lifecycle OBJECT according to TYPE."
  (pcase type
    ('timer
     (when (timerp object) (cancel-timer object)))
    ('process
     (when (processp object)
       (set-process-filter object nil)
       (set-process-sentinel object nil)
       (when (process-live-p object) (delete-process object))))
    ('hook
     (pcase-let ((`(,hook ,function ,local ,buffer) object))
       (if (buffer-live-p buffer)
           (with-current-buffer buffer
             (remove-hook hook function local))
         (remove-hook hook function local))))
    ('function
     (when (functionp object) (funcall object)))))

(cl-defun appkit-register-handle (owner type object &optional cancel-function)
  "Register lifecycle OBJECT of TYPE under live OWNER.

OWNER is a canonical App or Generated Surface.  CANCEL-FUNCTION, when non-nil,
receives OBJECT."
  (unless (appkit-owner-live-p owner)
    (error "Cannot register handle under unavailable owner"))
  (let ((handle
         (appkit-handle--create
          :type type :object object :cancel cancel-function
          :owner owner :alive-p t)))
    (appkit-owner-set-handles
     owner (cons handle (appkit-owner-handles owner)))
    handle))

(defun appkit-retire-handle (handle)
  "Retire lifecycle HANDLE without invoking cancellation."
  (when (and (appkit-handle-p handle) (appkit-handle-alive-p handle))
    (setf (appkit-handle-alive-p handle) nil)
    (when-let* ((owner (appkit-handle-owner handle)))
      (appkit-owner-set-handles
       owner (delq handle (appkit-owner-handles owner))))
    t))

(defun appkit-cancel-handle (handle)
  "Cancel HANDLE exactly once."
  (when (and (appkit-handle-p handle) (appkit-handle-alive-p handle))
    (setf (appkit-handle-alive-p handle) nil)
    (unwind-protect
        (let ((cancel (appkit-handle-cancel handle))
              (object (appkit-handle-object handle)))
          (if (functionp cancel)
              (funcall cancel object)
            (appkit--default-handle-cancel
             (appkit-handle-type handle) object)))
      (when-let* ((owner (appkit-handle-owner handle)))
        (appkit-owner-set-handles
         owner (delq handle (appkit-owner-handles owner)))))
    t))

(defun appkit-cancel-handles (owner)
  "Cancel and forget all lifecycle handles owned by OWNER."
  (let ((handles (appkit-owner-handles owner))
        conditions)
    (appkit-owner-set-handles owner nil)
    (appkit--run-cleanup-items
     handles #'appkit-cancel-handle
     (lambda (condition) (push condition conditions)))
    (setq conditions (nreverse conditions))
    (appkit--warn-cleanup-conditions (cdr conditions) 'appkit-core)
    (when-let* ((condition (car conditions)))
      (signal (car condition) (cdr condition)))))

(provide 'appkit-core)

;;; appkit-core.el ends here
