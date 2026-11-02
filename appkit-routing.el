;;; appkit-routing.el --- Exact runtime message targets  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-apps/appkit.el

;;; Commentary:

;; Internal exact-target admission for runtime post phases and callback gates.
;; Client code only receives opaque addresses and reply routes through a
;; transition context and can use them only in closed commands.

;;; Code:

(require 'cl-lib)
(require 'appkit-loop)
(require 'appkit-effect)

(cl-defstruct (appkit-runtime-address
               (:constructor appkit-runtime-address--create)
               (:copier nil)
               (:conc-name appkit-runtime-address--))
  "An opaque address for one exact owner incarnation."
  (owner nil :read-only t)
  (owner-identity nil :read-only t)
  (incarnation nil :read-only t))

(cl-defstruct (appkit-reply-route
               (:constructor appkit-reply-route--create)
               (:copier nil)
               (:conc-name appkit-reply-route--))
  "An opaque exact target paired with client CORRELATION."
  (target nil :read-only t)
  (correlation nil :read-only t)
  (request-scope nil :read-only t))

(cl-defstruct (appkit-delivery-report
               (:constructor appkit-delivery-report--create)
               (:copier nil))
  "One bounded report for a failed directed command admission."
  outcome
  target
  message
  correlation
  reason)

(defun appkit-routing--address (loop)
  "Return an exact internal runtime address for LOOP's current incarnation."
  (appkit-loop--check loop)
  (appkit-runtime-address--create
   :owner loop
   :owner-identity (appkit-loop--owner-identity loop)
   :incarnation (appkit-loop-incarnation loop)))

(defun appkit-routing--reply-route (target correlation &optional request-scope)
  "Return an internal reply route to exact runtime address TARGET."
  (unless (appkit-runtime-address-p target)
    (signal 'wrong-type-argument (list 'appkit-runtime-address-p target)))
  (appkit-reply-route--create :target target :correlation correlation
                               :request-scope request-scope))

(defun appkit-routing--target-address (target)
  "Return TARGET's exact runtime address, or signal a type error."
  (cond
   ((appkit-runtime-address-p target) target)
   ((appkit-reply-route-p target) (appkit-reply-route--target target))
   (t
    (signal 'wrong-type-argument
            (list '(or appkit-runtime-address-p appkit-reply-route-p)
                  target)))))

(defun appkit-routing--admit
    (target message &optional origin source-address source-revision reply-route)
  "Admit MESSAGE at exact TARGET with optional causal metadata."
  (let ((address (appkit-routing--target-address target)))
    (appkit-loop--post-addressed
     (appkit-runtime-address--owner address)
     message
     (appkit-runtime-address--incarnation address)
     reply-route
     origin
     source-address
     source-revision)))

(defun appkit-routing--post (target message)
  "Try to enqueue MESSAGE at exact internal TARGET.

Return `enqueued', `full', `stale', `stopped', or `faulted'.  Failed admission
does not allocate a target sequence number.  Update functions must instead
return a closed post command for execution after commit."
  (when appkit-loop--current-envelope
    (signal 'appkit-runtime-contract-error
            (list "Loop updates must return post commands")))
  (appkit-routing--admit target message))

(defun appkit-routing--post-command
    (sender target message source-revision reply-correlation &optional request-scope)
  "Execute a routed command after SOURCE-REVISION commits.
Request authority is inherited by every directed hop and return route.
Obsolete request work is discarded before admission and checked again before
client dispatch.  Request failures return to the request owner's control lane;
ordinary routing failures still report to SENDER."
  (let* ((route-scope (and (appkit-reply-route-p target)
                           (appkit-reply-route--request-scope target)))
         (scope (or route-scope request-scope)))
    (when (and route-scope request-scope (not (eq route-scope request-scope)))
      (error "Cannot cross request scopes in a reply"))
    (if (and scope (not (appkit-effect--current-p scope)))
        'superseded
      (let* ((source-address (appkit-routing--address sender))
             (request-route
              (and (or reply-correlation scope)
                   (appkit-runtime-address-p target)
                   (appkit-routing--reply-route
                    source-address reply-correlation scope)))
             (outcome
              (appkit-routing--admit
               target
               (if scope
                   (appkit-effect--scoped-message-create :scope scope :payload message)
                 message)
               'command source-address source-revision request-route)))
        (unless (eq outcome 'enqueued)
          (if scope
              (appkit-routing--report-request
               scope target message outcome nil
               (or reply-correlation
                   (and (appkit-reply-route-p target)
                        (appkit-reply-route--correlation target))))
            (let ((report-outcome
                   (appkit-loop--post-control-addressed
                    sender
                    (appkit-delivery-report--create
                     :outcome outcome :target target :message message
                     :correlation reply-correlation)
                    (appkit-loop-incarnation sender) nil 'runtime)))
              (unless (eq report-outcome 'enqueued)
                (error "Required delivery report admission failed: %S" report-outcome)))))
        outcome))))

(defun appkit-routing--report-request (scope target message outcome &optional reason correlation)
  "Report an admitted request's failure to its originating owner.
The control lane can deliver the failure even when the final reply's data
lane is full.  Authority remains live until the owner handles the report."
  (when (appkit-effect--current-p scope)
    (let* ((owner (appkit-effect--runtime-loop (appkit-effect--instance-runtime scope)))
           (report (appkit-delivery-report--create
                    :outcome outcome :target target :message message :reason reason
                    :correlation (or correlation (appkit-effect--instance-key scope))))
           (admission
            (appkit-loop--post-control-addressed
             owner (appkit-effect--scoped-message-create :scope scope :payload report)
             (appkit-loop-incarnation owner) nil 'runtime)))
      (unless (eq admission 'enqueued)
        (error "Required request report admission failed: %S" admission)))))

(provide 'appkit-routing)

;;; appkit-routing.el ends here
