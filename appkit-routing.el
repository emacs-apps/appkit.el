;;; appkit-routing.el --- Exact runtime message targets  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Internal exact-target admission for runtime post phases and callback gates.
;; Client code only receives opaque addresses and reply routes through a
;; transition context and can use them only in closed commands.

;;; Code:

(require 'cl-lib)
(require 'appkit-loop)

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
  (correlation nil :read-only t))

(cl-defstruct (appkit-delivery-report
               (:constructor appkit-delivery-report--create)
               (:copier nil))
  "One bounded report for a failed directed command admission."
  outcome
  target
  message
  correlation)

(defun appkit-routing--address (loop)
  "Return an exact internal runtime address for LOOP's current incarnation."
  (appkit-loop--check loop)
  (appkit-runtime-address--create
   :owner loop
   :owner-identity (appkit-loop--owner-identity loop)
   :incarnation (appkit-loop-incarnation loop)))

(defun appkit-routing--reply-route (target correlation)
  "Return an internal reply route to exact runtime address TARGET."
  (unless (appkit-runtime-address-p target)
    (signal 'wrong-type-argument (list 'appkit-runtime-address-p target)))
  (appkit-reply-route--create :target target :correlation correlation))

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
does not allocate a target sequence number.  Client transitions must instead
return a closed post command for execution after commit."
  (when appkit-loop--active-loop
    (error "Directed posts must use the runtime post-commit phase"))
  (appkit-routing--admit target message))

(defun appkit-routing--post-command
    (sender target message source-revision reply-correlation)
  "Execute one routed command from SENDER after SOURCE-REVISION commits.

Failed target admission produces one bounded delivery report in SENDER's next
pass.  Failure to admit that required report is a runtime fault."
  (let* ((source-address (appkit-routing--address sender))
         (request-route
          (and reply-correlation
               (appkit-runtime-address-p target)
               (appkit-routing--reply-route
                source-address reply-correlation)))
         (outcome
          (appkit-routing--admit
           target message 'command source-address source-revision
           request-route)))
    (unless (eq outcome 'enqueued)
      (let ((report-outcome
             (appkit-loop--post-control-addressed
              sender
              (appkit-delivery-report--create
               :outcome outcome
               :target target
               :message message
               :correlation reply-correlation)
              (appkit-loop-incarnation sender)
              nil 'runtime)))
        (unless (eq report-outcome 'enqueued)
          (error "Required delivery report admission failed: %S"
                 report-outcome))))
    outcome))

(provide 'appkit-routing)

;;; appkit-routing.el ends here
