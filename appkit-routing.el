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
  owner
  owner-identity
  incarnation)

(cl-defstruct (appkit-reply-route
               (:constructor appkit-reply-route--create)
               (:copier nil)
               (:conc-name appkit-reply-route--))
  "An opaque exact target paired with client CORRELATION."
  target
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

(defun appkit-routing--post (target message)
  "Try to enqueue MESSAGE at exact internal TARGET.

Return `enqueued', `full', `stale', `stopped', or `faulted'.  Failed admission
does not allocate a target sequence number.  Client transitions must instead
return a closed post command for execution after commit."
  (when appkit-loop--active-loop
    (error "Directed posts must use the runtime post-commit phase"))
  (let* ((route (and (appkit-reply-route-p target) target))
         (address (if route (appkit-reply-route--target route) target)))
    (unless (appkit-runtime-address-p address)
      (signal 'wrong-type-argument
              (list '(or appkit-runtime-address-p appkit-reply-route-p)
                    target)))
    (appkit-loop--post-addressed
     (appkit-runtime-address--owner address)
     message
     (appkit-runtime-address--incarnation address)
     route)))

(provide 'appkit-routing)

;;; appkit-routing.el ends here
