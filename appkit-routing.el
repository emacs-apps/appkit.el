;;; appkit-routing.el --- Exact runtime message targets  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Opaque, incarnation-exact targets for asynchronous cross-runtime posts.
;; Addresses and reply routes never expose a callable runtime entry point;
;; admission remains bounded by the target loop.

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

(defun appkit-loop-address (loop)
  "Return an exact runtime address for LOOP's current incarnation."
  (appkit-loop--check loop)
  (appkit-runtime-address--create
   :owner loop
   :owner-identity (appkit-loop-owner-identity loop)
   :incarnation (appkit-loop-incarnation loop)))

(defun appkit-reply-route-create (target correlation)
  "Return an opaque reply route to exact runtime address TARGET.

CORRELATION is retained as opaque routing metadata; it does not grant access to
TARGET's owner or alter the posted domain message."
  (unless (appkit-runtime-address-p target)
    (signal 'wrong-type-argument (list 'appkit-runtime-address-p target)))
  (appkit-reply-route--create :target target :correlation correlation))

(defun appkit-post-message (target message)
  "Try to enqueue MESSAGE at exact TARGET without invoking target code.

TARGET is an `appkit-runtime-address' or `appkit-reply-route'.  Return one of
`enqueued', `full', `stale', `stopped', or `faulted'.  Failed admission does
not allocate a target sequence number."
  (when appkit-loop--active-loop
    (error "Directed posts must be deferred until after the active pass"))
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
