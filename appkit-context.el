;;; appkit-context.el --- Runtime transition contexts  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Opaque per-transition routing context and immutable-by-contract App read
;; views.  Context values expose no loop, registry, buffer, or transport handle.

;;; Code:

(require 'cl-lib)
(require 'appkit-loop)
(require 'appkit-routing)

(cl-defstruct (appkit-app-read-view
               (:constructor appkit-app-read-view--create)
               (:copier nil))
  "One committed App snapshot captured for a Surface pass."
  (app-identity nil :read-only t)
  (incarnation nil :read-only t)
  (revision nil :read-only t)
  (model nil :read-only t))

(cl-defstruct (appkit-transition-context
               (:constructor appkit-transition-context--create)
               (:copier nil))
  "Opaque capabilities and causal metadata for one transition."
  (owner-address nil :read-only t)
  (parent-address nil :read-only t)
  (origin nil :read-only t)
  (source-address nil :read-only t)
  (source-revision nil :read-only t)
  (reply-route nil :read-only t)
  (app-read-view nil :read-only t))

(defun appkit-context--for-loop (loop &optional parent-address app-read-view)
  "Create the current transition context for LOOP.

PARENT-ADDRESS and APP-READ-VIEW are non-nil only for an attached Surface."
  (let ((envelope appkit-loop--current-envelope))
    (appkit-transition-context--create
     :owner-address (appkit-routing--address loop)
     :parent-address parent-address
     :origin (and envelope (appkit-loop-envelope-origin envelope))
     :source-address
     (and envelope (appkit-loop-envelope-source-address envelope))
     :source-revision
     (and envelope (appkit-loop-envelope-source-revision envelope))
     :reply-route (and envelope (appkit-loop-envelope-reply-route envelope))
     :app-read-view app-read-view)))

(provide 'appkit-context)

;;; appkit-context.el ends here
