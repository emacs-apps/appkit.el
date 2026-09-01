;;; appkit-transaction.el --- Buffer mutation boundaries  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Separate generated content mutation, display-property mutation, editable
;; user mutation, and explicitly audited raw mutation.

;;; Code:

(require 'appkit-core)
(require 'appkit-surface)

(defmacro appkit-with-content-update (surface &rest body)
  "Run BODY as an undo-free generated content update for SURFACE."
  (declare (indent 1) (debug t))
  `(let ((appkit-transaction-surface ,surface))
     (unless (appkit-surface--owns-host-p appkit-transaction-surface)
       (error "Cannot mutate an unavailable Appkit Surface"))
     (with-current-buffer (appkit-surface-buffer appkit-transaction-surface)
       (let ((inhibit-read-only t)
             (buffer-undo-list t)
             (appkit-old-modified-p (buffer-modified-p)))
         (unwind-protect
             (progn ,@body)
           (set-buffer-modified-p appkit-old-modified-p))))))

(defmacro appkit-with-property-update (surface &rest body)
  "Run BODY as a property-only update for SURFACE."
  (declare (indent 1) (debug t))
  `(let ((appkit-transaction-surface ,surface))
     (unless (appkit-surface--owns-host-p appkit-transaction-surface)
       (error "Cannot patch properties in an unavailable Appkit Surface"))
     (with-current-buffer (appkit-surface-buffer appkit-transaction-surface)
       (let ((appkit-old-size (buffer-size))
             (appkit-old-tick (buffer-chars-modified-tick)))
         (prog1
             (with-silent-modifications ,@body)
           (when (and appkit-strict-boundaries
                      (or (/= appkit-old-size (buffer-size))
                          (/= appkit-old-tick
                              (buffer-chars-modified-tick))))
             (error "Appkit property transaction changed buffer text")))))))

(defmacro appkit-with-edit-transaction (surface &rest body)
  "Run BODY as an ordinary undoable edit in SURFACE."
  (declare (indent 1) (debug t))
  `(let ((appkit-transaction-surface ,surface))
     (unless (appkit-surface--owns-host-p appkit-transaction-surface)
       (error "Cannot edit an unavailable Appkit Surface"))
     (with-current-buffer (appkit-surface-buffer appkit-transaction-surface)
       (atomic-change-group ,@body))))

(defmacro appkit-with-raw-buffer-mutation (surface reason &rest body)
  "Run BODY as an audited raw mutation for SURFACE, recording REASON in debug."
  (declare (indent 2) (debug t))
  `(let ((appkit-transaction-surface ,surface)
         (appkit-raw-reason ,reason))
     (unless (appkit-surface--owns-host-p appkit-transaction-surface)
       (error "Cannot raw-mutate an unavailable Appkit Surface"))
     (when appkit-debug
       (message "appkit: raw buffer mutation (%s)" appkit-raw-reason))
     (with-current-buffer (appkit-surface-buffer appkit-transaction-surface)
       (let ((inhibit-read-only t)) ,@body))))

(provide 'appkit-transaction)

;;; appkit-transaction.el ends here
