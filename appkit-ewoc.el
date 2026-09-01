;;; appkit-ewoc.el --- Stable-key EWOC projection helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Public-API-only keyed EWOC reconciliation and targeted invalidation.

;;; Code:

(require 'cl-lib)
(require 'ewoc)

(defun appkit-ewoc--key-set (keys)
  "Return an equal-tested set containing KEYS."
  (let ((set (make-hash-table :test #'equal)))
    (dolist (key keys set) (puthash key t set))))

(defun appkit-ewoc--nodes (ewoc key-function)
  "Return stable key to node table for EWOC using KEY-FUNCTION."
  (let ((nodes (make-hash-table :test #'equal))
        (node (ewoc-nth ewoc 0)))
    (while node
      (let ((key (funcall key-function (ewoc-data node))))
        (unless key (error "Appkit EWOC entry has no stable key"))
        (when (gethash key nodes)
          (error "Appkit EWOC has duplicate key %S" key))
        (puthash key node nodes))
      (setq node (ewoc-next ewoc node)))
    nodes))

(defun appkit-ewoc--validate-entries (entries key-function)
  "Require one unique stable key for every item in ENTRIES.
KEY-FUNCTION extracts that opaque key from an entry."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (let ((key (funcall key-function entry)))
        (unless key (error "Appkit projected entry has no stable key"))
        (when (gethash key seen)
          (error "Appkit projected entries duplicate key %S" key))
        (puthash key t seen)))))

(cl-defun appkit-ewoc-reconcile (ewoc entries key-function &key force-keys)
  "Reconcile keyed EWOC with ENTRIES and return its new node table.
KEY-FUNCTION extracts stable keys, and FORCE-KEYS names retained rows whose
printers must run again."
  (appkit-ewoc--validate-entries entries key-function)
  (let* ((available (appkit-ewoc--nodes ewoc key-function))
         (target-keys (appkit-ewoc--key-set (mapcar key-function entries)))
         (forced (appkit-ewoc--key-set force-keys))
         stale-keys
         next-node
         (new-table (make-hash-table :test #'equal)))
    (maphash (lambda (key _node)
               (unless (gethash key target-keys) (push key stale-keys)))
             available)
    (dolist (key stale-keys)
      (ewoc-delete ewoc (gethash key available))
      (remhash key available))
    (setq next-node (ewoc-nth ewoc 0))
    (dolist (entry entries)
      (let* ((key (funcall key-function entry))
             (existing (gethash key available))
             node)
        (cond
         ((and existing (eq existing next-node))
          (setq node existing
                next-node (ewoc-next ewoc next-node))
          (when (or (gethash key forced)
                    (not (equal (ewoc-data node) entry)))
            (ewoc-set-data node entry)
            (ewoc-invalidate ewoc node)))
         (existing
          (ewoc-delete ewoc existing)
          (setq node (if next-node
                         (ewoc-enter-before ewoc next-node entry)
                       (ewoc-enter-last ewoc entry))))
         (t
          (setq node (if next-node
                         (ewoc-enter-before ewoc next-node entry)
                       (ewoc-enter-last ewoc entry)))))
        (remhash key available)
        (puthash key node new-table)))
    (maphash (lambda (_key node) (ewoc-delete ewoc node)) available)
    new-table))

(defun appkit-ewoc-invalidate-key (ewoc node-table key)
  "Invalidate KEY in EWOC using NODE-TABLE."
  (when-let* ((node (and (hash-table-p node-table)
                         (gethash key node-table))))
    (ewoc-invalidate ewoc node)
    t))

(provide 'appkit-ewoc)

;;; appkit-ewoc.el ends here
