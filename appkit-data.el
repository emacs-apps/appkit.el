;;; appkit-data.el --- Functional updates for model values  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Copy the plist spines on edited paths, sharing untouched values.  A batch
;; groups writes to the same branch so each changed plist is copied once.

;;; Code:

(defun appkit-data--plist-with (plist fields)
  "Apply non-overlapping path/value FIELDS to keyword PLIST."
  (if (null fields)
      plist
    (let ((size (proper-list-p plist)) groups)
      (unless (and size (zerop (% size 2)))
        (error "Expected a keyword plist, got %S" plist))
      (let ((tail plist))
        (while tail
          (unless (keywordp (car tail))
            (error "Expected a keyword plist, got %S" plist))
          (setq tail (cddr tail))))
      (dolist (field fields)
        (let* ((key (caar field))
               (child (cons (cdar field) (cdr field)))
               (group (assq key groups)))
          (unless (keywordp key)
            (error "Plist path key is not a keyword: %S" key))
          (if group
              (push child (cdr group))
            (push (list key child) groups))))
      (let ((result (copy-sequence plist)))
        (dolist (group (nreverse groups) result)
          (let* ((writes (nreverse (cdr group)))
                 (leaf (assq nil writes))
                 (value
                  (if leaf
                      (if (cdr writes)
                          (error "Duplicate or overlapping plist writes at %S" (car group))
                        (cdr leaf))
                    (appkit-data--plist-with (plist-get plist (car group)) writes))))
            (setq result (plist-put result (car group) value))))))))

(defmacro appkit-plist-with (plist &rest patches)
  "Return PLIST with the field updates described by PATCHES.

Each patch is (PATH KEY VALUE...), where PATH is a list of key
expressions selecting a nested keyword plist and each KEY is a literal
keyword naming a field to replace.  An empty PATH selects PLIST itself:

  (appkit-plist-with state
    (() :items items)
    ((:status) :phase phase :token nil)
    ((:page) :number page :total total))

PLIST, path keys and values are evaluated once in source order.
Duplicate field paths and writes to both a field and its descendants
signal an error.  Missing branches are created from nil; traversing a
non-plist signals an error.

Each changed plist spine is copied once.  Unchanged branches and assigned
values are shared, so callers must continue to treat those values as
immutable.  With no field updates, return PLIST unchanged."
  (declare (indent 1))
  (let* ((root (make-symbol "plist"))
         (bindings (list (list root plist)))
         fields)
    (dolist (patch patches)
      (unless (and (proper-list-p patch) (consp patch)
                   (proper-list-p (car patch))
                   (zerop (% (length (cdr patch)) 2)))
        (error "Invalid plist patch: %S" patch))
      (let ((path (make-symbol "path"))
            (properties (cdr patch)))
        (push (list path (cons 'list (car patch))) bindings)
        (while properties
          (let ((key (pop properties))
                (value (pop properties))
                (temporary (make-symbol "value")))
            (unless (keywordp key)
              (error "Plist field is not a literal keyword: %S" key))
            (push (list temporary value) bindings)
            (push `(cons (append ,path (list ,key)) ,temporary) fields)))))
    `(let* ,(nreverse bindings)
       (appkit-data--plist-with ,root (list ,@(nreverse fields))))))

(provide 'appkit-data)

;;; appkit-data.el ends here
