;;; appkit-data.el --- Functional updates for model values -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Records, keyword plists and persistent maps share one path update contract.
;; Copy only edited spines; batch edits of one map share their writable nodes.
;; Stored children are shared immutable values, not recursively frozen copies.

;;; Code:

(require 'cl-lib)
(require 'appkit-data-map)

(defun appkit-data--slot (key)
  "Return the record slot named by symbol or keyword KEY."
  (unless (symbolp key) (error "Record field is not a symbol: %S" key))
  (if (keywordp key) (intern (substring (symbol-name key) 1)) key))

(defmacro appkit-data-step (state expression)
  "Thread reducer EXPRESSION through local STATE and return its value.
Arguments run before STATE is read, so nested reducers compose in order."
  (declare (indent 1))
  (unless (eq state (cadr expression))
    (error "Reducer must receive the state being threaded"))
  (let ((arguments (mapcar (lambda (value) (list (make-symbol "arg") value))
                           (cddr expression)))
        (next (make-symbol "next")))
    `(let* (,@arguments
            (,next (,(car expression) ,state ,@(mapcar #'car arguments))))
       (setq ,state (car ,next) ,next (cdr ,next)))))

(defun appkit-data-get (value key &optional default)
  "Read KEY from a record, keyword plist or persistent map VALUE.
DEFAULT applies to missing map/plist keys.  Unknown record fields signal."
  (cond
   ((appkit-data-map-p value) (appkit-data-map-get value key default))
   ((recordp value)
    (aref value (cl-struct-slot-offset (type-of value) (appkit-data--slot key))))
   ((listp value)
    (if (plist-member value key) (plist-get value key) default))
   (t (error "Not an Appkit data container: %S" value))))

(defun appkit-data--plist-check (value)
  "Validate keyword plist VALUE before updating it."
  (let ((size (proper-list-p value)) (tail value))
    (unless (and size (zerop (% size 2)))
      (error "Expected a keyword plist, got %S" value))
    (while tail
      (unless (keywordp (car tail))
        (error "Expected a keyword plist, got %S" value))
      (setq tail (cddr tail)))))

(defun appkit-data--with (value fields)
  "Apply non-overlapping path/value FIELDS to container VALUE."
  (if (null fields) value
    (let* ((map-p (appkit-data-map-p value))
           (record-p (and (not map-p) (recordp value)))
           groups changes)
      (unless (or map-p record-p) (appkit-data--plist-check value))
      (dolist (field fields)
        (let* ((key (if record-p (appkit-data--slot (caar field)) (caar field)))
               (child (cons (cdar field) (cdr field)))
               (group (if map-p (cl-assoc key groups :test #'equal) (assq key groups))))
          (unless (or map-p record-p (keywordp key))
            (error "Plist path key is not a keyword: %S" key))
          (if group (push child (cdr group))
            (push (list key child) groups))))
      (dolist (group (nreverse groups))
        (let* ((key (car group)) (writes (nreverse (cdr group)))
               (leaf (assq nil writes))
               (next
                (if leaf
                    (if (cdr writes) (error "Duplicate or overlapping data writes at %S" key)
                      (cdr leaf))
                  (appkit-data--with (appkit-data-get value key) writes))))
          (when (and record-p (eq next appkit-data-delete))
            (error "Cannot delete record field %S" key))
          ;; Reading also validates record fields when replacing a whole field.
          (let ((old (appkit-data-get value key appkit-data-absent)))
            (unless (or (and (eq old appkit-data-absent) (eq next appkit-data-delete))
                        (equal old next))
              (push (cons key next) changes)))))
      (if (null changes) value
        (if map-p (appkit-data-map-edit value (nreverse changes))
          (let ((next (copy-sequence value)))
            (dolist (change (nreverse changes) next)
              (if record-p
                  (aset next (cl-struct-slot-offset (type-of value) (car change)) (cdr change))
                (if (eq (cdr change) appkit-data-delete) (cl-remf next (car change))
                  (setq next (plist-put next (car change) (cdr change))))))))))))

(defmacro appkit-data-with (value &rest patches)
  "Return VALUE with PATCHES applied without changing its old contents.
Each patch is (PATH KEY VALUE...), with PATH a list of key expressions.
An empty PATH selects the root:

  (appkit-data-with model
    (() :items items)
    ((:status) :phase phase :token nil)
    ((:entities) entity-key entity))

Records use field symbols or keywords, keyword plists use keywords, and
persistent maps accept arbitrary equal keys.  VALUE, paths, keys and values
are evaluated once in source order.  Missing branches start as nil plists.
The value `appkit-data-delete' removes a map/plist key; record fields are
fixed and cannot be deleted.  Duplicate and overlapping paths signal.

Each edited record/plist spine is copied once; maps copy only touched trie
paths, sharing edits in the batch.  Unchanged children and assigned values
are shared and must be treated as immutable.  An empty/no-op update reuses
VALUE.  Record constructors, accessors and inheritance remain unchanged."
  (declare (indent 1))
  (let* ((root (make-symbol "value")) (bindings (list (list root value))) fields)
    (dolist (patch patches)
      (unless (and (proper-list-p patch) (consp patch) (proper-list-p (car patch))
                   (zerop (% (length (cdr patch)) 2)))
        (error "Invalid data patch: %S" patch))
      (let ((path (make-symbol "path")) (properties (cdr patch)))
        (push (list path (cons 'list (car patch))) bindings)
        (while properties
          (let ((key (make-symbol "key")) (next (make-symbol "next")))
            (push (list key (pop properties)) bindings)
            (push (list next (pop properties)) bindings)
            (push `(cons (append ,path (list ,key)) ,next) fields)))))
    `(let* ,(nreverse bindings)
       (appkit-data--with ,root (list ,@(nreverse fields))))))

(provide 'appkit-data)
;;; appkit-data.el ends here
