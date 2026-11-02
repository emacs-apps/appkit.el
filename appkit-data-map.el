;;; appkit-data-map.el --- Persistent keyed values -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A bitmap hash trie over equal keys.  Persistent roots share untouched nodes.
;; Each batch gets a fresh edit identity: nodes copied during that batch can be
;; edited again, but a node reachable from an earlier root is never changed.
;; Keys and stored values are borrowed immutable values, not deep copies.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defconst appkit-data-absent (make-symbol "appkit-data-absent")
  "Missing value in a data-map diff; distinct from a stored nil.")

(defconst appkit-data-delete (make-symbol "appkit-data-delete")
  "Deletion marker in data updates; cannot be stored as a map value.")

(cl-defstruct (appkit-data-map (:constructor appkit-data-map--create)
                              (:copier nil))
  (root nil :read-only t)
  (count 0 :read-only t))

(cl-defstruct (appkit-data--branch (:constructor appkit-data--branch-create))
  bitmap children edit)

(cl-defstruct (appkit-data--entry (:constructor appkit-data--entry-create))
  (hash nil :read-only t) (key nil :read-only t) (value nil :read-only t))

(cl-defstruct (appkit-data--collision (:constructor appkit-data--collision-create))
  (hash nil :read-only t) (entries nil :read-only t))

(defconst appkit-data-map-empty (appkit-data-map--create)
  "The empty persistent map.  Keys use `equal' and `sxhash-equal'.")

(defun appkit-data--hash (key)
  "Return KEY's hash."
  (sxhash-equal key))

(defun appkit-data--bit (hash shift)
  "Return the branch bit for HASH at SHIFT."
  (ash 1 (logand 31 (ash hash (- shift)))))

(defun appkit-data--index (bitmap bit)
  "Return BIT's compact vector index in BITMAP."
  (logcount (logand bitmap (1- bit))))

(defun appkit-data--node-hash (node)
  "Return the full hash of a leaf or collision NODE."
  (if (appkit-data--entry-p node)
      (appkit-data--entry-hash node)
    (appkit-data--collision-hash node)))

(defun appkit-data--find (node hash key shift default)
  "Find KEY with HASH in NODE at SHIFT, or DEFAULT."
  (while (appkit-data--branch-p node)
    (let* ((bit (appkit-data--bit hash shift))
           (bitmap (appkit-data--branch-bitmap node)))
      (setq node (and (not (zerop (logand bit bitmap)))
                      (aref (appkit-data--branch-children node)
                            (appkit-data--index bitmap bit)))
            shift (+ shift 5))))
  (cond
   ((appkit-data--entry-p node)
    (if (and (= hash (appkit-data--entry-hash node))
             (equal key (appkit-data--entry-key node)))
        (appkit-data--entry-value node) default))
   ((and (appkit-data--collision-p node)
         (= hash (appkit-data--collision-hash node)))
    (let ((entries (appkit-data--collision-entries node)) (i 0) (value default))
      (while (< i (length entries))
        (let ((entry (aref entries i)))
          (if (equal key (appkit-data--entry-key entry))
              (setq value (appkit-data--entry-value entry) i (length entries))
            (setq i (1+ i)))))
      value))
   (t default)))

(defun appkit-data-map-get (map key &optional default)
  "Return MAP's value for KEY, or DEFAULT when absent."
  (cl-check-type map appkit-data-map)
  (appkit-data--find (appkit-data-map-root map) (appkit-data--hash key) key 0 default))

(defun appkit-data--splice (vector index remove &optional item)
  "Copy VECTOR, removing REMOVE elements at INDEX and inserting ITEM if present."
  (vconcat (seq-subseq vector 0 index) item
           (seq-subseq vector (+ index remove))))

(defun appkit-data--join (node hash other other-hash shift edit)
  "Join differently hashed NODE and OTHER at SHIFT under EDIT."
  (let ((bit (appkit-data--bit hash shift))
        (other-bit (appkit-data--bit other-hash shift)))
    (appkit-data--branch-create
     :bitmap (logior bit other-bit) :edit edit
     :children (if (= bit other-bit)
                   (vector (appkit-data--join node hash other other-hash (+ shift 5) edit))
                 (if (< bit other-bit) (vector node other) (vector other node))))))

(defun appkit-data--writable (node edit)
  "Return NODE if owned by EDIT, otherwise a copy of its editable spine."
  (if (eq edit (appkit-data--branch-edit node)) node
    (appkit-data--branch-create :bitmap (appkit-data--branch-bitmap node)
                               :children (copy-sequence (appkit-data--branch-children node))
                               :edit edit)))

(defun appkit-data--change (node hash key value shift edit)
  "Return NODE after one KEY/VALUE change at SHIFT under EDIT."
  (cond
   ((null node)
    (unless (eq value appkit-data-delete)
      (appkit-data--entry-create :hash hash :key key :value value)))
   ((appkit-data--branch-p node)
    (let* ((bit (appkit-data--bit hash shift))
           (bitmap (appkit-data--branch-bitmap node))
           (index (appkit-data--index bitmap bit))
           (present (not (zerop (logand bit bitmap))))
           (child (and present (aref (appkit-data--branch-children node) index)))
           (next (appkit-data--change child hash key value (+ shift 5) edit)))
      (if (eq next child) node
        (let* ((result (appkit-data--writable node edit))
               (children (appkit-data--branch-children result)))
          (cond
           ((null next)
            (setq children (appkit-data--splice children index 1))
            (setf (appkit-data--branch-bitmap result) (logxor bitmap bit)))
           ((not present)
            (setq children (appkit-data--splice children index 0 (vector next)))
            (setf (appkit-data--branch-bitmap result) (logior bitmap bit)))
           (t (aset children index next)))
          (setf (appkit-data--branch-children result) children)
          (cond
           ((zerop (length children)) nil)
           ((and (= (length children) 1)
                 (not (appkit-data--branch-p (aref children 0))))
            (aref children 0))
           (t result))))))
   ((/= hash (appkit-data--node-hash node))
    (if (eq value appkit-data-delete) node
      (appkit-data--join
       node (appkit-data--node-hash node)
       (appkit-data--entry-create :hash hash :key key :value value) hash shift edit)))
   ((appkit-data--entry-p node)
    (cond
     ((equal key (appkit-data--entry-key node))
      (cond ((eq value appkit-data-delete) nil)
            ((equal value (appkit-data--entry-value node)) node)
            (t (appkit-data--entry-create :hash hash :key (appkit-data--entry-key node)
                                         :value value))))
     ((eq value appkit-data-delete) node)
     (t (appkit-data--collision-create
         :hash hash :entries (vector node (appkit-data--entry-create
                                          :hash hash :key key :value value))))))
   (t
    (let* ((entries (appkit-data--collision-entries node))
           (index (cl-position key entries :key #'appkit-data--entry-key :test #'equal)))
      (cond
       ((null index)
        (if (eq value appkit-data-delete) node
          (appkit-data--collision-create
           :hash hash :entries (vconcat entries
                                       (vector (appkit-data--entry-create
                                                :hash hash :key key :value value))))))
       ((eq value appkit-data-delete)
        (if (= (length entries) 2) (aref entries (- 1 index))
          (appkit-data--collision-create
           :hash hash :entries (appkit-data--splice entries index 1))))
       ((equal value (appkit-data--entry-value (aref entries index))) node)
       (t
        (let ((next (copy-sequence entries)))
          (aset next index (appkit-data--entry-create
                           :hash hash :key (appkit-data--entry-key (aref entries index))
                           :value value))
          (appkit-data--collision-create :hash hash :entries next))))))))

(defun appkit-data-map-edit (map changes)
  "Return MAP after applying the (KEY . VALUE) pairs in CHANGES in order.
VALUE `appkit-data-delete' removes a key.  Nil is an ordinary stored value.
A batch copies each touched existing branch at most once.  No builder escapes
the call, and failure leaves MAP intact.  Equal replacements reuse old values.
Keys and values must be treated as immutable after insertion."
  (cl-check-type map appkit-data-map)
  (let ((root (appkit-data-map-root map)) (count (appkit-data-map-count map))
        (edit (list nil)))
    (dolist (change changes)
      (unless (consp change) (error "Invalid map change: %S" change))
      (let* ((key (car change)) (value (cdr change)) (hash (appkit-data--hash key))
             (old (appkit-data--find root hash key 0 appkit-data-absent)))
        (when (eq value appkit-data-absent)
          (error "The absent marker is not a map value"))
        (setq root (appkit-data--change root hash key value 0 edit)
              count (+ count (cond ((eq value appkit-data-delete)
                                    (if (eq old appkit-data-absent) 0 -1))
                                   ((eq old appkit-data-absent) 1) (t 0))))))
    (if (eq root (appkit-data-map-root map)) map
      (if (null root) appkit-data-map-empty
        (appkit-data-map--create :root root :count count)))))

(defun appkit-data-map-set (map key value)
  "Return MAP with KEY associated with VALUE."
  (appkit-data-map-edit map (list (cons key value))))

(defun appkit-data-map-remove (map key)
  "Return MAP without KEY."
  (appkit-data-map-set map key appkit-data-delete))

(defun appkit-data--each (function node)
  "Call FUNCTION with each key and value in NODE."
  (cond
   ((appkit-data--branch-p node)
    (mapc (lambda (child) (appkit-data--each function child))
          (appkit-data--branch-children node)))
   ((appkit-data--entry-p node)
    (funcall function (appkit-data--entry-key node) (appkit-data--entry-value node)))
   ((appkit-data--collision-p node)
    (mapc (lambda (entry) (appkit-data--each function entry))
          (appkit-data--collision-entries node)))))

(defun appkit-data-map-each (function map)
  "Call FUNCTION with each key and value in MAP, in unspecified order."
  (cl-check-type map appkit-data-map)
  (appkit-data--each function (appkit-data-map-root map)))

(defun appkit-data-map-keys (map)
  "Return MAP's keys in unspecified order."
  (let (keys)
    (appkit-data-map-each (lambda (key _value) (push key keys)) map)
    keys))

(defun appkit-data--child (node bit shift)
  "Return NODE's child at BIT and SHIFT, treating a leaf as a singleton branch."
  (if (appkit-data--branch-p node)
      (let ((bitmap (appkit-data--branch-bitmap node)))
        (and (not (zerop (logand bit bitmap)))
             (aref (appkit-data--branch-children node) (appkit-data--index bitmap bit))))
    (and node (= bit (appkit-data--bit (appkit-data--node-hash node) shift)) node)))

(defun appkit-data--diff (old new shift emit)
  "Call EMIT for differences between OLD and NEW at SHIFT."
  (unless (eq old new)
    (if (or (appkit-data--branch-p old) (appkit-data--branch-p new))
        (let ((bitmap
               (logior (cond ((appkit-data--branch-p old) (appkit-data--branch-bitmap old))
                             (old (appkit-data--bit (appkit-data--node-hash old) shift))
                             (t 0))
                       (cond ((appkit-data--branch-p new) (appkit-data--branch-bitmap new))
                             (new (appkit-data--bit (appkit-data--node-hash new) shift))
                             (t 0)))))
          (while (not (zerop bitmap))
            (let ((bit (logand bitmap (- bitmap))))
              (appkit-data--diff (appkit-data--child old bit shift)
                                 (appkit-data--child new bit shift) (+ shift 5) emit)
              (setq bitmap (logxor bitmap bit)))))
      (appkit-data--each
       (lambda (key value)
         (let ((next (appkit-data--find new (appkit-data--hash key) key shift appkit-data-absent)))
           (unless (equal value next) (funcall emit key value next))))
       old)
      (appkit-data--each
       (lambda (key value)
         (when (eq (appkit-data--find old (appkit-data--hash key) key shift appkit-data-absent)
                   appkit-data-absent)
           (funcall emit key appkit-data-absent value)))
       new))))

(defun appkit-data-map-diff (old new)
  "Return (KEY OLD-VALUE NEW-VALUE) changes from OLD to NEW.
Absent values are `appkit-data-absent'; stored nil stays nil.  Shared branches
are skipped by identity.  The result's order is unspecified."
  (cl-check-type old appkit-data-map)
  (cl-check-type new appkit-data-map)
  (let (changes)
    (appkit-data--diff (appkit-data-map-root old) (appkit-data-map-root new) 0
                       (lambda (key before after) (push (list key before after) changes)))
    (nreverse changes)))

(defun appkit-data-map-values (map)
  "Return MAP's borrowed values in unspecified order."
  (let (values)
    (appkit-data-map-each (lambda (_key value) (push value values)) map)
    (nreverse values)))

(provide 'appkit-data-map)
;;; appkit-data-map.el ends here
