;;; appkit-projection.el --- Stable-key projection mechanics  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Protocol-independent projection mechanics embedded by Generated Renderers.
;; Clients own source state, query semantics, and row rendering.  Appkit owns
;; stable-key reconciliation, presentation dependency indexing, frame updates,
;; and semantic position preservation.  Projection state is presentation
;; cache: discarding and rebuilding it cannot change a domain decision.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-ewoc)
(require 'appkit-position)
(require 'appkit-transaction)
(require 'appkit-surface)
(require 'appkit-resource)

(cl-defstruct (appkit-projection-row
               (:constructor appkit-projection-row-create))
  "One protocol-independent projected row."
  key
  payload
  context
  dependencies
  resource-demands)

(cl-defstruct (appkit-projection--engine
               (:constructor appkit-projection--engine-create))
  "Stable-key presentation cache embedded by one Generated Renderer."
  ewoc
  node-table
  row-table
  keys
  dependency-index
  anchor-property
  printer
  header
  footer
  no-separator-p)

(cl-defstruct (appkit-projection-change
               (:constructor appkit-projection-change-create)
               (:copier nil))
  "One mergeable request for a standard Projection Renderer."
  full-p
  keys
  resources
  frame-p
  geometry-p
  rekeys
  position)

(defun appkit-projection--print-row (projection row)
  "Render projected ROW through PROJECTION's client printer."
  (let ((printer (appkit-projection--engine-printer projection)))
    (unless (functionp printer)
      (error "Appkit projection has no row printer"))
    (funcall printer row)))

(cl-defun appkit-projection-create
    (printer anchor-property &key header footer no-separator-p)
  "Create an empty Renderer projection at point.

PRINTER renders one `appkit-projection-row'.  ANCHOR-PROPERTY carries stable
row identity in generated text.  HEADER and FOOTER seed the EWOC frame.
NO-SEPARATOR-P is passed to `ewoc-create'.  The caller must be executing inside
its Generated Renderer mutation boundary."
  (unless (functionp printer)
    (error "Appkit projection requires a row printer"))
  (let* ((header (or header ""))
         (footer (or footer ""))
         (projection
          (appkit-projection--engine-create
           :node-table (make-hash-table :test #'equal)
           :row-table (make-hash-table :test #'equal)
           :keys nil
           :dependency-index (make-hash-table :test #'equal)
           :anchor-property anchor-property
           :printer printer
           :header header
           :footer footer
           :no-separator-p (and no-separator-p t))))
    (setf (appkit-projection--engine-ewoc projection)
          (ewoc-create
           (apply-partially #'appkit-projection--print-row projection)
           header footer no-separator-p))
    projection))

(cl-defun appkit-projection-project
    (entries key-function &key context-function dependencies-function
             (row-function #'appkit-projection-row-create))
  "Project ENTRIES into protocol-independent rows using KEY-FUNCTION.

CONTEXT-FUNCTION receives the previous and current entry.  DEPENDENCIES-FUNCTION
receives the current entry and returns opaque presentation dependency keys.
ROW-FUNCTION constructs each row from `:key', `:payload', `:context', and
`:dependencies' keyword arguments."
  (unless (functionp row-function)
    (error "Appkit projection row constructor is not callable"))
  (let (previous rows)
    (dolist (entry entries (nreverse rows))
      (push
       (funcall
        row-function
        :key (funcall key-function entry)
        :payload entry
        :context (and context-function
                      (funcall context-function previous entry))
        :dependencies
        (and dependencies-function
             (delete-dups
              (delq nil
                    (copy-sequence
                     (or (funcall dependencies-function entry) '()))))))
       rows)
      (setq previous entry))))

(defun appkit-projection--validate-rows (rows)
  "Require unique, stable keys on projected ROWS."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (row rows)
      (unless (appkit-projection-row-p row)
        (error "Appkit projection contains an invalid row: %S" row))
      (let ((key (appkit-projection-row-key row)))
        (unless key
          (error "Appkit projection row has no stable key"))
        (when (gethash key seen)
          (error "Appkit projection rows duplicate key %S" key))
        (puthash key t seen)))))

(defun appkit-projection--row-table (rows)
  "Return an equal-tested key table for ROWS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (row rows table)
      (puthash (appkit-projection-row-key row) row table))))

(defun appkit-projection--dependency-index (rows)
  "Return a presentation dependency index for ROWS."
  (let ((index (make-hash-table :test #'equal)))
    (dolist (row rows index)
      (let ((row-key (appkit-projection-row-key row)))
        (dolist (dependency (appkit-projection-row-dependencies row))
          (puthash dependency
                   (cons row-key
                         (delete row-key (gethash dependency index)))
                   index))))))

(defun appkit-projection--dependent-keys-in-index (index dependencies)
  "Return row keys in INDEX affected by DEPENDENCIES."
  (let (keys)
    (dolist (dependency dependencies)
      (setq keys
            (nconc (copy-sequence (gethash dependency index)) keys)))
    (delete-dups (delq nil keys))))

(defun appkit-projection--dependent-keys (projection dependencies)
  "Return PROJECTION row keys affected by DEPENDENCIES."
  (appkit-projection--dependent-keys-in-index
   (appkit-projection--engine-dependency-index projection)
   dependencies))

(defun appkit-projection--keys (projection)
  "Return the ordered row keys in PROJECTION."
  (copy-sequence (appkit-projection--engine-keys projection)))

(defun appkit-projection--row (projection key)
  "Return the row identified by KEY in PROJECTION."
  (gethash key (appkit-projection--engine-row-table projection)))

(defun appkit-projection--node (projection key)
  "Return the EWOC node identified by KEY in PROJECTION."
  (gethash key (appkit-projection--engine-node-table projection)))

(defun appkit-projection--invalidate (projection keys)
  "Redraw existing PROJECTION rows identified by KEYS."
  (dolist (key (delete-dups (delq nil (copy-sequence keys))))
    (appkit-ewoc-invalidate-key
     (appkit-projection--engine-ewoc projection)
     (appkit-projection--engine-node-table projection)
     key)))

(defun appkit-projection-dependent-keys (projection dependencies)
  "Return PROJECTION row keys affected by presentation DEPENDENCIES."
  (appkit-projection--dependent-keys projection dependencies))

(defun appkit-projection-keys (projection)
  "Return the ordered projected row keys in PROJECTION."
  (appkit-projection--keys projection))

(defun appkit-projection-row (projection key)
  "Return PROJECTION's row identified by KEY."
  (appkit-projection--row projection key))

(defun appkit-projection-node (projection key)
  "Return PROJECTION's EWOC node identified by KEY."
  (appkit-projection--node projection key))

(defun appkit-projection--validate-rekeys (projection row-table rekeys)
  "Validate REKEYS against PROJECTION and projected ROW-TABLE."
  (let ((nodes (appkit-projection--engine-node-table projection))
        (targets (make-hash-table :test #'equal)))
    (dolist (mapping rekeys)
      (let ((old-key (car mapping))
            (new-key (cdr mapping)))
        (unless (and old-key new-key (not (equal old-key new-key)))
          (error "Appkit projection has invalid rekey %S" mapping))
        (unless (gethash new-key row-table)
          (error "Appkit projection rekey target %S is not projected" new-key))
        (when (gethash new-key targets)
          (error "Appkit projection has duplicate rekey target %S" new-key))
        (puthash new-key t targets)
        (when (and (gethash old-key nodes)
                   (gethash new-key nodes)
                   (not (eq (gethash old-key nodes)
                            (gethash new-key nodes))))
          (error "Appkit projection rekey target %S already exists" new-key))))))

(defun appkit-projection--apply-rekeys (projection row-table rekeys)
  "Apply validated REKEYS to PROJECTION using projected ROW-TABLE."
  (let ((nodes (appkit-projection--engine-node-table projection)))
    (dolist (mapping rekeys)
      (when-let* ((node (gethash (car mapping) nodes)))
        (ewoc-set-data node (gethash (cdr mapping) row-table))))))

(cl-defun appkit-projection--reconcile
    (projection rows &key force-keys changed-dependencies rekeys)
  "Reconcile PROJECTION with ROWS inside the caller's mutation transaction.

FORCE-KEYS redraws retained rows.  CHANGED-DEPENDENCIES redraws rows that named
those opaque keys before or after reconciliation.  REKEYS promotes old stable
keys to projected replacement keys while preserving EWOC node identity."
  (appkit-projection--validate-rows rows)
  (let* ((row-table (appkit-projection--row-table rows))
         (new-index (appkit-projection--dependency-index rows))
         (dependency-keys
          (delete-dups
           (append
            (appkit-projection--dependent-keys-in-index
             (appkit-projection--engine-dependency-index projection)
             changed-dependencies)
            (appkit-projection--dependent-keys-in-index
             new-index changed-dependencies))))
         (effective-force-keys
          (delete-dups
           (delq nil
                 (append (copy-sequence force-keys)
                         dependency-keys
                         (mapcar #'cdr rekeys))))))
    (appkit-projection--validate-rekeys projection row-table rekeys)
    (appkit-projection--apply-rekeys projection row-table rekeys)
    (setf (appkit-projection--engine-node-table projection)
          (appkit-ewoc-reconcile
           (appkit-projection--engine-ewoc projection)
           rows #'appkit-projection-row-key
           :force-keys effective-force-keys)
          (appkit-projection--engine-row-table projection) row-table
          (appkit-projection--engine-keys projection)
          (mapcar #'appkit-projection-row-key rows)
          (appkit-projection--engine-dependency-index projection) new-index)
    (appkit-projection--keys projection)))

(defun appkit-projection--capture-position (projection position)
  "Capture a position in PROJECTION when POSITION requests preservation."
  (when (or (null position) (eq position 'preserve))
    (appkit-position-capture
     :anchor-property (appkit-projection--engine-anchor-property projection)
     :preserve-window-start t)))

(defun appkit-projection--first-row-position (projection)
  "Return the first projected row position in PROJECTION, or `point-min'."
  (let ((property (appkit-projection--engine-anchor-property projection)))
    (or (and property
             (text-property-not-all
              (point-min) (point-max) property nil))
        (point-min))))

(defun appkit-projection--restore-position (projection position snapshot)
  "Restore POSITION in PROJECTION, using SNAPSHOT for preservation."
  (pcase position
    ('first
     (goto-char (appkit-projection--first-row-position projection)))
    ((pred appkit-position-snapshot-p)
     (appkit-position-restore position))
    ((or 'preserve 'nil)
     (when snapshot
       (appkit-position-restore snapshot)))
    (key
     (when-let* ((property
                  (appkit-projection--engine-anchor-property projection))
                 (target
                  (appkit-position-find-property-value
                   (point-min) (point-max) property key)))
       (goto-char target)))))

(defun appkit-projection--set-frame (projection header footer)
  "Update PROJECTION's frame to HEADER and FOOTER when changed."
  (let ((new-header (or header ""))
        (new-footer (or footer "")))
    (unless (and (equal new-header
                        (appkit-projection--engine-header projection))
                 (equal new-footer
                        (appkit-projection--engine-footer projection)))
      (setf (appkit-projection--engine-header projection) new-header
            (appkit-projection--engine-footer projection) new-footer)
      (ewoc-set-hf
       (appkit-projection--engine-ewoc projection)
       new-header new-footer))))

(cl-defun appkit-projection-sync
    (surface projection rows
             &key header footer force-keys changed-dependencies rekeys
             (position 'preserve) (reconcile-p t))
  "Synchronize Renderer PROJECTION in SURFACE with projected ROWS.

HEADER and FOOTER update the generated frame.  FORCE-KEYS redraws retained
rows.  CHANGED-DEPENDENCIES redraws rows that named any changed presentation
dependency before or after this synchronization.  REKEYS promotes old stable
keys to replacements.  POSITION is `preserve', `first', a stable row key, or
an `appkit-position-snapshot'.  When RECONCILE-P is nil, update only the frame
and position without inspecting ROWS."
  (unless (appkit-projection--engine-p projection)
    (signal 'wrong-type-argument
            (list 'appkit-projection--engine-p projection)))
  (unless (and (appkit-surface--owns-host-p surface)
               (eq (current-buffer) (appkit-surface-buffer surface))
               (eq surface (appkit-current-surface)))
    (error "Projection synchronization requires its exact Surface host"))
  (let ((snapshot
         (appkit-projection--capture-position projection position)))
    (appkit-with-content-update surface
      (appkit-projection--set-frame projection header footer)
      (when reconcile-p
        (appkit-projection--reconcile
         projection rows
         :force-keys force-keys
         :changed-dependencies changed-dependencies
         :rekeys rekeys))
      (appkit-projection--restore-position projection position snapshot))
    (appkit-projection--keys projection)))

(defun appkit-projection--merge-position (left right)
  "Merge projection position intents LEFT and RIGHT."
  (if (or (null right) (eq right 'preserve)) left right))

(defun appkit-projection-change-merge (left right)
  "Merge Projection Renderer changes LEFT and RIGHT in FIFO order."
  (unless (appkit-projection-change-p left)
    (signal 'wrong-type-argument (list 'appkit-projection-change-p left)))
  (unless (appkit-projection-change-p right)
    (signal 'wrong-type-argument (list 'appkit-projection-change-p right)))
  (let ((full-p (or (appkit-projection-change-full-p left)
                    (appkit-projection-change-full-p right))))
    (appkit-projection-change-create
     :full-p full-p
     :keys
     (unless full-p
       (delete-dups
        (append (copy-sequence (appkit-projection-change-keys left))
                (copy-sequence (appkit-projection-change-keys right)))))
     :resources
     (delete-dups
      (append (copy-sequence (appkit-projection-change-resources left))
              (copy-sequence (appkit-projection-change-resources right))))
     :frame-p (or (appkit-projection-change-frame-p left)
                  (appkit-projection-change-frame-p right))
     :geometry-p (or (appkit-projection-change-geometry-p left)
                     (appkit-projection-change-geometry-p right))
     :rekeys
     (append (copy-tree (appkit-projection-change-rekeys left))
             (copy-tree (appkit-projection-change-rekeys right)))
     :position
     (appkit-projection--merge-position
      (appkit-projection-change-position left)
      (appkit-projection-change-position right)))))

(defun appkit-projection--frame-value
    (project-frame surface app-read-view model)
  "Return validated frame pair from PROJECT-FRAME."
  (if (null project-frame)
      (cons "" "")
    (let ((frame (funcall project-frame surface app-read-view model)))
      (unless (and (consp frame)
                   (stringp (car frame))
                   (stringp (cdr frame)))
        (error "Projection frame projector returned invalid frame: %S" frame))
      frame)))

(defun appkit-projection--render-plan
    (projection change geometry-mode)
  "Return `(RECONCILE-P FORCE-KEYS DEPENDENCIES POSITION)' for CHANGE."
  (let ((geometry-p (appkit-projection-change-geometry-p change)))
    (list
     (or (appkit-projection-change-full-p change)
         (appkit-projection-change-keys change)
         (appkit-projection-change-rekeys change)
         (and geometry-p (eq geometry-mode 'reproject)))
     (delete-dups
      (append
       (copy-sequence (appkit-projection-change-keys change))
       (and geometry-p (appkit-projection-keys projection))))
     (copy-sequence (appkit-projection-change-resources change))
     (or (appkit-projection-change-position change) 'preserve))))

(defun appkit-projection--resource-result (rows)
  "Return a replacing Resource companion result discovered from ROWS."
  (let ((demands (make-hash-table :test #'equal))
        (interests (make-hash-table :test #'equal))
        demand-order)
    (dolist (row rows)
      (dolist (demand (appkit-projection-row-resource-demands row))
        (unless (appkit-resource-demand-p demand)
          (error "Projection row contains invalid resource demand: %S" demand))
        (let* ((key (appkit-resource-demand-key demand))
               (existing (gethash key demands)))
          (when (and existing (not (equal existing demand)))
            (error "Projection rows disagree on resource demand %S" key))
          (unless existing
            (puthash key demand demands)
            (push key demand-order))
          (puthash key
                   (cons (appkit-projection-row-key row)
                         (delete (appkit-projection-row-key row)
                                 (gethash key interests)))
                   interests))))
    (appkit-render-result-create
     :resource-demands
     (mapcar (lambda (key) (gethash key demands))
             (nreverse demand-order))
     :resource-interest-update
     (appkit-resource-interest-update-create
      :mode 'replace
      :entries
      (let (entries)
        (maphash
         (lambda (key row-keys)
           (push
            (appkit-resource-interest-create
             :key key :row-keys (nreverse row-keys))
            entries))
         interests)
        (nreverse entries))))))

(cl-defun appkit-projection-renderer-create
    (&key project-all project-frame printer anchor-property
          (geometry-mode 'redraw) no-separator-p)
  "Create a standard Generated Renderer backed by a keyed projection.

PROJECT-ALL receives Surface, pass-scoped App read view, and committed Surface
model, and returns every projected row.  PROJECT-FRAME receives the same
arguments and returns `(HEADER . FOOTER)'.  PRINTER receives Surface,
pass-scoped App read view, and one row.  GEOMETRY-MODE is `redraw' or
`reproject'.  Projection state remains Renderer-owned presentation cache."
  (dolist (callback (list project-all printer))
    (unless (functionp callback)
      (signal 'wrong-type-argument (list 'functionp callback))))
  (unless (or (null project-frame) (functionp project-frame))
    (signal 'wrong-type-argument (list 'functionp project-frame)))
  (unless (memq geometry-mode '(redraw reproject))
    (error "Unsupported projection geometry mode: %S" geometry-mode))
  (let (projection render-surface render-app-read-view)
    (cl-labels
        ((create-cache
           ()
           (setq projection
                 (appkit-projection-create
                  (lambda (row)
                    (unless (and (appkit-surface--owns-host-p render-surface)
                                 render-app-read-view)
                      (error "Projection printer escaped its render pass"))
                    (funcall printer render-surface render-app-read-view row))
                  anchor-property
                  :no-separator-p no-separator-p)))
         (render-change
           (surface app-read-view model change force-full-p)
           (unless (appkit-projection-change-p change)
             (error "Projection Renderer received invalid request: %S" change))
           (setq render-surface surface
                 render-app-read-view app-read-view)
           (unwind-protect
               (let* ((effective
                       (if force-full-p
                           (appkit-projection-change-create
                            :full-p t
                            :resources
                            (appkit-projection-change-resources change)
                            :frame-p t
                            :position
                            (appkit-projection-change-position change))
                         change))
                      (plan
                       (appkit-projection--render-plan
                        projection effective geometry-mode))
                      (reconcile-p (nth 0 plan))
                      (force-keys (nth 1 plan))
                      (changed-dependencies (nth 2 plan))
                      (position (nth 3 plan))
                      (frame
                       (if (or force-full-p
                               (appkit-projection-change-frame-p effective))
                           (appkit-projection--frame-value
                            project-frame surface app-read-view model)
                         (cons (appkit-projection--engine-header projection)
                               (appkit-projection--engine-footer projection))))
                      (rows
                       (and reconcile-p
                            (funcall project-all surface app-read-view model))))
                 (appkit-projection-sync
                  surface projection rows
                  :header (car frame)
                  :footer (cdr frame)
                  :force-keys (and reconcile-p force-keys)
                  :changed-dependencies changed-dependencies
                  :rekeys
                  (and reconcile-p
                       (appkit-projection-change-rekeys effective))
                  :position position
                  :reconcile-p reconcile-p)
                 (unless reconcile-p
                   (appkit-with-content-update surface
                     (appkit-projection--invalidate
                      projection
                      (delete-dups
                       (append
                        force-keys
                        (appkit-projection-dependent-keys
                         projection changed-dependencies))))))
                 (if reconcile-p
                     (appkit-projection--resource-result rows)
                   nil))
             (setq render-surface nil
                   render-app-read-view nil))))
      (appkit-generated-renderer-create
       :mount
       (lambda (_surface _app-read-view _model)
         (let ((inhibit-read-only t))
           (erase-buffer)
           (create-cache)))
       :merge #'appkit-projection-change-merge
       :resource-request
       (lambda (keys)
         (appkit-projection-change-create :resources keys))
       :render
       (lambda (surface app-read-view model change)
         (render-change surface app-read-view model change nil))
       :recover
       (lambda (surface app-read-view model _condition)
         (let ((inhibit-read-only t))
           (erase-buffer)
           (create-cache))
         (render-change
          surface app-read-view model
          (appkit-projection-change-create :full-p t :frame-p t)
          t))
       :unmount
       (lambda (_surface)
         (setq projection nil
               render-surface nil
               render-app-read-view nil))))))

(provide 'appkit-projection)

;;; appkit-projection.el ends here
