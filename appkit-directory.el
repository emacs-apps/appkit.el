;;; appkit-directory.el --- Sectioned directory surfaces  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; A protocol-neutral, non-recursive directory surface.  Applications project
;; their domain hierarchy into a visible flat stream of sections, groups,
;; items, notes, and spacers.  Appkit owns stable reconciliation, fold state,
;; semantic position, exact row activation, and item navigation.  Every item
;; belongs to a section; its group key is optional so sections may own items
;; directly as well as through a full section/group/item path.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'seq)
(require 'subr-x)
(require 'appkit-ewoc)
(require 'appkit-position)
(require 'appkit-ui)

(defconst appkit-directory-entry-roles
  '(section group item note spacer)
  "Closed set of roles accepted by an Appkit directory surface.")

(defconst appkit-directory-key-property 'appkit-directory-key
  "Text property carrying an opaque directory entry key.")

(defconst appkit-directory-role-property 'appkit-directory-role
  "Text property carrying a directory entry role.")

(defconst appkit-directory-section-property 'appkit-directory-section-key
  "Text property carrying an optional owning section key.")

(defconst appkit-directory-group-property 'appkit-directory-group-key
  "Text property carrying an optional owning group key.")

(defconst appkit-directory-item-property 'appkit-directory-item
  "Text property marking rows included in item navigation.")

(defconst appkit-directory-unread-property 'appkit-directory-unread
  "Text property marking unread directory items.")

(cl-defstruct (appkit-directory-entry
               (:constructor appkit-directory-entry-create))
  key
  role
  section-key
  group-key
  label
  trailing
  face
  indent
  foldable-p
  fold-key
  fold-default-expanded-p
  expanded-p
  fold-locked-reason
  primary-action
  item-p
  unread-p
  payload
  stamp
  help-echo
  mouse-face
  properties)

(cl-defstruct (appkit-directory-surface
               (:constructor appkit-directory-surface--create))
  ewoc
  node-table
  fold-state
  entry-inserter
  item-inserter
  activate-function
  fold-function
  action-rows-p
  anchor-property)

(defvar-local appkit-directory--surface nil
  "Directory surface owned by the current buffer.")

(defconst appkit-directory--missing-fold-state
  (make-symbol "appkit-directory-missing-fold-state")
  "Sentinel distinguishing a missing fold override from nil.")

(defun appkit-directory-current-surface ()
  "Return the current buffer's directory surface, or nil when unowned."
  (and (appkit-directory-surface-p appkit-directory--surface)
       appkit-directory--surface))

(defun appkit-directory-surface ()
  "Return the current buffer's initialized directory surface."
  (or (appkit-directory-current-surface)
      (error "Appkit directory surface is not initialized")))

(defun appkit-directory-retire ()
  "Retire the current buffer's directory surface and return its fold state.

This only releases buffer ownership; applications remain responsible for
replacing or clearing presentation state before installing another renderer.
Return nil when the current buffer has no directory surface."
  (when-let* ((surface (appkit-directory-current-surface)))
    (setq-local appkit-directory--surface nil)
    (appkit-directory-surface-fold-state surface)))

(cl-defun appkit-directory-configure
    (surface &key
             (entry-inserter nil entry-inserter-p)
             (item-inserter nil item-inserter-p)
             (activate-function nil activate-function-p)
             (fold-function nil fold-function-p)
             (action-rows-p nil action-rows-p-supplied-p)
             (anchor-property nil anchor-property-p))
  "Configure adapter callbacks and position ownership for SURFACE.

ENTRY-INSERTER receives SURFACE and every non-spacer ENTRY after Appkit has
inserted indentation and an optional fold marker.  A non-nil return value
means that it handled the entry by inserting exactly one newline-terminated
physical row.  A nil return value falls through to ITEM-INSERTER for item
entries, or to Appkit's default label renderer otherwise.  ITEM-INSERTER also
receives SURFACE and one item ENTRY and must insert exactly one terminated
physical row.  ACTIVATE-FUNCTION receives SURFACE and an item whose primary
action is item activation.  FOLD-FUNCTION receives SURFACE, the fold entry,
and its new expanded state.  ACTION-ROWS-P opts into whole-row text buttons;
ordinary directory modes dispatch through their mode map.  ANCHOR-PROPERTY
defaults to `appkit-directory-key-property'."
  (unless (appkit-directory-surface-p surface)
    (error "Appkit directory configuration requires a surface"))
  (dolist (pair `((entry-inserter
                   . ,(and entry-inserter-p entry-inserter))
                  (item-inserter . ,(and item-inserter-p item-inserter))
                  (activate-function
                   . ,(and activate-function-p activate-function))
                  (fold-function . ,(and fold-function-p fold-function))))
    (when (and (cdr pair) (not (functionp (cdr pair))))
      (error "Appkit directory %s is not callable" (car pair))))
  (when entry-inserter-p
    (setf (appkit-directory-surface-entry-inserter surface) entry-inserter))
  (when item-inserter-p
    (setf (appkit-directory-surface-item-inserter surface) item-inserter))
  (when activate-function-p
    (setf (appkit-directory-surface-activate-function surface)
          activate-function))
  (when fold-function-p
    (setf (appkit-directory-surface-fold-function surface) fold-function))
  (when action-rows-p-supplied-p
    (unless (memq action-rows-p '(nil t))
      (error "Appkit directory action-rows-p must be boolean"))
    (setf (appkit-directory-surface-action-rows-p surface) action-rows-p))
  (when anchor-property-p
    (unless (symbolp anchor-property)
      (error "Appkit directory anchor property must be a symbol"))
    (setf (appkit-directory-surface-anchor-property surface)
          anchor-property))
  surface)

(cl-defun appkit-directory-initialize (&key fold-state)
  "Initialize and return a fresh directory surface in the current buffer.

FOLD-STATE may be an existing hash table whose overrides should be shared by
the new surface.  This lets an application rebuild or replace its directory
layout without discarding the user's fold choices."
  (when (and fold-state (not (hash-table-p fold-state)))
    (error "Appkit directory fold-state must be a hash table"))
  (let ((surface
         (appkit-directory-surface--create
          :node-table (make-hash-table :test #'equal)
          :fold-state (or fold-state (make-hash-table :test #'equal))
          :action-rows-p nil
          :anchor-property appkit-directory-key-property)))
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (erase-buffer)
      (setf (appkit-directory-surface-ewoc surface)
            (ewoc-create #'appkit-directory--print-entry nil nil t)))
    (setq-local appkit-directory--surface surface)
    surface))

(defun appkit-directory-fold-expanded-p
    (surface fold-key default-expanded-p &optional force-expanded-p)
  "Return effective expansion state for FOLD-KEY in SURFACE.

DEFAULT-EXPANDED-P supplies the application default.  FORCE-EXPANDED-P makes
the result true without mutating the user's stored fold override."
  (unless fold-key
    (error "Appkit directory fold key must be non-nil"))
  (if force-expanded-p
      t
    (let ((value
           (gethash fold-key
                    (appkit-directory-surface-fold-state surface)
                    appkit-directory--missing-fold-state)))
      (if (eq value appkit-directory--missing-fold-state)
          (and default-expanded-p t)
        (and value t)))))

(defun appkit-directory-set-fold-expanded
    (surface fold-key expanded-p)
  "Store EXPANDED-P as FOLD-KEY's explicit state in SURFACE."
  (unless fold-key
    (error "Appkit directory fold key must be non-nil"))
  (puthash fold-key (and expanded-p t)
           (appkit-directory-surface-fold-state surface))
  (and expanded-p t))

(defun appkit-directory-clear-fold-state (surface &optional fold-key)
  "Clear SURFACE fold override for FOLD-KEY, or all overrides when nil."
  (if fold-key
      (remhash fold-key (appkit-directory-surface-fold-state surface))
    (clrhash (appkit-directory-surface-fold-state surface))))

(defun appkit-directory--validate-entry (entry)
  "Require one complete directory ENTRY."
  (unless (appkit-directory-entry-p entry)
    (error "Appkit directory projection contains a non-entry"))
  (unless (appkit-directory-entry-key entry)
    (error "Appkit directory entry has no stable key"))
  (unless (memq (appkit-directory-entry-role entry)
                appkit-directory-entry-roles)
    (error "Appkit directory entry has invalid role %S"
           (appkit-directory-entry-role entry)))
  (let ((indent (or (appkit-directory-entry-indent entry) 0)))
    (unless (and (integerp indent) (>= indent 0))
      (error "Appkit directory indent must be a non-negative integer")))
  (pcase (appkit-directory-entry-role entry)
    ('section
     (when (or (appkit-directory-entry-section-key entry)
               (appkit-directory-entry-group-key entry))
       (error "Appkit directory section cannot have an owner")))
    ('group
     (unless (appkit-directory-entry-section-key entry)
       (error "Appkit directory group has no owning section"))
     (when (appkit-directory-entry-group-key entry)
       (error "Appkit directory group cannot own itself")))
    ('item
     (unless (appkit-directory-entry-section-key entry)
       (error "Appkit directory item has no owning section")))
    ('spacer
     (when (or (appkit-directory-entry-section-key entry)
               (appkit-directory-entry-group-key entry))
       (error "Appkit directory spacer cannot have an owner"))))
  (when (and (appkit-directory-entry-item-p entry)
             (not (eq (appkit-directory-entry-role entry) 'item)))
    (error "Only Appkit directory item rows can be navigable"))
  (when (and (appkit-directory-entry-unread-p entry)
             (not (appkit-directory-entry-item-p entry)))
    (error "Unread Appkit directory rows must be navigable items"))
  (when (appkit-directory-entry-foldable-p entry)
    (unless (memq (appkit-directory-entry-role entry)
                  '(section group item))
      (error "Appkit directory role cannot be folded"))
    (unless (appkit-directory-entry-fold-key entry)
      (error "Foldable Appkit directory entry has no fold key"))
    (unless (memq (appkit-directory-entry-expanded-p entry) '(nil t))
      (error "Foldable Appkit directory entry has invalid expanded state")))
  (pcase (appkit-directory-entry-primary-action entry)
    ('nil nil)
    ('fold
     (unless (appkit-directory-entry-foldable-p entry)
       (error "Appkit directory fold primary action requires a foldable entry")))
    ('item
     (unless (appkit-directory-entry-item-p entry)
       (error "Appkit directory item primary action requires a navigable item")))
    (_
     (error "Appkit directory entry has invalid primary action %S"
            (appkit-directory-entry-primary-action entry))))
  (when (eq (appkit-directory-entry-role entry) 'spacer)
    (when (or (appkit-directory-entry-foldable-p entry)
              (appkit-directory-entry-item-p entry))
      (error "Appkit directory spacer cannot be foldable or navigable")))
  entry)

(defun appkit-directory--validate-entries (entries)
  "Require unique keys and valid shapes throughout ENTRIES."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (appkit-directory--validate-entry entry)
      (let ((key (appkit-directory-entry-key entry)))
        (when (gethash key seen)
          (error "Appkit directory projection duplicates key %S" key))
        (puthash key t seen))))
  entries)

(defun appkit-directory--entry-properties (entry)
  "Return reserved and application-owned text properties for ENTRY."
  (let ((cursor (appkit-directory-entry-properties entry))
        (owned
         (list appkit-directory-key-property
               appkit-directory-role-property
               appkit-directory-section-property
               appkit-directory-group-property
               appkit-directory-item-property
               appkit-directory-unread-property
               'help-echo
               'mouse-face))
        custom
        custom-rear)
    (while cursor
      (unless (cdr cursor)
        (error "Appkit directory entry properties are not a valid plist"))
      (let ((property (pop cursor))
            (value (pop cursor)))
        (if (eq property 'rear-nonsticky)
            (setq custom-rear value)
          (unless (memq property (append owned '(rear-nonsticky)))
            (setq custom (append custom (list property value)))))))
    (unless (or (null custom-rear) (eq custom-rear t) (listp custom-rear))
      (error "Appkit directory rear-nonsticky must be t or a property-name list"))
    (append
     (list
      appkit-directory-key-property (appkit-directory-entry-key entry)
      appkit-directory-role-property (appkit-directory-entry-role entry)
      appkit-directory-section-property
      (appkit-directory-entry-section-key entry)
      appkit-directory-group-property (appkit-directory-entry-group-key entry)
      appkit-directory-item-property
      (and (appkit-directory-entry-item-p entry) t)
      appkit-directory-unread-property
      (and (appkit-directory-entry-unread-p entry) t)
      'help-echo (appkit-directory-entry-help-echo entry)
      'mouse-face (appkit-directory-entry-mouse-face entry)
      'rear-nonsticky
      (if (eq custom-rear t)
          t
        (delete-dups
         (append owned (and (listp custom-rear) custom-rear)))))
     custom)))

(defun appkit-directory--insert-label-entry (entry)
  "Insert default label presentation for ENTRY."
  (insert (or (appkit-directory-entry-label entry) ""))
  (when-let* ((trailing (appkit-directory-entry-trailing entry)))
    (insert trailing))
  (insert "\n"))

(defun appkit-directory--single-row-p (start end)
  "Return non-nil when START..END is exactly one terminated physical row."
  (and (< start end)
       (eq (char-before end) ?\n)
       (save-excursion
         (goto-char start)
         (equal (search-forward "\n" end t) end))))

(defun appkit-directory--activate-rendered-entry (entry)
  "Activate rendered directory ENTRY in the current buffer."
  (appkit-directory-activate-entry (appkit-directory-surface) entry))

(defun appkit-directory--print-entry (entry)
  "Insert one directory ENTRY using the current surface adapter."
  (appkit-directory--validate-entry entry)
  (let* ((surface (appkit-directory-surface))
         (start (point))
         (indent (or (appkit-directory-entry-indent entry) 0)))
    (pcase (appkit-directory-entry-role entry)
      ('spacer (insert "\n"))
      (_
       (insert (make-string indent ?\s))
       (when (appkit-directory-entry-foldable-p entry)
         (insert (if (appkit-directory-entry-expanded-p entry) "▾ " "▸ ")))
       (unless (and (functionp
                     (appkit-directory-surface-entry-inserter surface))
                    (funcall
                     (appkit-directory-surface-entry-inserter surface)
                     surface entry))
         (if (and (eq (appkit-directory-entry-role entry) 'item)
                  (functionp
                   (appkit-directory-surface-item-inserter surface)))
             (funcall (appkit-directory-surface-item-inserter surface)
                      surface entry)
           (appkit-directory--insert-label-entry entry)))))
    (unless (appkit-directory--single-row-p start (point))
      (error "Appkit directory adapter must render exactly one physical row"))
    (let ((properties (appkit-directory--entry-properties entry)))
      (add-text-properties start (point) properties)
      (when-let* ((face (appkit-directory-entry-face entry)))
        (add-face-text-property start (point) face 'append)))
    (when (and (appkit-directory-surface-action-rows-p surface)
               (or (appkit-directory-entry-foldable-p entry)
                   (appkit-directory-entry-item-p entry)))
      (appkit-ui-make-action-row
       start (point) entry #'appkit-directory--activate-rendered-entry
       :help-echo (appkit-directory-entry-help-echo entry)
       :mouse-face (appkit-directory-entry-mouse-face entry)))))

(cl-defun appkit-directory-reconcile
    (surface entries &key force-keys (preserve-position-p t))
  "Reconcile SURFACE with visible flat ENTRIES.

FORCE-KEYS redraw retained rows with matching stable keys.
PRESERVE-POSITION-P captures and restores semantic point/window state using
SURFACE's anchor property.  Return the updated key-to-EWOC-node table."
  (unless (appkit-directory-surface-p surface)
    (error "Appkit directory reconciliation requires a surface"))
  (appkit-directory--validate-entries entries)
  (let ((snapshot
         (and preserve-position-p
              (appkit-position-capture
               :anchor-property
               (appkit-directory-surface-anchor-property surface)
               :preserve-window-start t))))
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (with-silent-modifications
        (setf (appkit-directory-surface-node-table surface)
              (appkit-ewoc-reconcile
               (appkit-directory-surface-ewoc surface)
               entries #'appkit-directory-entry-key
               :force-keys force-keys))))
    (when snapshot
      (appkit-position-restore snapshot)))
  (appkit-directory-surface-node-table surface))

(defun appkit-directory-key-at-point (&optional position)
  "Return the opaque directory key at POSITION or point."
  (let ((probe (or position (point))))
    (when (and (integer-or-marker-p probe)
               (<= (point-min) probe)
               (<= probe (point-max)))
      (or (and (< probe (point-max))
               (get-text-property probe appkit-directory-key-property))
          (save-excursion
            (goto-char probe)
            (get-text-property (line-beginning-position)
                               appkit-directory-key-property))))))

(defun appkit-directory-entry-at-point (&optional position surface)
  "Return the current directory entry at POSITION from SURFACE."
  (let* ((surface (or surface (appkit-directory-surface)))
         (key (appkit-directory-key-at-point position))
         (entry (and key (appkit-directory-entry-for-key surface key))))
    entry))

(defun appkit-directory-entry-for-key (surface key)
  "Return SURFACE entry identified by opaque KEY, or nil."
  (let ((node (gethash key (appkit-directory-surface-node-table surface))))
    (and node (ewoc-data node))))

(defun appkit-directory--entry-positions (surface predicate)
  "Return SURFACE row starts whose entries satisfy PREDICATE."
  (let ((node (ewoc-nth (appkit-directory-surface-ewoc surface) 0))
        positions)
    (while node
      (when (funcall predicate (ewoc-data node))
        (push (ewoc-location node) positions))
      (setq node
            (ewoc-next (appkit-directory-surface-ewoc surface) node)))
    (nreverse positions)))

(defun appkit-directory--current-entry-start (positions)
  "Return the current entry start in POSITIONS, or point."
  (let ((key (appkit-directory-key-at-point)))
    (or (and key
             (seq-find
              (lambda (position)
                (equal key (appkit-directory-key-at-point position)))
              positions))
        (point))))

(defun appkit-directory-move
    (surface predicate direction &optional wrap-p)
  "Move among SURFACE entries satisfying PREDICATE in DIRECTION.

DIRECTION is 1 for next and -1 for previous.  WRAP-P permits one wrap.  Return
the destination position, or nil when no matching entry exists."
  (unless (memq direction '(-1 1))
    (error "Appkit directory direction must be -1 or 1"))
  (let* ((positions (appkit-directory--entry-positions surface predicate))
         (origin (appkit-directory--current-entry-start positions))
         (target
          (if (> direction 0)
              (or (seq-find (lambda (position) (> position origin)) positions)
                  (and wrap-p (car positions)))
            (or (car (last (seq-filter
                            (lambda (position) (< position origin))
                            positions)))
                (and wrap-p (car (last positions)))))))
    (when target
      (goto-char target))
    target))

(defun appkit-directory-toggle-entry-fold (surface entry)
  "Toggle foldable SURFACE ENTRY independently of its primary action."
  (unless (appkit-directory-entry-foldable-p entry)
    (user-error "Appkit directory entry is not foldable"))
  (when-let* ((reason (appkit-directory-entry-fold-locked-reason entry)))
    (user-error "%s" reason))
  (let* ((fold-key (appkit-directory-entry-fold-key entry))
         (fold-state (appkit-directory-surface-fold-state surface))
         (old-override
          (gethash fold-key fold-state
                   appkit-directory--missing-fold-state))
         (expanded
          (not
           (appkit-directory-fold-expanded-p
            surface fold-key
            (appkit-directory-entry-fold-default-expanded-p entry)))))
    (appkit-directory-set-fold-expanded surface fold-key expanded)
    (condition-case error-data
        (when-let* ((function
                     (appkit-directory-surface-fold-function surface)))
          (funcall function surface entry expanded))
      (error
       (if (eq old-override appkit-directory--missing-fold-state)
           (remhash fold-key fold-state)
         (puthash fold-key old-override fold-state))
       (signal (car error-data) (cdr error-data))))
    expanded))

(defun appkit-directory--entry-primary-action (entry)
  "Return ENTRY's explicit or backwards-compatible primary action."
  (or (appkit-directory-entry-primary-action entry)
      (cond
       ((appkit-directory-entry-foldable-p entry) 'fold)
       ((appkit-directory-entry-item-p entry) 'item))))

(defun appkit-directory-activate-entry (surface entry)
  "Run the primary action for one current SURFACE ENTRY.

Foldable items may declare `item' as their `primary-action', allowing primary
activation to remain independent from `appkit-directory-toggle-entry-fold'."
  (pcase (appkit-directory--entry-primary-action entry)
    ('fold
     (appkit-directory-toggle-entry-fold surface entry))
    ('item
     (if-let* ((function
                (appkit-directory-surface-activate-function surface)))
         (funcall function surface entry)
       (user-error "Appkit directory has no item activation adapter")))
    (_
     (user-error "No directory action at point"))))

(defun appkit-directory-activate ()
  "Activate the row at point, or advance from a passive row."
  (interactive)
  (let ((entry (appkit-directory-entry-at-point)))
    (if (and entry
             (or (appkit-directory-entry-foldable-p entry)
                 (appkit-directory-entry-item-p entry)))
        (appkit-directory-activate-entry
         (appkit-directory-surface) entry)
      (appkit-directory-next-item))))

(defun appkit-directory-tab-dwim ()
  "Toggle a foldable row, otherwise move to the next item."
  (interactive)
  (let ((entry (appkit-directory-entry-at-point)))
    (if (and entry (appkit-directory-entry-foldable-p entry))
        (appkit-directory-toggle-entry-fold
         (appkit-directory-surface) entry)
      (appkit-directory-next-item))))

(defun appkit-directory-next-item ()
  "Move to the next navigable directory item."
  (interactive)
  (unless (appkit-directory-move
           (appkit-directory-surface)
           #'appkit-directory-entry-item-p 1)
    (message "Appkit: no next directory item")))

(defun appkit-directory-previous-item ()
  "Move to the previous navigable directory item."
  (interactive)
  (unless (appkit-directory-move
           (appkit-directory-surface)
           #'appkit-directory-entry-item-p -1)
    (message "Appkit: no previous directory item")))

(defun appkit-directory-next-unread ()
  "Move to the next unread directory item, wrapping once."
  (interactive)
  (unless (appkit-directory-move
           (appkit-directory-surface)
           (lambda (entry)
             (and (appkit-directory-entry-item-p entry)
                  (appkit-directory-entry-unread-p entry)))
           1 t)
    (message "Appkit: no unread directory item")))

(defun appkit-directory-mouse-activate (event)
  "Activate the exact directory row selected by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (appkit-directory-activate))

(defvar appkit-directory-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'appkit-directory-activate)
    (define-key map (kbd "TAB") #'appkit-directory-tab-dwim)
    (define-key map (kbd "<backtab>") #'appkit-directory-previous-item)
    (define-key map (kbd "n") #'appkit-directory-next-item)
    (define-key map (kbd "p") #'appkit-directory-previous-item)
    (define-key map (kbd "u") #'appkit-directory-next-unread)
    (define-key map (kbd "q") #'quit-window)
    (define-key map [mouse-1] #'appkit-directory-mouse-activate)
    map)
  "Shared keymap for sectioned directory surfaces.")

(define-derived-mode appkit-directory-mode special-mode "Appkit-Directory"
  "Base mode for non-recursive sectioned directory surfaces."
  (setq buffer-read-only t
        truncate-lines t)
  (buffer-disable-undo)
  (setq-local buffer-undo-list t)
  (appkit-directory-initialize))

(provide 'appkit-directory)

;;; appkit-directory.el ends here
