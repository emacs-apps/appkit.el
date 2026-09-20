;;; appkit-selection.el --- Stable-key selection values -*- lexical-binding: t; -*-

;;; Commentary:

;; Protocol-neutral values for client Surface models.  Transitions return new
;; selections; projection caches and buffer positions never own these values.
;; Clients decide order, selectability, deletion, rendering and key bindings.

;;; Code:

(require 'cl-lib)
(require 'seq)

(cl-defstruct (appkit-selection
               (:constructor appkit-selection--create)
               (:copier nil))
  keys previous)

(defun appkit-selection-create ()
  "Return an empty selection value."
  (appkit-selection--create))

(defun appkit-selection-member-p (selection key)
  "Return non-nil when SELECTION contains KEY."
  (member key (appkit-selection-keys selection)))

(defun appkit-selection-set (selection key selected)
  "Return SELECTION with KEY's SELECTED state, without mutating either input."
  (let ((keys (appkit-selection-keys selection)))
    (appkit-selection--create
     :keys (if selected
               (if (member key keys) keys (append keys (list key)))
             (remove key keys))
     :previous (appkit-selection-previous selection))))

(defun appkit-selection-toggle (selection)
  "Return cleared SELECTION, or restore the most recently cleared keys."
  (appkit-selection--create
   :keys (unless (appkit-selection-keys selection)
           (appkit-selection-previous selection))
   :previous (appkit-selection-keys selection)))

(defun appkit-selection-forget (selection keys)
  "Remove deleted KEYS from SELECTION and its restorable keys.
A row disappearing from a projection is not evidence of deletion."
  (appkit-selection--create
   :keys (seq-difference (appkit-selection-keys selection) keys #'equal)
   :previous (seq-difference (appkit-selection-previous selection) keys #'equal)))

(provide 'appkit-selection)
;;; appkit-selection.el ends here
