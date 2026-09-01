;;; appkit-compose-edit.el --- Owner-bound child text editors  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; A compose surface may need a temporary native-mode child editor for one
;; client-owned value such as a code block.  This component owns only the
;; editor buffer, recursive finish/cancel interaction, and exact Appkit owner
;; lifecycle.  Clients retain document structure, validation policy, wire
;; encoding, persistence, and submission semantics.

;;; Code:

(require 'cl-lib)
(require 'appkit-core)

(defvar-local appkit-compose-edit--result nil
  "Accepted text from the current compose editor, or `canceled'.")

(defvar-local appkit-compose-edit--validation-function nil
  "Optional validation function for the current compose editor.")

(defvar-local appkit-compose-edit--recursion-depth nil
  "Recursion depth immediately before the current compose editor.")

(defvar-keymap appkit-compose-edit-map
  :doc "Keys layered over a compose editor's client-selected major mode."
  "C-c C-c" #'appkit-compose-edit-finish
  "C-x C-s" #'appkit-compose-edit-finish
  "C-c C-z" #'appkit-compose-edit-cancel
  "C-c C-k" #'appkit-compose-edit-cancel)

(defun appkit-compose-edit-finish ()
  "Validate and accept the current compose editor contents."
  (interactive)
  (let ((value (buffer-substring-no-properties (point-min) (point-max))))
    (when (functionp appkit-compose-edit--validation-function)
      (funcall appkit-compose-edit--validation-function value))
    (setq appkit-compose-edit--result value)
    (exit-recursive-edit)))

(defun appkit-compose-edit-cancel ()
  "Cancel the current compose editor without returning its contents."
  (interactive)
  (setq appkit-compose-edit--result 'canceled)
  (exit-recursive-edit))

(defun appkit-compose-edit--cancel-buffer (buffer)
  "Cancel lifecycle-owned compose editor BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq appkit-compose-edit--result 'canceled)
      (if (and (integerp appkit-compose-edit--recursion-depth)
               (> (recursion-depth) appkit-compose-edit--recursion-depth))
          (abort-recursive-edit)
        (kill-buffer buffer)))))

(cl-defun appkit-compose-edit-buffer
    (owner initial-content
           &key mode buffer-name display-action validation-function
           header-line)
  "Edit INITIAL-CONTENT in a temporary buffer owned by OWNER.

MODE is an interactive major-mode function and defaults to `text-mode'.
BUFFER-NAME defaults to a content-free Appkit name.  DISPLAY-ACTION is passed
to `pop-to-buffer'.  VALIDATION-FUNCTION receives the plain edited string and
may signal a user error before the editor exits.  HEADER-LINE overrides the
generic finish/cancel help.

Return the accepted plain string, or nil after explicit cancellation or OWNER
shutdown.  The editor buffer is registered as an Appkit lifecycle handle and
is always killed before this function returns."
  (unless (stringp initial-content)
    (error "Appkit compose editor content must be a string"))
  (let* ((mode-function (or mode #'text-mode))
         (buffer (generate-new-buffer
                  (or buffer-name "*Appkit Compose Edit*")))
         handle
         result)
    (unless (commandp mode-function)
      (kill-buffer buffer)
      (error "Appkit compose editor mode is not interactive: %S"
             mode-function))
    (unwind-protect
        (save-window-excursion
          (pop-to-buffer buffer display-action)
          (insert initial-content)
          (funcall mode-function)
          (setq-local appkit-compose-edit--result nil)
          (setq-local appkit-compose-edit--validation-function
                      validation-function)
          (setq-local appkit-compose-edit--recursion-depth (recursion-depth))
          (use-local-map
           (make-composed-keymap appkit-compose-edit-map (current-local-map)))
          (setq-local
           header-line-format
           (or header-line
               "Finish: C-c C-c / C-x C-s    Cancel: C-c C-k"))
          (setq handle
                (appkit-register-handle
                 owner 'compose-edit-buffer buffer
                 #'appkit-compose-edit--cancel-buffer))
          (condition-case nil
              (recursive-edit)
            (quit (setq appkit-compose-edit--result 'canceled)))
          (when (stringp appkit-compose-edit--result)
            (setq result appkit-compose-edit--result)))
      (when handle (appkit-retire-handle handle))
      (when (buffer-live-p buffer) (kill-buffer buffer)))
    result))

(provide 'appkit-compose-edit)

;;; appkit-compose-edit.el ends here
