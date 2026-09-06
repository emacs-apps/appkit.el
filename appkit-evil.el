;;; appkit-evil.el --- Optional Evil bindings for Appkit  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Appkit's ordinary mode maps remain the Emacs-state contract.  This optional
;; integration defines a separate Evil-state vocabulary instead of raising an
;; entire application map above Evil: doing the latter turns single application
;; keys such as `g' into terminal bindings and breaks native sequences such as
;; `gg'.  Clients can use `appkit-evil-define-keys' for their own state maps.

;;; Code:

(require 'cl-lib)
(require 'appkit-directory)
(require 'appkit-chatbuf)

(declare-function evil-define-key* "evil-core"
                  (state keymap key def &rest bindings))
(declare-function evil-normalize-keymaps "evil-core" (&optional state))
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function evil-insert-state "evil-states" ())
(declare-function evil-quit "evil-commands" (&optional force))
(defvar evil-local-mode)

(defgroup appkit-evil nil
  "Optional Evil integration for Appkit surfaces."
  :group 'appkit
  :prefix "appkit-evil-")

(defcustom appkit-evil-enable-integration t
  "If non-nil, install Appkit's Evil bindings automatically."
  :type 'boolean
  :group 'appkit-evil)

(defcustom appkit-evil-directory-initial-state 'normal
  "Initial Evil state for `appkit-directory-mode'.
When nil, leave Evil's initial-state selection untouched."
  :type '(choice (const :tag "Don't override" nil)
          (const :tag "Normal" normal)
          (const :tag "Motion" motion)
          (const :tag "Emacs" emacs)
          (symbol :tag "Custom state"))
  :group 'appkit-evil)

(defvar appkit-evil--deferred-bindings nil
  "Bindings waiting for their keymap variables to be defined.")

(defconst appkit-evil--readonly-states '(normal motion)
  "Evil states used by read-only Appkit application bindings.")

(defun appkit-evil--keymap-value (symbol)
  "Return SYMBOL's keymap value, or nil when it is not available."
  (and (boundp symbol)
       (keymapp (symbol-value symbol))
       (symbol-value symbol)))

(defun appkit-evil--install-binding (entry)
  "Install deferred binding ENTRY and return non-nil when successful."
  (pcase-let ((`(,states ,keymap-symbol ,bindings) entry))
    (when-let* (((featurep 'evil))
                ((fboundp 'evil-define-key*))
                (keymap (appkit-evil--keymap-value keymap-symbol)))
      (apply #'evil-define-key* states keymap bindings)
      t)))

(defun appkit-evil--after-load (&rest _args)
  "Install bindings whose keymaps became available after a library load."
  (when (and (featurep 'evil)
             appkit-evil-enable-integration
             appkit-evil-directory-initial-state)
    (evil-set-initial-state
     'appkit-directory-mode appkit-evil-directory-initial-state))
  (setq appkit-evil--deferred-bindings
        (cl-delete-if #'appkit-evil--install-binding
                      appkit-evil--deferred-bindings))
  (unless appkit-evil--deferred-bindings
    (remove-hook 'after-load-functions #'appkit-evil--after-load)))

(defun appkit-evil--normalize-bindings (bindings)
  "Return BINDINGS with string key descriptions parsed by `kbd'."
  (let (normalized)
    (while bindings
      (let ((key (pop bindings)))
        (unless bindings
          (error "Appkit Evil bindings contain a key without a definition"))
        (push (if (stringp key) (kbd key) key) normalized)
        (push (pop bindings) normalized)))
    (nreverse normalized)))

(defun appkit-evil-define-keys (states keymap-symbol &rest bindings)
  "Define Evil BINDINGS for STATES in KEYMAP-SYMBOL.

STATES is an Evil state or a list of states.  KEYMAP-SYMBOL names a keymap
variable and may be defined later.  A binding key may be an event sequence or
a string accepted by `kbd'.  Installation waits until both Evil and the keymap
are available."
  (declare (indent 2))
  (unless (symbolp keymap-symbol)
    (error "Appkit Evil keymap name is not a symbol: %S" keymap-symbol))
  (let ((entry
         (list states keymap-symbol
               (appkit-evil--normalize-bindings bindings))))
    (if (appkit-evil--install-binding entry)
        (setq appkit-evil--deferred-bindings
              (delete entry appkit-evil--deferred-bindings))
      (cl-pushnew entry appkit-evil--deferred-bindings :test #'equal)
      (add-hook 'after-load-functions #'appkit-evil--after-load t)))
  keymap-symbol)

(eval-and-compile
  (defconst appkit-evil--state-shorthands
    '((?n . normal)
      (?v . visual)
      (?i . insert)
      (?e . emacs)
      (?o . operator)
      (?m . motion)
      (?r . replace))
    "Doom-style state letters accepted by `appkit-evil-map'.")

  (defun appkit-evil--states-from-keyword (keyword)
    "Return Evil states encoded by shorthand KEYWORD."
    (mapcar
     (lambda (letter)
       (or (alist-get letter appkit-evil--state-shorthands)
           (error "Invalid Appkit Evil state shorthand: %c" letter)))
     (string-to-list (substring (symbol-name keyword) 1)))))

(defmacro appkit-evil-map (&rest clauses)
  "Define grouped Evil bindings using Doom-style state shorthands.

Each clause is a list containing `:map' KEYMAP-SYMBOL, a state keyword such as
`:n', `:m', or `:nm', and string key/definition pairs.  Changing the state
keyword starts another binding group for the same map.  Keymaps and Evil may be
loaded later because every group delegates to `appkit-evil-define-keys'.

For example:

  (appkit-evil-map
    (:map example-mode-map
     :nm \"RET\" (function example-open)
         \"g r\" (function example-refresh)
     :n  \"D\"   (function example-delete)))"
  (declare (indent 0) (debug t))
  (let (forms)
    (dolist (clause clauses)
      (unless (listp clause)
        (error "Appkit Evil map clause is not a list: %S" clause))
      (let ((cursor clause)
            keymap
            states
            bindings)
        (cl-labels
            ((flush
               ()
               (when bindings
                 (unless (and keymap states)
                   (error "Appkit Evil bindings require :map and a state keyword"))
                 (push
                  `(appkit-evil-define-keys
                       ',states ',keymap ,@(nreverse bindings))
                  forms)
                 (setq bindings nil))))
          (while cursor
            (let ((item (pop cursor)))
              (cond
               ((eq item :map)
                (flush)
                (unless cursor
                  (error "Appkit Evil :map has no keymap"))
                (setq keymap (pop cursor)))
               ((keywordp item)
                (flush)
                (setq states (appkit-evil--states-from-keyword item)))
               (t
                (unless cursor
                  (error "Appkit Evil key has no definition: %S" item))
                (push item bindings)
                (push (pop cursor) bindings)))))
          (flush))))
    `(progn ,@(nreverse forms))))

(defun appkit-evil-normalize-keymaps ()
  "Refresh Evil's active keymaps in the current buffer when Evil is active."
  (when (and (featurep 'evil)
             (boundp 'evil-local-mode)
             evil-local-mode)
    (evil-normalize-keymaps)))

(defun appkit-evil-set-initial-states (modes state)
  "Register Evil initial STATE for every major mode in MODES.
Do nothing until Evil is loaded or when STATE is nil."
  (when (and state (featurep 'evil) (fboundp 'evil-set-initial-state))
    (dolist (mode modes)
      (evil-set-initial-state mode state))))

(defun appkit-evil-normalize-buffers (modes)
  "Refresh Evil keymap projections in live buffers using major MODES."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (memq major-mode modes)
          (appkit-evil-normalize-keymaps))))))

(defun appkit-evil-chatbuf-enter-input ()
  "Focus the current Appkit chat composer and enter Evil insert state."
  (interactive)
  (unless (appkit-chatbuf-focus-input)
    (user-error "Appkit chat buffer has no writable composer"))
  (evil-insert-state))

(defun appkit-evil-define-readonly-keys (keymap-symbol)
  "Install standard read-only modal bindings in KEYMAP-SYMBOL.

`q' and `ZZ' close the window, and `ZQ' uses `evil-quit'.  Editing commands
are deliberately left to Evil and the buffer's own read-only enforcement.
Call this before adding surface-specific bindings."
  (appkit-evil-define-keys appkit-evil--readonly-states keymap-symbol
    "q" #'quit-window
    "ZZ" #'quit-window
    "ZQ" #'evil-quit))

(defun appkit-evil--define-directory-keys ()
  "Install modal bindings for `appkit-directory-mode-map'."
  (appkit-evil-define-readonly-keys 'appkit-directory-mode-map)
  (appkit-evil-map
    (:map appkit-directory-mode-map
     :nm
     "RET" #'appkit-directory-activate
     "<return>" #'appkit-directory-activate
     "TAB" #'appkit-directory-tab-dwim
     "<backtab>" #'appkit-directory-previous-item)))

;;;###autoload
(defun appkit-evil-setup ()
  "Install Appkit's native Evil integration.
Safe to call multiple times."
  (interactive)
  (when appkit-evil-enable-integration
    (when (and (featurep 'evil) appkit-evil-directory-initial-state)
      (evil-set-initial-state
       'appkit-directory-mode appkit-evil-directory-initial-state))
    (appkit-evil--define-directory-keys)))

(appkit-evil-setup)

(provide 'appkit-evil)

;;; appkit-evil.el ends here
