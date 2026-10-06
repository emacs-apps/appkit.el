;;; appkit-bindings.el --- Semantic bindings across application adapters -*- lexical-binding: t; -*-

;;; Commentary:
;; Adapters name supported actions, not keys.  One theme supplies native and
;; optional Evil bindings.  Read-only maps own single-letter actions; writable
;; maps never promote those actions into composer text.  Protocol commands and
;; exact Surface ownership remain with the adapter.

;;; Code:

(require 'cl-lib)
(require 'keymap)
(require 'subr-x)

(declare-function appkit-evil-define-keys "appkit-evil" (states keymap-symbol &rest bindings))
(declare-function appkit-evil-normalize-keymaps "appkit-evil" ())
(declare-function evil-get-auxiliary-keymap "evil-core" (map state &optional create ignore-parent))
(defvar appkit-evil-enable-integration)

(defgroup appkit-bindings nil
  "Semantic key themes shared by application adapters."
  :group 'appkit)

(defconst appkit-binding-defaults
  '((activate :read-only ("RET" "<return>") :evil ("RET" "<return>"))
    (app-menu :native ("C-c ?") :read-only ("?") :evil ("g a")
              :states (normal motion visual))
    (navigation :native ("M-g") :evil ("g A"))
    (refresh :native ("C-c g") :read-only ("g") :evil ("g r"))
    (next-item :read-only ("n") :evil ("g j"))
    (previous-item :read-only ("p") :evil ("g k"))
    (next-button :read-only ("TAB" "<tab>") :evil ("TAB" "<tab>"))
    (previous-button :read-only ("<backtab>" "<S-tab>") :evil ("<backtab>" "<S-tab>"))
    (help :native ("C-c C-h") :evil ("g ?"))
    (quit :read-only ("q") :evil ("q" "Z Z"))
    (quit-cancel :evil ("Z Q"))
    (return :native ("RET" "<return>") :evil ("RET" "<return>")
            :states (normal motion insert) :filter appkit-bindings--completion-return)
    (newline :native ("C-j") :evil ("C-j") :states (insert))
    (submit :native ("C-c C-c" "C-c RET" "C-c <return>")
            :evil ("C-c C-c" "C-c RET" "C-c <return>") :states (normal motion insert))
    (input-options :native ("C-c C-o") :evil ("C-c C-o") :states (normal motion insert))
    (preview :native ("M-RET" "M-<return>") :evil ("M-RET" "M-<return>")
             :states (normal motion insert))
    (cancel :native ("C-c C-k") :evil ("C-c C-k") :states (normal motion insert))
    (focus-input :native ("C-c '") :evil ("i"))
    (attach :native ("C-c C-a") :evil ("Z a"))
    (attach-file :native ("C-c C-f") :evil ("Z f"))
    (attach-clipboard :native ("C-c C-v") :evil ("Z v"))
    (detach :native ("C-c C-d") :evil ("Z d"))
    (message-menu :read-only ("o") :evil ("o"))
    (reply :read-only ("r") :evil ("r"))
    (forward :read-only ("f") :evil ("R"))
    (edit :read-only ("e") :evil ("i"))
    (delete :read-only ("d") :evil ("D" "d d"))
    (copy-text :read-only ("c") :evil ("Z y"))
    (copy-link :read-only ("l") :evil ("Z l"))
    (react :read-only ("!") :evil ("!"))
    (history-older :read-only ("<") :evil ("g +"))
    (history-newer :read-only (">") :evil ("g -"))
    (latest :native ("C-c >") :evil ("g b"))
    (compose :read-only ("c") :evil ("c"))
    (search :read-only ("s") :evil ("g s"))
    (mark :read-only ("m") :evil ("m"))
    (unmark :read-only ("u") :evil ("u"))
    (clear-marks :read-only ("U") :evil ("U"))
    (toggle-mark :read-only ("t") :evil ("t"))
    (mark-all :read-only ("M") :evil ("M"))
    (toggle-all-marks :read-only ("~") :evil ("~")))
  "Semantic theme defaults, inspired by evil-collection's ID/command bindings.
Each entry is (ID . PLIST).  :native keys apply to every context; :read-only
replaces them in read-only maps.  :evil keys use :states, defaulting to normal
and motion.  :filter is a menu-item lookup filter.  Missing actions are never
installed: adapters must not supply fake commands for unsupported capabilities.")

(defcustom appkit-binding-overrides nil
  "Per-property overrides of `appkit-binding-defaults'.
An explicit nil removes that property; :enabled nil disables the action.
Use `appkit-bindings-refresh' after changing this value with `setq'.  Customize
refreshes registered maps automatically.  Example:
  ((attach-file :native (\"C-c f\") :evil (\"Z F\")))"
  :type '(alist :key-type symbol :value-type plist)
  :group 'appkit-bindings
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (fboundp 'appkit-bindings-refresh) (appkit-bindings-refresh))))

(cl-defstruct (appkit-bindings--adapter (:constructor appkit-bindings--adapter-create))
  map context actions native-installed evil-installed evil-enabled pending customized)

(defvar appkit-bindings--adapters (make-hash-table :test #'eq)
  "Map symbols and their supported semantic actions.")

(defun appkit-bindings--property (id property &optional default)
  "Resolve ID's PROPERTY, distinguishing explicit nil from absence."
  (let ((override (cdr (assq id appkit-binding-overrides)))
        (base (cdr (assq id appkit-binding-defaults))))
    (cond ((plist-member override property) (plist-get override property))
          ((plist-member base property) (plist-get base property))
          (t default))))

(defun appkit-bindings--keys (value)
  "Normalize key strings in VALUE into event vectors."
  (mapcar (lambda (key) (vconcat (key-parse key)))
          (if (stringp value) (list value) value)))

(defun appkit-bindings--completion-return (fallback)
  "Let the active in-region completion frontend own Return before FALLBACK.
Resolve before pre-command hooks can turn an unaccepted preview into text."
  (let* ((map (and (bound-and-true-p completion-in-region-mode)
                   (or (cdr (assq 'completion-in-region-mode minor-mode-overriding-map-alist))
                       (cdr (assq 'completion-in-region-mode minor-mode-map-alist)))))
         (command (and (keymapp map) (keymap-lookup map "RET"))))
    (if (commandp command) command fallback)))

(defun appkit-bindings--native-readonly (command)
  "Expose COMMAND only outside Evil's modal editing states.
Native timeline letters must not shadow Evil search or visual motions."
  (unless (and (bound-and-true-p evil-local-mode)
               (not (eq (bound-and-true-p evil-state) 'emacs)))
    command))

(defun appkit-bindings--raw-binding (map key)
  "Read KEY's unfiltered local definition in MAP, without inherited entries."
  ;; The core iterator deliberately returns, rather than traverses, parents.
  ;; `lookup-key' evaluates menu-item filters, which is wrong for ownership.
  (let ((index 0) definition)
    (while (and (keymapp map) (< index (length key)))
      (setq definition nil)
      (map-keymap-internal
       (lambda (event value)
         (when (equal event (aref key index)) (setq definition value)))
       map)
      (setq map definition index (1+ index)))
    (when (= index (length key)) definition)))

(defun appkit-bindings--retire (adapter installed)
  "Retire INSTALLED keys, remembering user replacements in ADAPTER."
  (dolist (entry installed)
    (pcase-let ((`(,map ,key ,definition) entry))
      (if (equal (appkit-bindings--raw-binding map key) definition)
          ;; REMOVE restores parent inheritance instead of shadowing it with nil.
          (define-key map key nil t)
        (cl-pushnew (list map key) (appkit-bindings--adapter-customized adapter)
                    :test #'equal)))))

(defun appkit-bindings--plan (adapter modal)
  "Prepare ADAPTER's bindings for native keys, or MODAL keys."
  (let (plan)
    (dolist (action (appkit-bindings--adapter-actions adapter))
      (let* ((id (car action)) (command (cdr action))
             (property (if modal :evil
                         (if (and (eq (appkit-bindings--adapter-context adapter) 'read-only)
                                  (or (plist-member (cdr (assq id appkit-binding-overrides)) :read-only)
                                      (plist-member (cdr (assq id appkit-binding-defaults)) :read-only)))
                             :read-only :native)))
             (keys (appkit-bindings--keys (appkit-bindings--property id property)))
             (states (if modal (ensure-list (appkit-bindings--property id :states '(normal motion)))
                       '(nil)))
             (filter (appkit-bindings--property id :filter))
             (native-readonly (and (not modal)
                                   (eq (appkit-bindings--adapter-context adapter) 'read-only)))
             (effective-filter
              (if native-readonly
                  (if filter
                      (lambda (definition)
                        (when (appkit-bindings--native-readonly definition)
                          (funcall filter definition)))
                    #'appkit-bindings--native-readonly)
                filter))
             (definition (if effective-filter
                             (list 'menu-item "" command :filter effective-filter)
                           command)))
        (when (and command (appkit-bindings--property id :enabled t))
          (dolist (state states)
            (dolist (key keys)
              (let ((old (cl-find-if (lambda (entry) (and (eq state (car entry))
                                                         (equal key (cadr entry)))) plan)))
                (when (and old (not (equal (caddr old) definition)))
                  (error "Appkit semantic bindings conflict in %s: %s"
                         (appkit-bindings--adapter-map adapter) (key-description key)))
                (unless old (push (list state key definition) plan))))))))
    (nreverse plan)))

(defun appkit-bindings--install (adapter)
  "Install available parts of ADAPTER, retaining pending load-order work."
  (let* ((symbol (appkit-bindings--adapter-map adapter))
         (map (and (boundp symbol) (symbol-value symbol)))
         (modal (and (appkit-bindings--adapter-evil-enabled adapter)
                     (featurep 'evil) (fboundp 'appkit-evil-define-keys)
                     (bound-and-true-p appkit-evil-enable-integration))))
    (if (not (and (keymapp map) (appkit-bindings--adapter-context adapter)))
        (setf (appkit-bindings--adapter-pending adapter) t)
      ;; Validate both plans before removing any old bindings.
      (let ((native-plan (appkit-bindings--plan adapter nil))
            (modal-plan (when modal (appkit-bindings--plan adapter t))))
        (appkit-bindings--retire adapter (appkit-bindings--adapter-native-installed adapter))
        (appkit-bindings--retire adapter (appkit-bindings--adapter-evil-installed adapter))
        (setf (appkit-bindings--adapter-native-installed adapter) nil
              (appkit-bindings--adapter-evil-installed adapter) nil)
        (dolist (entry native-plan)
          (pcase-let ((`(,_state ,key ,definition) entry))
            (unless (member (list map key) (appkit-bindings--adapter-customized adapter))
              (define-key map key definition)
              (push (list map key definition) (appkit-bindings--adapter-native-installed adapter)))))
        (dolist (entry modal-plan)
          (pcase-let ((`(,state ,key ,definition) entry))
            (let ((aux (evil-get-auxiliary-keymap map state t t)))
              (unless (member (list aux key) (appkit-bindings--adapter-customized adapter))
                (appkit-evil-define-keys state symbol key definition)
                (push (list aux key definition) (appkit-bindings--adapter-evil-installed adapter))))))
        (setf (appkit-bindings--adapter-pending adapter)
              (and (appkit-bindings--adapter-evil-enabled adapter) (not modal)))))))

(defun appkit-bindings--after-load (&rest _args)
  "Finish only adapters awaiting their map or optional modal backend."
  (let (pending)
    (maphash (lambda (_symbol adapter)
               (when (appkit-bindings--adapter-pending adapter)
                 (appkit-bindings--install adapter)
                 (when (appkit-bindings--adapter-pending adapter) (setq pending t))))
             appkit-bindings--adapters)
    (unless pending (remove-hook 'after-load-functions #'appkit-bindings--after-load))))

(defun appkit-bindings--schedule ()
  "Keep one deferred installer hook while any adapter awaits a dependency."
  (when (cl-loop for adapter being the hash-values of appkit-bindings--adapters
                 thereis (appkit-bindings--adapter-pending adapter))
    (add-hook 'after-load-functions #'appkit-bindings--after-load t)))

(defun appkit-bindings-define (keymap-symbol context &rest actions)
  "Declare supported semantic ACTIONS in KEYMAP-SYMBOL for CONTEXT.
CONTEXT is `input' or `read-only'.  ACTIONS alternate semantic IDs and command
or prefix-map definitions, like evil-collection's ID/command adapter API.
This replaces the map's previous declaration.  Native keys work without Evil;
`appkit-bindings-enable-evil' opts the same declaration into modal projection.
Maps may be defined later.  Unsupported actions must be omitted, not stubbed."
  (unless (and (symbolp keymap-symbol) (memq context '(input read-only))
               (zerop (% (length actions) 2)))
    (error "Invalid Appkit semantic binding declaration: %s" keymap-symbol))
  (let ((adapter (or (gethash keymap-symbol appkit-bindings--adapters)
                     (appkit-bindings--adapter-create :map keymap-symbol)))
        pairs)
    (while actions
      (let ((id (pop actions)) (command (pop actions)))
        (unless (assq id appkit-binding-defaults) (error "Unknown Appkit action: %s" id))
        (when (assq id pairs) (error "Duplicate Appkit action: %s" id))
        (push (cons id command) pairs)))
    (setf (appkit-bindings--adapter-context adapter) context
          (appkit-bindings--adapter-actions adapter) (nreverse pairs))
    (puthash keymap-symbol adapter appkit-bindings--adapters)
    (appkit-bindings--install adapter)
    (appkit-bindings--schedule))
  keymap-symbol)

(defun appkit-bindings-enable-evil (keymap-symbol)
  "Project KEYMAP-SYMBOL's existing semantic declaration into Evil.
Call from the adapter's own enable/initial-state lifecycle, not globally.
Declaration, map, and Evil may arrive in any order."
  (let ((adapter (or (gethash keymap-symbol appkit-bindings--adapters)
                     (appkit-bindings--adapter-create :map keymap-symbol))))
    (setf (appkit-bindings--adapter-evil-enabled adapter) t)
    (puthash keymap-symbol adapter appkit-bindings--adapters)
    (appkit-bindings--install adapter)
    (appkit-bindings--schedule))
  keymap-symbol)

(defun appkit-bindings-refresh (&optional keymap-symbol)
  "Reapply the shared theme to KEYMAP-SYMBOL or every registered adapter.
Retire old owned bindings; preserve unrelated or subsequently customized keys."
  (interactive)
  (if keymap-symbol
      (when-let* ((adapter (gethash keymap-symbol appkit-bindings--adapters)))
        (appkit-bindings--install adapter))
    (maphash (lambda (_symbol adapter) (appkit-bindings--install adapter)) appkit-bindings--adapters))
  (appkit-bindings--schedule)
  (when (fboundp 'appkit-evil-normalize-keymaps)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer (appkit-evil-normalize-keymaps)))))

(provide 'appkit-bindings)
;;; appkit-bindings.el ends here
