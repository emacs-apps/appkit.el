;;; appkit-chatbuf.el --- Shared chat buffer core helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Shared canonical composer state and prompt/input helpers for telega-style
;; chat buffers.  Persistent projected timelines live in
;; `appkit-chat-timeline'; protocol-specific message rendering remains in each
;; client.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'ring)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-surface)
(require 'appkit-transaction)
(require 'appkit-ui)

(declare-function appkit-evil-normalize-keymaps "appkit-evil" ())
(declare-function visual-fill-column-mode "visual-fill-column" (&optional arg))

(defcustom appkit-chatbuf-input-ring-size 50
  "Default size for shared chat buffer input history rings."
  :type 'integer
  :group 'appkit)

(defcustom appkit-chatbuf-wrap-long-lines t
  "Whether Appkit chat buffers visually wrap long timeline lines.

Derived clients can call `appkit-chatbuf-set-soft-wrap' to override this
default for one buffer."
  :type 'boolean
  :group 'appkit)

(defcustom appkit-chatbuf-use-visual-fill-column nil
  "Whether wrapped Appkit chat buffers use `visual-fill-column-mode'.

This integration is optional.  When enabled, Appkit loads the external
`visual-fill-column' package when available; clients can customize its standard
`visual-fill-column-width' and related options in their mode hooks."
  :type 'boolean
  :group 'appkit)

(defface appkit-chatbuf-input-object
  '((t :inherit shadow))
  "Face used for structured input objects inserted into chatbuf input."
  :group 'appkit)

(defface appkit-chatbuf-aux-title
  '((t :inherit bold))
  "Face used for the title of an active composer context."
  :group 'appkit)

(defface appkit-chatbuf-aux-preview
  '((t :inherit shadow))
  "Face used for the one-line preview of an active composer context."
  :group 'appkit)

(defface appkit-chatbuf-aux-accent
  '((t :inherit font-lock-keyword-face))
  "Face used for the vertical accent in composer context cards."
  :group 'appkit)

(defface appkit-chatbuf-aux-close
  '((t :inherit link))
  "Face used for the composer context cancel action."
  :group 'appkit)

(defconst appkit-chatbuf-input-object-property 'appkit-chatbuf-input-object
  "Text property storing the semantic object represented in chatbuf input.")

(defconst appkit-chatbuf-input-object-span-property
  'appkit-chatbuf-input-object-span
  "Text property identifying one structured input object occurrence.

Unlike `appkit-chatbuf-input-object-property', this value is unique for every
insertion.  Adjacent occurrences may carry the same semantic payload, so
payload equality must not define their deletion or serialization boundary.")

(defconst appkit-chatbuf-input-object-start-property 'appkit-chatbuf-input-object-start
  "Text property marking the first character of a structured input object.")

(defconst appkit-chatbuf-input-object-end-property 'appkit-chatbuf-input-object-end
  "Text property marking the last character of a structured input object.")

(defconst appkit-chatbuf-input-object-text-property 'appkit-chatbuf-input-object-text
  "Text property storing the original visible text of an input object.

This lets the shared repair pass reject an object whose interior was edited
without relying on protocol-specific object payloads.")

(defvar-local appkit-chatbuf-owns-wrap-prefix-p nil
  "Non-nil when the current Appkit chat renderer owns continuation layout.

Word-wrapping coordinators can use this capability to avoid enabling an
automatic `wrap-prefix' producer such as `visual-wrap-prefix-mode'.")

(defvar appkit-chatbuf--input-cache)

(defun appkit-chatbuf-copy-string (value)
  "Return VALUE copied, preserving text properties when it is a string."
  (if (stringp value)
      (copy-sequence value)
    ""))

(defun appkit-chatbuf-string-plain-text (value)
  "Return VALUE without text properties, or an empty string."
  (substring-no-properties (or value "")))

(defun appkit-chatbuf-string-has-objects-p (value)
  "Return non-nil when VALUE contains structured input objects."
  (and (stringp value)
       (text-property-not-all 0 (length value)
                              appkit-chatbuf-input-object-property nil
                              value)))

(defun appkit-chatbuf--advance-composer-revision ()
  "Advance and return the current composer state revision."
  (setq appkit-chatbuf--composer-revision
        (1+ appkit-chatbuf--composer-revision)))

(defun appkit-chatbuf-composer-revision ()
  "Return the monotonic revision of the canonical input and aux state."
  appkit-chatbuf--composer-revision)

(defun appkit-chatbuf-input-state ()
  "Return canonical chat composer input, preserving text properties."
  (appkit-chatbuf-copy-string appkit-chatbuf--input-cache))

(cl-defun appkit-chatbuf-input-state-set (value &key reset-history-p)
  "Set canonical composer input to VALUE, preserving text properties.

When RESET-HISTORY-P is non-nil, clear shared history navigation state."
  (let ((new-value (appkit-chatbuf-copy-string value)))
    (unless (equal-including-properties new-value appkit-chatbuf--input-cache)
      (setq appkit-chatbuf--input-cache new-value)
      (appkit-chatbuf--advance-composer-revision)))
  (when reset-history-p
    (appkit-chatbuf-input-history-reset))
  (appkit-chatbuf-input-state))

(cl-defun appkit-chatbuf-input-state-clear (&key reset-history-p)
  "Clear canonical composer input.

When RESET-HISTORY-P is non-nil, clear shared history navigation state."
  (appkit-chatbuf-input-state-set "" :reset-history-p reset-history-p))

(cl-defun appkit-chatbuf-input-state-sync (&key (reset-history-p t))
  "Synchronize canonical composer input from the editable tail region.

Return a plist with keys `:value' and `:changed-p'.  When the live input text
changes including text properties, canonical state is updated and shared
history navigation state is reset when RESET-HISTORY-P is non-nil."
  (let* ((text (appkit-chatbuf-copy-string (or (appkit-chatbuf-input-string) "")))
         (current appkit-chatbuf--input-cache)
         (changed-p (not (equal-including-properties text current))))
    (when changed-p
      (setq appkit-chatbuf--input-cache text)
      (appkit-chatbuf--advance-composer-revision)
      (when reset-history-p
        (appkit-chatbuf-input-history-reset)))
    (list :value (appkit-chatbuf-copy-string appkit-chatbuf--input-cache)
          :changed-p changed-p)))

(defvar-local appkit-chatbuf--input-marker nil
  "Marker pointing to the start of the editable input region.")

(defvar-local appkit-chatbuf--prompt-marker nil
  "Marker pointing to the start of the current prompt button.")

(defvar-local appkit-chatbuf--prompt-button nil
  "Button object used for the visible chat prompt.")

(defvar-local appkit-chatbuf--input-ring nil
  "Ring containing shared chatbuf input history entries.")

(defvar-local appkit-chatbuf--input-idx nil
  "Current absolute position inside `appkit-chatbuf--input-ring'.")

(defvar-local appkit-chatbuf--input-pending nil
  "Input remembered before entering history navigation.")

(defvar-local appkit-chatbuf--input-cache ""
  "Canonical chat composer input, preserving structured text properties.")

(defvar-local appkit-chatbuf--composer-revision 0
  "Monotonic revision of canonical composer input and aux state.")

(defvar-local appkit-chatbuf--aux-plist nil
  "Current aux state plist for the active chat buffer.")

(defvar-local appkit-chatbuf--input-options-plist nil
  "Current input options plist for the active chat buffer.")

(defvar-local appkit-chatbuf--rendering nil
  "Non-nil while the owning chat buffer is performing a bulk redraw.")

(defvar-local appkit-chatbuf--timeline-mode nil
  "Minor mode function used for commands outside the composer.")

(defvar-local appkit-chatbuf-input-sync-function
  #'appkit-chatbuf-input-state-sync
  "Function that synchronizes canonical input after an editable change.

A nil value disables automatic canonical input synchronization.")

(defvar appkit-chatbuf--mutating-input nil
  "Dynamically non-nil while a compound composer mutation is in progress.")

(defun appkit-chatbuf-rendering-p ()
  "Return non-nil while shared chat buffer structure is being updated."
  appkit-chatbuf--rendering)

(defmacro appkit-chatbuf-with-generated-update (&rest body)
  "Run BODY as one generated Surface structural update."
  (declare (indent 0) (debug t))
  `(let ((appkit-generated-surface (appkit-current-surface))
         (appkit-chatbuf--rendering t))
     (unless (appkit-surface-p appkit-generated-surface)
       (error "Appkit generated chat update requires a live Surface"))
     (let ((inhibit-read-only t)
           (buffer-undo-list t)
           (appkit-old-modified-p (buffer-modified-p)))
       (unwind-protect
           (progn ,@body)
         (set-buffer-modified-p appkit-old-modified-p)))))

(define-button-type 'appkit-chatbuf-prompt
  :supertype 'button
  'face 'default
  'read-only t
  'front-sticky t
  'rear-nonsticky t
  'cursor-intangible t
  'inactive t
  'field 'appkit-chatbuf-prompt)

(defun appkit-chatbuf-init-state (&optional ring-size)
  "Initialize shared chat buffer state in the current buffer.

RING-SIZE overrides `appkit-chatbuf-input-ring-size' when non-nil.  Existing
markers and rings are reused when already present."
  (unless (markerp appkit-chatbuf--input-marker)
    (setq-local appkit-chatbuf--input-marker (make-marker)))
  (unless (markerp appkit-chatbuf--prompt-marker)
    (setq-local appkit-chatbuf--prompt-marker (make-marker)))
  (unless (ring-p appkit-chatbuf--input-ring)
    (setq-local appkit-chatbuf--input-ring
                (make-ring (max 1 (or ring-size appkit-chatbuf-input-ring-size)))))
  (unless (local-variable-p 'appkit-chatbuf--input-idx)
    (setq-local appkit-chatbuf--input-idx nil))
  (unless (local-variable-p 'appkit-chatbuf--input-pending)
    (setq-local appkit-chatbuf--input-pending nil))
  (unless (local-variable-p 'appkit-chatbuf--input-cache)
    (setq-local appkit-chatbuf--input-cache ""))
  (unless (local-variable-p 'appkit-chatbuf--aux-plist)
    (setq-local appkit-chatbuf--aux-plist nil))
  (unless (local-variable-p 'appkit-chatbuf--composer-revision)
    (setq-local appkit-chatbuf--composer-revision 0))
  (unless (local-variable-p 'appkit-chatbuf--input-options-plist)
    (setq-local appkit-chatbuf--input-options-plist nil))
  (unless (local-variable-p 'appkit-chatbuf--rendering)
    (setq-local appkit-chatbuf--rendering nil)))

(defun appkit-chatbuf-reset-state (&optional ring-size)
  "Reset shared chat buffer state in the current buffer.

This recreates prompt/input markers, clears prompt button state, allocates a
fresh input history ring, and resets aux/input-options/rendering state.
RING-SIZE overrides `appkit-chatbuf-input-ring-size' when non-nil."
  (appkit-chatbuf--advance-composer-revision)
  (when (markerp appkit-chatbuf--input-marker)
    (set-marker appkit-chatbuf--input-marker nil))
  (when (markerp appkit-chatbuf--prompt-marker)
    (set-marker appkit-chatbuf--prompt-marker nil))
  (setq-local appkit-chatbuf--input-marker (make-marker))
  (setq-local appkit-chatbuf--prompt-marker (make-marker))
  (setq-local appkit-chatbuf--prompt-button nil)
  (setq-local appkit-chatbuf--input-ring
              (make-ring (max 1 (or ring-size appkit-chatbuf-input-ring-size))))
  (setq-local appkit-chatbuf--input-idx nil)
  (setq-local appkit-chatbuf--input-pending nil)
  (setq-local appkit-chatbuf--input-cache "")
  (setq-local appkit-chatbuf--aux-plist nil)
  (setq-local appkit-chatbuf--input-options-plist nil)
  (setq-local appkit-chatbuf--rendering nil))

(defun appkit-chatbuf-set-soft-wrap (enabled)
  "Set renderer-compatible soft wrapping according to ENABLED.

When ENABLED is non-nil, use `visual-line-mode' so Emacs wraps at word
boundaries while honoring Appkit's `line-prefix' and `wrap-prefix' properties.
When ENABLED is nil, truncate long lines.  Optional visual filling follows
`appkit-chatbuf-use-visual-fill-column'.  This function never enables an
automatic continuation-prefix producer."
  (setq-local appkit-chatbuf-wrap-long-lines (not (null enabled)))
  (appkit-ui-set-soft-wrap enabled appkit-chatbuf-use-visual-fill-column)
  appkit-chatbuf-wrap-long-lines)

(defun appkit-chatbuf--buffer-substring-filter (beg end delete)
  "Copy BEG..END, removing presentation only before the editable input."
  (appkit-ui-buffer-substring-filter
   beg end delete (appkit-chatbuf-input-start-position)))

(defun appkit-chatbuf-mode-setup ()
  "Apply telega-like chat buffer defaults in the current buffer."
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local window-point-insertion-type t)
  (setq-local next-line-add-newlines nil)
  (setq-local filter-buffer-substring-function
              #'appkit-chatbuf--buffer-substring-filter)
  (setq-local next-screen-context-lines 0)
  (setq-local appkit-chatbuf-owns-wrap-prefix-p t)
  (appkit-chatbuf-set-soft-wrap appkit-chatbuf-wrap-long-lines)
  (when (fboundp 'cursor-intangible-mode)
    (cursor-intangible-mode 1))
  (when (fboundp 'cursor-sensor-mode)
    (cursor-sensor-mode 1)))

(defun appkit-chatbuf-prompt-start-position ()
  "Return current prompt start position, or nil when prompt is unavailable."
  (and (markerp appkit-chatbuf--prompt-marker)
       (eq (marker-buffer appkit-chatbuf--prompt-marker) (current-buffer))
       (marker-position appkit-chatbuf--prompt-marker)))

(defun appkit-chatbuf-input-start-position ()
  "Return current editable input start position, or nil when unavailable."
  (and (markerp appkit-chatbuf--input-marker)
       (eq (marker-buffer appkit-chatbuf--input-marker) (current-buffer))
       (marker-position appkit-chatbuf--input-marker)))

(defun appkit-chatbuf-input-region-bounds ()
  "Return current editable input bounds as (START . END), or nil."
  (when-let* ((start (appkit-chatbuf-input-start-position)))
    (when (<= start (point-max))
      (cons start (point-max)))))

(defun appkit-chatbuf-input-logical-end-position ()
  "Return logical end position of the current editable input region."
  (when (appkit-chatbuf-input-start-position)
    (point-max)))

(defun appkit-chatbuf-focus-input ()
  "Move point to the logical composer end and refresh point context.

Return the new point, or nil when the input region is unavailable.  This
function never changes modal editor state."
  (when-let* ((position (appkit-chatbuf-input-logical-end-position)))
    (goto-char position)
    (appkit-chatbuf-update-context-mode)
    position))

(defun appkit-chatbuf-capture-window-input-offsets ()
  "Return (WINDOW . OFFSET) pairs for windows whose point is in composer input."
  (let ((bounds (appkit-chatbuf-input-region-bounds))
        offsets)
    (when bounds
      (let ((start (car bounds))
            (end (cdr bounds)))
        (dolist (window (get-buffer-window-list (current-buffer) nil t))
          (let ((window-point (window-point window)))
            (when (and (<= start window-point)
                       (<= window-point end))
              (push (cons window (- window-point start)) offsets))))))
    offsets))

(defun appkit-chatbuf-restore-window-input-offsets (offsets)
  "Restore window points in OFFSETS relative to current composer input start."
  (let ((start (appkit-chatbuf-input-start-position))
        (logical-end (appkit-chatbuf-input-logical-end-position)))
    (when (and (number-or-marker-p start)
               (number-or-marker-p logical-end))
      (dolist (entry offsets)
        (let ((window (car entry))
              (offset (cdr entry)))
          (when (and (window-live-p window)
                     (eq (window-buffer window) (current-buffer)))
            (set-window-point
             window
             (min logical-end
                  (max start (+ start offset))))))))))

(defun appkit-chatbuf-prompt-button-live-p ()
  "Return non-nil when the current prompt button is live in this buffer."
  (let ((prompt-start (appkit-chatbuf-prompt-start-position))
        (input-start (appkit-chatbuf-input-start-position)))
    (and prompt-start
         input-start
         (< prompt-start input-start)
         (let ((button (button-at prompt-start)))
           (and button
                (eq (button-get button 'field) 'appkit-chatbuf-prompt))))))

(defun appkit-chatbuf-point-in-input-p (&optional position)
  "Return non-nil when POSITION or point is inside the editable input region."
  (let* ((bounds (appkit-chatbuf-input-region-bounds))
         (pos (or position (point))))
    (and bounds
         (<= (car bounds) pos)
         (<= pos (cdr bounds)))))

(defun appkit-chatbuf-point-in-prompt-p (&optional position)
  "Return non-nil when POSITION or point is inside the prompt glyph span."
  (let ((prompt-start (appkit-chatbuf-prompt-start-position))
        (input-start (appkit-chatbuf-input-start-position))
        (pos (or position (point))))
    (and prompt-start
         input-start
         (>= pos prompt-start)
         (< pos input-start))))

(defun appkit-chatbuf-post-command-clamp-point ()
  "Keep point out of the prompt glyph span."
  (when (appkit-chatbuf-point-in-prompt-p)
    (when-let* ((input-start (appkit-chatbuf-input-start-position)))
      (goto-char input-start))))

(defun appkit-chatbuf-update-context-mode ()
  "Toggle the configured timeline mode for the current point context."
  (when-let* ((mode appkit-chatbuf--timeline-mode))
    (let ((timeline-p (not (appkit-chatbuf-point-in-input-p)))
          (active-p (and (boundp mode) (symbol-value mode))))
      (unless (eq (not (null active-p)) timeline-p)
        (funcall mode (if timeline-p 1 -1))
        (when (fboundp 'appkit-evil-normalize-keymaps)
          (appkit-evil-normalize-keymaps))))))

(defun appkit-chatbuf-use-timeline-mode (mode)
  "Use minor MODE for commands outside the trailing composer.

MODE must name a defined minor mode whose function and state variable share
that symbol.  Passing nil removes the current timeline mode."
  (unless (or (null mode)
              (and (symbolp mode) (fboundp mode) (boundp mode)))
    (error "Appkit timeline mode is unavailable: %S" mode))
  (let ((disabled-p
         (and appkit-chatbuf--timeline-mode
              (boundp appkit-chatbuf--timeline-mode)
              (symbol-value appkit-chatbuf--timeline-mode))))
    (when disabled-p
      (funcall appkit-chatbuf--timeline-mode -1))
    (setq-local appkit-chatbuf--timeline-mode mode)
    (appkit-chatbuf-update-context-mode)
    (when (and disabled-p (fboundp 'appkit-evil-normalize-keymaps))
      (appkit-evil-normalize-keymaps)))
  mode)

(defun appkit-chatbuf-post-command ()
  "Maintain shared composer and point-context invariants."
  (unless (appkit-chatbuf-rendering-p)
    (appkit-chatbuf-post-command-clamp-point)
    (when (and (appkit-chatbuf-point-in-input-p)
               (appkit-chatbuf-input-has-objects-p))
      (appkit-chatbuf-input-prune-broken-objects))
    (appkit-chatbuf-update-context-mode)))

(defun appkit-chatbuf--after-change (beg end old-length)
  "Synchronize canonical input after BEG to END replaces OLD-LENGTH chars."
  (unless appkit-chatbuf--mutating-input
    (appkit-chatbuf-after-change
     beg end
     :old-length old-length
     :rendering-p (appkit-chatbuf-rendering-p)
     :prune-broken-objects t
     :sync-function appkit-chatbuf-input-sync-function)))

(defvar appkit-chatbuf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "DEL") #'appkit-chatbuf-input-backward-delete)
    (define-key map (kbd "<backspace>")
                #'appkit-chatbuf-input-backward-delete)
    (define-key map (kbd "C-d") #'appkit-chatbuf-input-forward-delete)
    (define-key map (kbd "<delete>") #'appkit-chatbuf-input-forward-delete)
    (define-key map (kbd "M-p") #'appkit-chatbuf-input-history-prev)
    (define-key map (kbd "M-n") #'appkit-chatbuf-input-history-next)
    map)
  "Base keymap for editable Appkit chat buffers.")

(define-derived-mode appkit-chatbuf-mode nil "Appkit-Chat"
  "Base major mode for a generated timeline with a trailing composer."
  (appkit-chatbuf-mode-setup)
  (appkit-chatbuf-reset-state)
  (setq-local buffer-read-only nil)
  (add-hook 'after-change-functions #'appkit-chatbuf--after-change nil t)
  (add-hook 'post-command-hook #'appkit-chatbuf-post-command nil t))

(defun appkit-chatbuf--ensure-point-after-input-start ()
  "Move point to the end of buffer when it is before input start."
  (when-let* ((input-start (appkit-chatbuf-input-start-position)))
    (when (< (point) input-start)
      (goto-char (point-max)))))

(defun appkit-chatbuf--restore-input-point (input-offset)
  "Restore point inside the input region using INPUT-OFFSET when possible."
  (when (numberp input-offset)
    (when-let* ((input-start (appkit-chatbuf-input-start-position)))
      (goto-char (min (point-max)
                      (max input-start (+ input-start input-offset)))))))

(defun appkit-chatbuf-install-prompt (&optional prompt)
  "Ensure a prompt button exists at the end of the current buffer.

PROMPT defaults to `>>> '.  If a prompt already exists, update it in place."
  (interactive)
  (appkit-chatbuf-init-state)
  (if (appkit-chatbuf-prompt-button-live-p)
      (appkit-chatbuf-prompt-update prompt)
    (let ((prompt-text (or prompt ">>> "))
          (inhibit-read-only t))
      (goto-char (point-max))
      ;; The prompt lives after the EWOC footer.  Keep its marker advancing
      ;; when timeline rows are inserted at that boundary, but not while the
      ;; prompt text itself is being inserted.
      (set-marker-insertion-type appkit-chatbuf--prompt-marker nil)
      (set-marker appkit-chatbuf--prompt-marker (point) (current-buffer))
      (setq appkit-chatbuf--prompt-button
            (insert-text-button prompt-text 'type 'appkit-chatbuf-prompt))
      (set-marker-insertion-type appkit-chatbuf--prompt-marker t)
      (set-marker appkit-chatbuf--input-marker (point) (current-buffer))
      appkit-chatbuf--prompt-button)))

(defun appkit-chatbuf-prompt-update (&optional prompt)
  "Replace the visible prompt text with PROMPT.

PROMPT defaults to `>>> '.  Any existing input contents stay in place and
point is restored relative to the input start when it was inside the input.

The prompt is generated presentation, not composer input.  Replacing it does
not mark the buffer modified, enter undo history, or run input modification
hooks.  Text properties carried by PROMPT, including inline image display
properties, are preserved."
  (interactive)
  (appkit-chatbuf-init-state)
  (let* ((prompt-text (or prompt ">>> "))
         (prompt-start (or (appkit-chatbuf-prompt-start-position) (point-max)))
         (input-start (or (appkit-chatbuf-input-start-position) prompt-start))
         (in-input (appkit-chatbuf-point-in-input-p))
         (input-offset (and in-input (- (point) input-start)))
         (inhibit-read-only t))
    (with-silent-modifications
      (save-excursion
        (delete-region prompt-start input-start)
        (goto-char prompt-start)
        (set-marker-insertion-type appkit-chatbuf--prompt-marker nil)
        (set-marker appkit-chatbuf--prompt-marker (point) (current-buffer))
        (setq appkit-chatbuf--prompt-button
              (insert-text-button prompt-text 'type 'appkit-chatbuf-prompt))
        (set-marker-insertion-type appkit-chatbuf--prompt-marker t)
        (set-marker appkit-chatbuf--input-marker (point) (current-buffer))))
    (appkit-chatbuf--restore-input-point input-offset)
    appkit-chatbuf--prompt-button))

(defun appkit-chatbuf-clear-prompt-and-input ()
  "Remove the current prompt and input region from the buffer."
  (interactive)
  (let ((prompt-start (appkit-chatbuf-prompt-start-position))
        (inhibit-read-only t))
    (when prompt-start
      (delete-region prompt-start (point-max))))
  (setq appkit-chatbuf--prompt-button nil)
  (when (markerp appkit-chatbuf--prompt-marker)
    (set-marker appkit-chatbuf--prompt-marker nil))
  (when (markerp appkit-chatbuf--input-marker)
    (set-marker appkit-chatbuf--input-marker nil)))

(defun appkit-chatbuf-has-input-p ()
  "Return non-nil when the current chat buffer has some input text."
  (when-let* ((input-start (appkit-chatbuf-input-start-position)))
    (< input-start (point-max))))

(defun appkit-chatbuf-input-string ()
  "Return the current editable input string, preserving text properties."
  (when-let* ((bounds (appkit-chatbuf-input-region-bounds)))
    (buffer-substring (car bounds) (cdr bounds))))

(defun appkit-chatbuf-input-delete ()
  "Delete all current input contents, respecting the buffer's edit policy."
  (interactive)
  (barf-if-buffer-read-only)
  (when-let* ((input-start (appkit-chatbuf-input-start-position)))
    (delete-region input-start (point-max)))
  (unless (or appkit-chatbuf--mutating-input
              (appkit-chatbuf-rendering-p))
    (appkit-chatbuf-input-state-sync)))

(cl-defun appkit-chatbuf-input-set-text (text &key preserve-history-navigation-p)
  "Replace current input with TEXT, respecting the buffer's edit policy.

When PRESERVE-HISTORY-NAVIGATION-P is non-nil, retain the current history
cursor and remembered pending input.  A failed replacement leaves the input
text intact.  Authorized generated updates may bind `inhibit-read-only'."
  (barf-if-buffer-read-only)
  (appkit-chatbuf-init-state)
  (let ((appkit-chatbuf--mutating-input t))
    (atomic-change-group
      (appkit-chatbuf-input-delete)
      (when-let* ((input-start (appkit-chatbuf-input-start-position)))
        (save-excursion
          (goto-char input-start)
          (insert (or text "")))))
    (goto-char (point-max)))
  (unless (appkit-chatbuf-rendering-p)
    (appkit-chatbuf-input-state-sync
     :reset-history-p (not preserve-history-navigation-p))))

(defun appkit-chatbuf-input-replace (text)
  "Replace current input contents with TEXT, preserving point inside input.

If point was inside the input region before replacement, restore its relative
offset from the input start when possible."
  (let* ((input-start (appkit-chatbuf-input-start-position))
         (in-input (and input-start (appkit-chatbuf-point-in-input-p)))
         (input-offset (and in-input (- (point) input-start))))
    (appkit-chatbuf-input-set-text text)
    (appkit-chatbuf--restore-input-point input-offset)))

(defun appkit-chatbuf-protect-generated-content ()
  "Make generated content outside the trailing composer read-only."
  (let ((end (or (appkit-chatbuf-prompt-start-position) (point-max))))
    (with-silent-modifications
      (add-text-properties
       (point-min) end
       '(read-only t front-sticky (read-only)
                   rear-nonsticky (read-only))))))

(cl-defun appkit-chatbuf-bind-input-region (&key visible-p prompt input-text post-bind-function)
  "Ensure the tail input region matches VISIBLE-P, PROMPT and INPUT-TEXT.

When VISIBLE-P is nil, remove the current prompt and input region.  Otherwise,
install or update PROMPT, protect generated content, replace the editable input
with INPUT-TEXT, and call POST-BIND-FUNCTION when non-nil.  Callers can use
POST-BIND-FUNCTION for owner-specific text properties or local repair."
  (appkit-chatbuf-init-state)
  (if (not visible-p)
      (progn
        (appkit-chatbuf-clear-prompt-and-input)
        (appkit-chatbuf-protect-generated-content))
    (save-excursion
      (goto-char (point-max))
      (if (appkit-chatbuf-prompt-button-live-p)
          (appkit-chatbuf-prompt-update prompt)
        (appkit-chatbuf-install-prompt prompt)))
    (appkit-chatbuf-input-set-text input-text)
    (appkit-chatbuf-protect-generated-content)
    (appkit-chatbuf-input-apply-text-properties)
    (when (functionp post-bind-function)
      (funcall post-bind-function))))

(defun appkit-chatbuf-input-apply-text-properties ()
  "Normalize current input region after redraws and edits."
  (appkit-chatbuf-init-state)
  (when-let* ((bounds (appkit-chatbuf-input-region-bounds)))
    (with-silent-modifications
      (add-text-properties (car bounds) (cdr bounds)
                           '(read-only nil)))))

(defun appkit-chatbuf--change-overlaps-input-p (beg end old-length bounds)
  "Return non-nil when one after-change event touches input BOUNDS.

BEG, END, and OLD-LENGTH have the meaning documented for
`after-change-functions'.  In particular, deleting at the first input
character reports BEG equal to END; testing only the new non-empty span would
miss that edit and leave canonical composer state stale."
  (let ((input-start (car bounds))
        (input-end (cdr bounds)))
    (or
     ;; Inserted/replaced text intersects the current input interval.
     (and (< beg input-end)
          (> end input-start))
     ;; A deletion has an empty new interval.  Input is the persistent tail,
     ;; so a deletion beginning anywhere from its current start through end
     ;; belongs to it.  A deletion crossing the old marker moves that marker
     ;; to BEG and is covered by the same test.
     (and (> (or old-length 0) 0)
          (<= input-start beg)
          (<= beg input-end)))))

(cl-defun appkit-chatbuf-after-change
    (beg end &key old-length rendering-p sync-function prune-broken-objects)
  "Maintain shared input-region invariants after a buffer change.

BEG, END, and OLD-LENGTH describe the change as in `after-change-functions'.
When RENDERING-P is non-nil, do nothing.  Otherwise, if the change overlaps
the current input region, normalize input text properties, prune broken
structured objects when PRUNE-BROKEN-OBJECTS is non-nil, and then call
SYNC-FUNCTION when non-nil."
  (unless rendering-p
    (when-let* ((bounds (appkit-chatbuf-input-region-bounds)))
      (when (appkit-chatbuf--change-overlaps-input-p
             beg end old-length bounds)
        (appkit-chatbuf-input-apply-text-properties)
        (when prune-broken-objects
          (appkit-chatbuf-input-prune-broken-objects))
        (when (functionp sync-function)
          (funcall sync-function))))))

(defun appkit-chatbuf--next-input-object-start (position object limit)
  "Return the next object-start sentinel after POSITION, or LIMIT.

OBJECT is the optional string accepted by text-property functions; nil means
the current buffer."
  (let ((cursor position)
        found)
    (while (and (< cursor limit) (not found))
      (setq cursor
            (or (next-single-property-change
                 cursor appkit-chatbuf-input-object-start-property object limit)
                limit))
      (when (and (< cursor limit)
                 (get-text-property
                  cursor appkit-chatbuf-input-object-start-property object))
        (setq found cursor)))
    (or found limit)))

(defun appkit-chatbuf-next-input-object-change
    (position &optional object limit)
  "Return the next structured-object boundary after POSITION.

OBJECT is an optional string; nil means the current buffer.  LIMIT defaults to
the end of OBJECT or the current buffer.  Boundaries follow the per-occurrence
span id, while payload changes and start sentinels keep older input strings
without span ids readable."
  (let* ((finish (or limit (if (stringp object) (length object) (point-max))))
         (span-change
          (or (next-single-property-change
               position appkit-chatbuf-input-object-span-property object finish)
              finish))
         (payload-change
          (or (next-single-property-change
               position appkit-chatbuf-input-object-property object finish)
              finish))
         (next-start
          (appkit-chatbuf--next-input-object-start position object finish)))
    (min span-change payload-change next-start)))

(defun appkit-chatbuf-split-by-text-property (string property)
  "Split STRING by changes of text PROPERTY.

Mirrors `telega--split-by-text-prop' (used by telega-chatbuf input→IMC).
For `appkit-chatbuf-input-object-property', occurrence spans and legacy start
sentinels keep equal adjacent payloads as separate objects."
  (let ((finish (length string))
        (start 0)
        (pos 0)
        result)
    (while (< pos finish)
      (setq pos
            (if (eq property appkit-chatbuf-input-object-property)
                (appkit-chatbuf-next-input-object-change pos string finish)
              (or (next-single-property-change pos property string finish)
                  finish)))
      (push (substring string start pos) result)
      (setq start pos))
    (nreverse result)))

(cl-defun appkit-chatbuf-input-object-string (content object &key properties)
  "Return CONTENT encoded as one structured composer OBJECT string.

The returned string includes the trailing boundary spacer used by
`appkit-chatbuf-input-insert'.  PROPERTIES are additional text properties for
the complete object span.  This pure constructor lets clients serialize and
reorder objects without reimplementing Appkit's boundary contract."
  (unless (stringp content)
    (user-error "Appkit chat buffer: input content must be a string"))
  (when (or (null object) (string-empty-p content))
    (user-error "Appkit chat buffer: structured input objects need content and payload"))
  (dolist (property (list appkit-chatbuf-input-object-property
                          appkit-chatbuf-input-object-span-property
                          appkit-chatbuf-input-object-text-property
                          appkit-chatbuf-input-object-start-property
                          appkit-chatbuf-input-object-end-property))
    (when (plist-member properties property)
      (user-error "Appkit chat buffer: %S is a reserved object property" property)))
  (let* ((text (concat content " "))
         (span-id (make-symbol "appkit-chatbuf-input-object-span"))
         (body-end (1- (length text))))
    (add-text-properties
     0 (length text)
     (append
      (list 'face 'appkit-chatbuf-input-object)
      properties
      (list appkit-chatbuf-input-object-property object
            appkit-chatbuf-input-object-span-property span-id
            appkit-chatbuf-input-object-text-property
            (substring-no-properties content)
            'cursor-intangible t))
     text)
    (add-text-properties
     0 1 (list appkit-chatbuf-input-object-start-property t) text)
    (add-text-properties
     body-end (length text)
     (list appkit-chatbuf-input-object-end-property t
           'rear-nonsticky t
           'cursor-intangible nil)
     text)
    text))

(cl-defun appkit-chatbuf-input-insert (content &key object properties)
  "Insert CONTENT into the current input region.

CONTENT must be a string.  When OBJECT is non-nil, tag the inserted text as a
structured input object using `appkit-chatbuf-input-object-property'.  Extra
PROPERTIES are appended to the inserted text properties.

Attachment layout follows telega's `telega-chatbuf-input-insert':

1. If point is inside an existing object, move to its trailing boundary first;
   point exactly at its start remains before it.
2. Object body carries the object property (+ optional face).
3. First char gets `appkit-chatbuf-input-object-start-property' (telega
   `attach-open-bracket').
4. A trailing space after the body carries
   `appkit-chatbuf-input-object-end-property' and `rear-nonsticky t'
   (telega `attach-close-bracket' + `rear-nonsticky t' on the spacer).

The trailing spacer is what keeps following typed text (e.g. Chinese after an
image) from inheriting the object property — default Emacs rear-stickiness
would otherwise glue it into the attachment."
  (unless (stringp content)
    (user-error "Appkit chat buffer: input content must be a string"))
  (when (and object (string-empty-p content))
    (user-error "Appkit chat buffer: structured input objects need visible text"))
  (appkit-chatbuf-init-state)
  (appkit-chatbuf--ensure-point-after-input-start)
  (when-let* ((input-start (appkit-chatbuf-input-start-position)))
    (when (< (point) input-start)
      (goto-char (point-max))))
  ;; Never split an atomic object by inserting into its body or end sentinel.
  ;; At its exact start, point represents the boundary before the object and
  ;; insertion there remains meaningful.
  (when (and object
             (appkit-chatbuf-input-start-position)
             (>= (point) (appkit-chatbuf-input-start-position))
             (appkit-chatbuf-input-object-at-point (point)))
    (when-let* ((bounds (appkit-chatbuf-input-object-bounds-at-point (point))))
      (unless (= (point) (car bounds))
        (goto-char (cdr bounds)))))
  (if (not object)
      (let ((start (point)))
        (insert content)
        (when (and properties (< start (point)))
          (add-text-properties start (point) properties)))
    ;; Structured object (telega `telega-chatbuf-input-insert' pattern).
    ;;
    ;; Inhibit modification hooks while the object is half-built: after-change
    ;; prune would otherwise see start-without-end and delete the body (image
    ;; labels with `display' properties hit this immediately).  telega applies
    ;; open/close brackets before post-command validation runs.
    (let ((inhibit-modification-hooks t))
      (insert (appkit-chatbuf-input-object-string
               content object :properties properties))))
  (unless (appkit-chatbuf-rendering-p)
    (appkit-chatbuf-input-state-sync)))

(defun appkit-chatbuf-input-object-at-point (&optional position)
  "Return structured input object at POSITION or point, or nil."
  (get-text-property (or position (point)) appkit-chatbuf-input-object-property))

(defun appkit-chatbuf-input-has-objects-p ()
  "Return non-nil when current input contains structured objects."
  (when-let* ((bounds (appkit-chatbuf-input-region-bounds)))
    (text-property-not-all (car bounds) (cdr bounds)
                           appkit-chatbuf-input-object-property nil)))

(defun appkit-chatbuf--input-object-region-start (position)
  "Return start position of object region containing POSITION, or nil."
  (when (appkit-chatbuf-input-object-at-point position)
    (let* ((lower (or (appkit-chatbuf-input-start-position) (point-min)))
           (span-id
            (get-text-property
             position appkit-chatbuf-input-object-span-property)))
      (if span-id
          (or (previous-single-property-change
               (min (point-max) (1+ position))
               appkit-chatbuf-input-object-span-property nil lower)
              lower)
        ;; Compatibility for strings created before occurrence ids existed.
        (let ((cursor (min (point-max) (1+ position)))
              found)
          (when (get-text-property
                 position appkit-chatbuf-input-object-start-property)
            (setq found position))
          (while (and (> cursor lower) (not found))
            (setq cursor
                  (or (previous-single-property-change
                       cursor appkit-chatbuf-input-object-start-property
                       nil lower)
                      lower))
            (when (get-text-property
                   cursor appkit-chatbuf-input-object-start-property)
              (setq found cursor)))
          (or found
              (previous-single-property-change
               (min (point-max) (1+ position))
               appkit-chatbuf-input-object-property nil lower)
              lower))))))

(defun appkit-chatbuf-input-object-bounds-at-point (&optional position)
  "Return bounds of structured input object at POSITION or point, or nil."
  (let ((pos (or position (point))))
    (when (appkit-chatbuf-input-object-at-point pos)
      (let* ((start (appkit-chatbuf--input-object-region-start pos))
             (end (appkit-chatbuf-next-input-object-change
                   pos nil (point-max))))
        (cons start end)))))

(defun appkit-chatbuf--delete-region-expanding-input-objects (beg end)
  "Delete BEG through END, expanding partial structured-object edges."
  (let ((expanded-beg beg)
        (expanded-end end))
    (when (< expanded-beg expanded-end)
      (when-let* ((object-bounds
                   (appkit-chatbuf-input-object-bounds-at-point expanded-beg)))
        (setq expanded-beg (car object-bounds)))
      (when-let* ((object-bounds
                   (appkit-chatbuf-input-object-bounds-at-point
                    (1- expanded-end))))
        (setq expanded-end (cdr object-bounds))))
    (delete-region expanded-beg expanded-end)))

(defun appkit-chatbuf-input-backward-delete (n)
  "Delete N composer units backward, treating each input object atomically.

Outside the active composer this behaves like
`backward-delete-char-untabify'.  A negative N delegates to
`appkit-chatbuf-input-forward-delete'."
  (interactive "p")
  (cond
   ((< n 0)
    (appkit-chatbuf-input-forward-delete (- n)))
   ((not (appkit-chatbuf-point-in-input-p))
    (backward-delete-char-untabify n))
   (t
    (unwind-protect
        (if (use-region-p)
            (appkit-chatbuf--delete-region-expanding-input-objects
             (region-beginning) (region-end))
          (dotimes (_ n)
            (let ((input-start (appkit-chatbuf-input-start-position)))
              (when (or (not input-start) (<= (point) input-start))
                (user-error "Beginning of composer input"))
              (if-let* ((object-bounds
                         (appkit-chatbuf-input-object-bounds-at-point
                          (1- (point)))))
                  (delete-region (car object-bounds) (cdr object-bounds))
                (backward-delete-char-untabify 1)))))
      (unless (appkit-chatbuf-rendering-p)
        (appkit-chatbuf-input-state-sync))))))

(defun appkit-chatbuf-input-forward-delete (n)
  "Delete N composer units forward, treating each input object atomically.

Outside the active composer this behaves like `delete-char'.  A
negative N delegates to `appkit-chatbuf-input-backward-delete'."
  (interactive "p")
  (cond
   ((< n 0)
    (appkit-chatbuf-input-backward-delete (- n)))
   ((not (appkit-chatbuf-point-in-input-p))
    (delete-char n))
   (t
    (unwind-protect
        (if (use-region-p)
            (appkit-chatbuf--delete-region-expanding-input-objects
             (region-beginning) (region-end))
          (dotimes (_ n)
            (when (>= (point) (point-max))
              (user-error "End of composer input"))
            (if-let* ((object-bounds
                       (appkit-chatbuf-input-object-bounds-at-point (point))))
                (delete-region (car object-bounds) (cdr object-bounds))
              (delete-char 1))))
      (unless (appkit-chatbuf-rendering-p)
        (appkit-chatbuf-input-state-sync))))))

(defun appkit-chatbuf-input-prune-broken-objects ()
  "Delete structured input objects whose boundary markers became invalid."
  (when-let* ((bounds (appkit-chatbuf-input-region-bounds)))
    (let ((pos (car bounds))
          (limit (cdr bounds))
          (inhibit-read-only t))
      (while (< pos limit)
        (let ((object (get-text-property pos appkit-chatbuf-input-object-property)))
          (if object
              (let* ((start (or (appkit-chatbuf--input-object-region-start pos) pos))
                     (end (appkit-chatbuf-next-input-object-change pos nil limit))
                     (valid-start (get-text-property start
                                                     appkit-chatbuf-input-object-start-property))
                     (valid-end (get-text-property (1- end)
                                                   appkit-chatbuf-input-object-end-property))
                     (expected-text
                      (get-text-property
                       start appkit-chatbuf-input-object-text-property))
                     (actual-text
                      (and valid-end
                           (buffer-substring-no-properties start (1- end)))))
                (if (and valid-start
                         valid-end
                         (or (not (stringp expected-text))
                             (equal expected-text actual-text)))
                    (setq pos end)
                  (delete-region start end)
                  (setq limit (point-max))
                  (setq pos start)))
            (setq pos (appkit-chatbuf-next-input-object-change
                       pos nil limit))))))))

(defun appkit-chatbuf-aux-set (aux-plist)
  "Replace current aux state with AUX-PLIST and return it."
  (unless (equal aux-plist appkit-chatbuf--aux-plist)
    (setq appkit-chatbuf--aux-plist aux-plist)
    (appkit-chatbuf--advance-composer-revision))
  appkit-chatbuf--aux-plist)

(defun appkit-chatbuf-aux-reset ()
  "Clear current aux state and return nil."
  (appkit-chatbuf-aux-set nil))

(defun appkit-chatbuf-aux-state ()
  "Return current shared aux state plist, or nil."
  appkit-chatbuf--aux-plist)

(defun appkit-chatbuf-aux-type ()
  "Return current shared aux type, or nil."
  (plist-get appkit-chatbuf--aux-plist :aux-type))

(defun appkit-chatbuf-aux-message-id ()
  "Return current shared aux message id, or nil."
  (plist-get appkit-chatbuf--aux-plist :message-id))

(defun appkit-chatbuf-aux-active-p ()
  "Return non-nil when a shared aux state is currently active."
  (not (null appkit-chatbuf--aux-plist)))

(defun appkit-chatbuf--aux-one-line (text width face)
  "Return TEXT normalized, styled with FACE, and truncated to WIDTH."
  (let* ((normalized
          (string-trim
           (replace-regexp-in-string "[[:space:]]+" " " (or text ""))))
         (line (truncate-string-to-width normalized (max 1 width)
                                         nil nil "…")))
    (when (and face (not (string-empty-p line)))
      (add-face-text-property 0 (length line) face 'append line))
    line))

(cl-defun appkit-chatbuf-aux-render
    (&key title preview cancel-action cancel-help title-face preview-face
          accent-face width)
  "Return a unified two-line composer context card.

TITLE is client-owned presentation text.  PREVIEW must be an
`appkit-ui-one-line-preview'.  Both rows are bounded to WIDTH, which defaults
to the current `fill-column'.  CANCEL-ACTION is an optional zero-argument
function attached to the visible ×.  Client-specific faces may be supplied
with TITLE-FACE, PREVIEW-FACE, and ACCENT-FACE; shared theme-friendly faces
are used otherwise.  The returned string ends in one newline, or is empty
when TITLE is empty."
  (let* ((card-width (max 12 (or width
                                 (and (integerp fill-column) fill-column)
                                 80)))
         (content-width (max 1 (- card-width 4)))
         (title
          (appkit-chatbuf--aux-one-line
           title content-width (or title-face 'appkit-chatbuf-aux-title)))
         (preview
          (appkit-ui-render-one-line-preview
           preview content-width
           :face (or preview-face 'appkit-chatbuf-aux-preview))))
    (if (string-empty-p title)
        ""
      (let* ((accent
              (propertize "▏" 'face
                          (or accent-face 'appkit-chatbuf-aux-accent)))
             (close
              (if (functionp cancel-action)
                  (propertize
                   "×"
                   'face 'appkit-chatbuf-aux-close
                   appkit-ui-action-property cancel-action
                   'keymap appkit-ui-action-map
                   'mouse-face 'highlight
                   'pointer 'hand
                   'help-echo (or cancel-help "Cancel composer context")
                   'rear-nonsticky
                   (list 'face 'keymap 'mouse-face 'pointer 'help-echo
                         appkit-ui-action-property))
                " "))
             (card
              (concat close " " accent " " title "\n"
                      (unless (string-empty-p preview)
                        (concat "  " accent " " preview "\n")))))
        (add-text-properties 0 (length card)
                             '(appkit-chatbuf-aux-ui t)
                             card)
        card))))

(defun appkit-chatbuf-composer-idle-p ()
  "Return non-nil when the canonical composer has no active content.

Whitespace-only text is idle.  Shared aux state and structured input objects
remain active even when their plain visible text would otherwise look empty."
  (let ((input (appkit-chatbuf-input-state)))
    (and (not (appkit-chatbuf-aux-active-p))
         (not (appkit-chatbuf-string-has-objects-p input))
         (string-empty-p
          (string-trim
           (appkit-chatbuf-string-plain-text input))))))

(defun appkit-chatbuf-input-options-set (options-plist)
  "Replace current input options state with OPTIONS-PLIST and return it."
  (setq appkit-chatbuf--input-options-plist options-plist))

(defun appkit-chatbuf-input-options-reset ()
  "Clear current input options state and return nil."
  (setq appkit-chatbuf--input-options-plist nil))

(defun appkit-chatbuf-input-options-state ()
  "Return current shared input options plist, or nil."
  appkit-chatbuf--input-options-plist)

(defun appkit-chatbuf-input-option (key &optional default)
  "Return input option KEY from shared state, or DEFAULT when missing."
  (if (plist-member appkit-chatbuf--input-options-plist key)
      (plist-get appkit-chatbuf--input-options-plist key)
    default))

(defun appkit-chatbuf-input-history-push (&optional input)
  "Push INPUT into shared input history when it is plain text and non-empty.

When INPUT is nil, use current input contents.  Structured-object input is not
stored yet because history semantics for mixed object/text entries are not
finalized.  When INPUT is non-nil, object detection uses INPUT's text
properties instead of inspecting the live buffer contents."
  (let* ((value (or input (appkit-chatbuf-input-string)))
         (has-objects (and value
                           (if input
                               (text-property-not-all
                                0 (length value)
                                appkit-chatbuf-input-object-property nil
                                value)
                             (appkit-chatbuf-input-has-objects-p)))))
    (unless (or (null value)
                has-objects
                (string-empty-p (string-trim-right (substring-no-properties value)))
                (and (ring-p appkit-chatbuf--input-ring)
                     (> (ring-length appkit-chatbuf--input-ring) 0)
                     (equal value (ring-ref appkit-chatbuf--input-ring 0))))
      (ring-insert appkit-chatbuf--input-ring value)))
  (appkit-chatbuf-input-history-reset))

(defun appkit-chatbuf-input-history-goto (index)
  "Replace current input with history entry INDEX.

When INDEX is nil, restore pending input remembered before history navigation."
  (unless (ring-p appkit-chatbuf--input-ring)
    (user-error "Appkit chat buffer: input history is unavailable"))
  (unless appkit-chatbuf--input-idx
    (setq appkit-chatbuf--input-pending (or (appkit-chatbuf-input-string) "")))
  (setq appkit-chatbuf--input-idx index)
  (cond
   ((null index)
    (appkit-chatbuf-input-set-text
     (or appkit-chatbuf--input-pending "")
     :preserve-history-navigation-p t))
   ((or (< index 0) (>= index (ring-length appkit-chatbuf--input-ring)))
    (user-error "Appkit chat buffer: history index %s is out of range" index))
   (t
    (appkit-chatbuf-input-set-text
     (ring-ref appkit-chatbuf--input-ring index)
     :preserve-history-navigation-p t))))

(defun appkit-chatbuf-input-history-elements ()
  "Return current input history entries from newest to oldest."
  (let (items)
    (when (ring-p appkit-chatbuf--input-ring)
      (dotimes (idx (ring-length appkit-chatbuf--input-ring))
        (push (ring-ref appkit-chatbuf--input-ring idx) items)))
    (nreverse items)))

(defun appkit-chatbuf-input-history-reset ()
  "Clear shared input-history navigation state without altering history items."
  (setq appkit-chatbuf--input-idx nil)
  (setq appkit-chatbuf--input-pending nil))

(defun appkit-chatbuf-input-history-active-p ()
  "Return non-nil while the composer is navigating input history."
  (or (integerp appkit-chatbuf--input-idx)
      (not (null appkit-chatbuf--input-pending))))

(defun appkit-chatbuf-input-history-prev-value (current-input &optional n)
  "Return N older history entries for cached CURRENT-INPUT.

CURRENT-INPUT is remembered as the pending latest value the first time history
navigation moves away from it.  The return value is a plist with `:status' set
to either `ok' or `empty'.  When the status is `ok', `:value' contains the
selected history string, preserving text properties of CURRENT-INPUT when it is
restored later via `appkit-chatbuf-input-history-next-value'."
  (let* ((step (max 1 (or n 1)))
         (ring-size (and (ring-p appkit-chatbuf--input-ring)
                         (ring-length appkit-chatbuf--input-ring))))
    (if (or (null ring-size) (= ring-size 0))
        (list :status 'empty)
      (unless (integerp appkit-chatbuf--input-idx)
        (setq appkit-chatbuf--input-pending
              (appkit-chatbuf-copy-string current-input))
        (setq appkit-chatbuf--input-idx -1))
      (setq appkit-chatbuf--input-idx
            (min (1- ring-size) (+ appkit-chatbuf--input-idx step)))
      (list :status 'ok
            :value (appkit-chatbuf-copy-string
                    (ring-ref appkit-chatbuf--input-ring
                              appkit-chatbuf--input-idx))))))

(defun appkit-chatbuf-input-history-next-value (&optional n)
  "Return N newer history entries for cached chatbuf input state.

The return value is a plist with `:status' set to either `ok' or `latest'.
When the status is `ok', `:value' contains the selected history string or the
remembered pending latest input when navigation returns to the newest state."
  (let ((step (max 1 (or n 1))))
    (cond
     ((null appkit-chatbuf--input-idx)
      (list :status 'latest))
     ((<= appkit-chatbuf--input-idx (1- step))
      (let ((value (appkit-chatbuf-copy-string
                    (or appkit-chatbuf--input-pending ""))))
        (setq appkit-chatbuf--input-idx nil)
        (setq appkit-chatbuf--input-pending nil)
        (list :status 'ok :value value)))
     (t
      (setq appkit-chatbuf--input-idx (- appkit-chatbuf--input-idx step))
      (list :status 'ok
            :value (appkit-chatbuf-copy-string
                    (ring-ref appkit-chatbuf--input-ring
                              appkit-chatbuf--input-idx)))))))

(defun appkit-chatbuf-input-history-prev (&optional n)
  "Replace input with N previous entries from input history."
  (interactive "p")
  (let ((result (appkit-chatbuf-input-history-prev-value
                 (appkit-chatbuf-input-string)
                 n)))
    (pcase (plist-get result :status)
      ('ok
       (appkit-chatbuf-input-set-text
        (plist-get result :value)
        :preserve-history-navigation-p t))
      (_
       (user-error "Appkit chat buffer: input history is empty")))))

(defun appkit-chatbuf-input-history-next (&optional n)
  "Replace input with N newer entries from input history."
  (interactive "p")
  (let ((result (appkit-chatbuf-input-history-next-value n)))
    (pcase (plist-get result :status)
      ('ok
       (appkit-chatbuf-input-set-text
        (plist-get result :value)
        :preserve-history-navigation-p t))
      (_
       (user-error "Appkit chat buffer: already at latest input")))))

(provide 'appkit-chatbuf)

;;; appkit-chatbuf.el ends here
