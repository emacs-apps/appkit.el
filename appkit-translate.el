;;; appkit-translate.el --- Optional inline translation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Applications supply source identity, version and plain text.  This module
;; owns disposable translation state and presentation, not original messages.
;; Work starts only on explicit request and belongs to the supplied App/Surface.
;; Backends are small descriptors, not a registry or a fallback chain.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-projection)
(require 'appkit-surface)
(require 'appkit-task-queue)
(require 'appkit-ui)

(defgroup appkit-translate nil
  "Explicit inline translation for AppKit applications."
  :group 'applications)

(autoload 'appkit-translate-respond-backend "appkit-translate-respond")

(defcustom appkit-translate-target-language "zh"
  "Default target language for manually requested translations."
  :type 'string
  :group 'appkit-translate)

(defcustom appkit-translate-backend-function #'appkit-translate-respond-backend
  "Zero-argument function returning a translation backend descriptor.
A descriptor contains :id, an equal-comparable configuration identity;
:label, a human-readable service name; and :start, an asynchronous function.
START receives (SOURCE LANGUAGE RESOLVE REJECT), where RESOLVE accepts the
translated string and REJECT a readable error string.  It returns a zero-arg
cancel function, an `appkit-handle', or nil for logical cancellation only.
Applications may explicitly select a different backend, never as fallback.
The default loads Respond only when a translation is requested."
  :type 'function
  :group 'appkit-translate)

(defface appkit-translate-face
  '((t :inherit font-lock-doc-face))
  "Face for translated text, distinct from the untouched original."
  :group 'appkit-translate)

(cl-defstruct (appkit-translate-context
               (:constructor appkit-translate--context-create)
               (:copier nil))
  owner
  queue
  states
  notify)

(defvar appkit-translate--contexts
  (make-hash-table :test #'eq :weakness 'key)
  "Active translation contexts keyed by their live AppKit owner.")

(defun appkit-translate-context-live-p (context)
  "Return non-nil when CONTEXT can accept work."
  (and (appkit-translate-context-p context)
       (appkit-task-queue-live-p (appkit-translate-context-queue context))))

(defun appkit-translate-enable (owner &optional notify)
  "Enable inline translation on live AppKit OWNER with optional NOTIFY.
NOTIFY receives a source key when a translation state changes.  When omitted
on a Generated Surface, NOTIFY defaults to posting an `appkit-projection-change'
with the source key.  Returns the translation context."
  (unless (appkit-owner-live-p owner)
    (error "Cannot enable translation on unavailable owner"))
  (let ((context (gethash owner appkit-translate--contexts)))
    (if (and context (appkit-translate-context-live-p context))
        (progn
          (when notify
            (setf (appkit-translate-context-notify context) notify))
          context)
      (let* ((queue (appkit-task-queue-create owner 2))
             (ctx (appkit-translate--context-create
                   :owner owner
                   :queue queue
                   :states (make-hash-table :test #'equal)
                   :notify (or notify
                               (lambda (key)
                                 (when (appkit-surface-live-p owner)
                                   (appkit-surface-post
                                    owner
                                    (appkit-projection-change-create
                                     :keys (list key)))))))))
        (puthash owner ctx appkit-translate--contexts)
        (appkit-register-handle
         owner 'translate ctx
         (lambda (_c)
           (remhash owner appkit-translate--contexts)))
        ctx))))

(defun appkit-translate--context-for-owner (owner &optional create notify)
  "Return the translation context for OWNER, creating when CREATE."
  (when (appkit-owner-live-p owner)
    (let ((context (gethash owner appkit-translate--contexts)))
      (if (and context (appkit-translate-context-live-p context))
          (progn
            (when notify
              (setf (appkit-translate-context-notify context) notify))
            context)
        (when create
          (appkit-translate-enable owner notify))))))

(defun appkit-translate--snapshot (value)
  "Copy source VALUE's Lisp data, including mutable strings and vectors."
  (cond ((stringp value) (copy-sequence value))
        ((consp value) (cons (appkit-translate--snapshot (car value))
                             (appkit-translate--snapshot (cdr value))))
        ((vectorp value) (apply #'vector (mapcar #'appkit-translate--snapshot value)))
        (t value)))

(defun appkit-translate-state (source &optional surface)
  "Return read-only translation state matching SOURCE in SURFACE, or nil.
SURFACE defaults to `appkit-current-surface'.  SOURCE contains :key and
:version.  Its version must identify the exact source content and representation.
If :text is supplied, compare that too.  Renderers can omit :text to avoid
extracting plain text again for unchanged content.  A source edit never
inherits a translation for an older version."
  (let* ((owner (or surface (appkit-current-surface)))
         (context (and owner (appkit-translate--context-for-owner owner nil))))
    (when context
      (let* ((state (gethash (plist-get source :key)
                             (appkit-translate-context-states context)))
             (saved (plist-get state :source)))
        (when (and state
                   (equal (plist-get source :version) (plist-get saved :version))
                   (or (not (plist-member source :text))
                       (equal (plist-get source :text) (plist-get saved :text))))
          state)))))

(defun appkit-translate--current-p (context state)
  "Whether STATE still owns its source slot in live CONTEXT."
  (and (appkit-translate-context-live-p context)
       (eq state (gethash (plist-get (plist-get state :source) :key)
                          (appkit-translate-context-states context)))))

(defun appkit-translate--notify (context state)
  "Notify CONTEXT's view about STATE without changing source content."
  (when (appkit-translate--current-p context state)
    (funcall (appkit-translate-context-notify context)
             (plist-get (plist-get state :source) :key))))

(defun appkit-translate-request (source &optional backend language force notify-or-surface surface)
  "Explicitly translate SOURCE in SURFACE with BACKEND into LANGUAGE.
SURFACE defaults to `appkit-current-surface'.  SOURCE is a plist with scoped
:key, immutable :version, plain :text, and optional opaque :data for its backend.
Source data is snapshotted here.
BACKEND defaults to `appkit-translate-backend-function'; LANGUAGE defaults to
`appkit-translate-target-language'.  Backend settings must be captured by its
factory, and reflected in its :id.  Identical completed results are reused;
FORCE requests a new translation.  No fallback or automatic retry occurs.
NOTIFY-OR-SURFACE can be a notification callback or an explicit surface."
  (let* ((notify (when (functionp notify-or-surface) notify-or-surface))
         (owner (or surface
                    (and (not (functionp notify-or-surface)) notify-or-surface)
                    (appkit-current-surface))))
    (unless (appkit-owner-live-p owner)
      (user-error "Translation view is closed"))
    (setq backend (or backend (funcall appkit-translate-backend-function))
          language (string-trim (or language appkit-translate-target-language)))
    (when (string-empty-p language) (user-error "Translation language is empty"))
    (unless (and (stringp (plist-get source :text))
                 (not (string-empty-p (string-trim (plist-get source :text)))))
      (user-error "There is no text to translate"))
    (let* ((context (appkit-translate-enable owner notify))
           (key (plist-get source :key))
           (queue (appkit-translate-context-queue context))
           (old (appkit-translate-state source owner)))
      (if (and (not force) old
               (equal language (plist-get old :language))
               (equal (plist-get backend :id) (plist-get (plist-get old :backend) :id))
               (memq (plist-get old :status) '(queued running completed)))
          (progn
            (setf (plist-get old :visible) t)
            (appkit-translate--notify context old)
            old)
        (let ((state (list :source (appkit-translate--snapshot source)
                           :backend (copy-sequence backend)
                           :language (copy-sequence language)
                           :status 'queued :visible t :text "" :error nil)))
          ;; Replace authority before cancelling: even reentrant old callbacks
          ;; and previously rendered buttons cannot mutate the new translation.
          (puthash key state (appkit-translate-context-states context))
          (appkit-task-queue-cancel-key queue key)
          (appkit-translate--notify context state)
          (appkit-task-queue-submit
           queue key
           (lambda (complete)
             (setf (plist-get state :status) 'running)
             (appkit-translate--notify context state)
             (condition-case err
                 (funcall (plist-get backend :start)
                          (plist-get state :source) (plist-get state :language)
                          (lambda (text)
                            (if (and (stringp text) (not (string-empty-p (string-trim text))))
                                (funcall complete 'completed text)
                              (funcall complete 'failed "No translated text returned")))
                          (lambda (error) (funcall complete 'failed error)))
               (error (funcall complete 'failed (error-message-string err)) nil)))
           :finish
           (lambda (status value)
             (when (appkit-translate--current-p context state)
               (setf (plist-get state :status) status
                     (plist-get state :text) (if (eq status 'completed) value "")
                     (plist-get state :error) (and (eq status 'failed) value))
               (appkit-translate--notify context state))))
          state)))))

(defun appkit-translate-request-many
    (sources &optional backend language force notify-or-surface surface)
  "Explicitly translate SOURCES through one owner's existing bounded queue.
Arguments follow `appkit-translate-request'.  Capture the owner, backend and
target language once for the batch.  Return states in SOURCES order, retaining
per-source caching, errors and cancellation; requests are not merged on the wire.
An empty SOURCES list does not initialize a backend or translation context.
Callers select the scope and omit sources without translatable text."
  (when sources
    (let ((owner (or surface
                     (and (not (functionp notify-or-surface)) notify-or-surface)
                     (appkit-current-surface))))
      (unless (appkit-owner-live-p owner)
        (user-error "Translation view is closed"))
      (let ((backend (or backend (funcall appkit-translate-backend-function)))
            (language (copy-sequence (or language appkit-translate-target-language))))
        (mapcar
         (lambda (source)
           (appkit-translate-request
            source backend language force notify-or-surface owner))
         sources)))))

(defun appkit-translate-hide (source-or-state &optional surface)
  "Hide SOURCE-OR-STATE and revoke unfinished work, without affecting other sources."
  (let* ((owner (or surface (appkit-current-surface)))
         (context (and owner (appkit-translate--context-for-owner owner nil))))
    (when (and context (appkit-translate-context-live-p context))
      (let* ((state (if (plist-member source-or-state :source)
                        (and (eq source-or-state
                                 (gethash (plist-get (plist-get source-or-state :source) :key)
                                          (appkit-translate-context-states context)))
                             source-or-state)
                      (appkit-translate-state source-or-state owner))))
        (when (and state (appkit-translate--current-p context state))
          (setf (plist-get state :visible) nil)
          (when (memq (plist-get state :status) '(queued running))
            (setf (plist-get state :status) 'cancelled)
            (appkit-task-queue-cancel-key
             (appkit-translate-context-queue context)
             (plist-get (plist-get state :source) :key)))
          (appkit-translate--notify context state))))))
(defun appkit-translate--again (context state &optional force)
  "Show or explicitly repeat current STATE's exact request."
  (when (appkit-translate--current-p context state)
    (let ((owner (appkit-translate-context-owner context)))
      (appkit-translate-request
       (plist-get state :source)
       (plist-get state :backend)
       (plist-get state :language)
       force
       (appkit-translate-context-notify context)
       owner))))

(defun appkit-translate--button (label action)
  "Insert a LABEL action in the current generated translation region."
  (let ((start (point)))
    (insert label)
    (appkit-ui-add-action start (point) action
                          :face 'link)))

(defun appkit-translate--line (prefix prefix-face insert-body)
  "Insert one generated line using PREFIX and INSERT-BODY."
  (let ((text (appkit-ui-prefix-string prefix t)))
    (insert (if prefix-face (propertize text 'face prefix-face) text)))
  (let ((start (point)))
    (funcall insert-body)
    (insert "\n")
    (when prefix
      (let ((text (appkit-ui-prefix-string prefix)))
        (put-text-property start (point) 'wrap-prefix
                           (if prefix-face (propertize text 'face prefix-face) text))))))

(defun appkit-translate-insert (source &optional prefix prefix-face surface)
  "Insert matching SOURCE's translation region for SURFACE, without I/O.
SURFACE defaults to `appkit-current-surface'.  Original text is the caller's
responsibility and is never replaced.  PREFIX can be a string or AppKit mutable
prefix state for nested rows.  Actions bind the exact saved request."
  (let* ((owner (or surface (appkit-current-surface)))
         (context (and owner (appkit-translate--context-for-owner owner nil)))
         (state (and context (appkit-translate-state source owner))))
    (when state
      (appkit-translate--line
       prefix prefix-face
       (lambda ()
         (insert (propertize
                  (format "Translation · %s · %s  "
                          (plist-get state :language)
                          (plist-get (plist-get state :backend) :label))
                  'face 'shadow))
         (if (plist-get state :visible)
             (progn
               (appkit-translate--button "Hide" (lambda () (appkit-translate-hide state owner)))
               (insert " · ")
               (appkit-translate--button "Translate again"
                                         (lambda () (appkit-translate--again context state t))))
           (appkit-translate--button "Show" (lambda () (appkit-translate--again context state))))))
      (when (plist-get state :visible)
        (let* ((status (plist-get state :status))
               (text (pcase status
                       ('completed (plist-get state :text))
                       ('queued "Waiting to translate…")
                       ('running "Translating…")
                       ('failed (format "Translation failed: %s" (plist-get state :error)))
                       (_ "Translation cancelled")))
               (face (pcase status ('completed 'appkit-translate-face) ('failed 'error) (_ 'shadow))))
          (dolist (line (split-string text "\n" nil))
            (appkit-translate--line prefix prefix-face
                                    (lambda () (insert (propertize line 'face face))))))))))

(provide 'appkit-translate)
;;; appkit-translate.el ends here
