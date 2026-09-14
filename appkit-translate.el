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
  queue
  states
  notify)

(defun appkit-translate-context-create (owner notify)
  "Create a translation context owned by live AppKit OWNER.
NOTIFY receives a source key after a state change; schedule a targeted redraw
there.  Requests use an owner-scoped queue with two concurrent translations.
Stopping OWNER revokes pending delivery and requests transport cancellation."
  (appkit-translate--context-create
   :queue (appkit-task-queue-create owner 2)
   :states (make-hash-table :test #'equal)
   :notify notify))

(defun appkit-translate-context-live-p (context)
  "Return non-nil when CONTEXT can accept work."
  (and (appkit-translate-context-p context)
       (appkit-task-queue-live-p (appkit-translate-context-queue context))))

(defun appkit-translate--snapshot (value)
  "Copy source VALUE's Lisp data, including mutable strings and vectors."
  (cond ((stringp value) (copy-sequence value))
        ((consp value) (cons (appkit-translate--snapshot (car value))
                             (appkit-translate--snapshot (cdr value))))
        ((vectorp value) (apply #'vector (mapcar #'appkit-translate--snapshot value)))
        (t value)))

(defun appkit-translate-state (context source)
  "Return read-only translation state matching SOURCE in CONTEXT, or nil.
SOURCE contains :key and :version.  Its version must identify the exact source
content and representation.  If :text is supplied, compare that too.  Renderers
can omit :text to avoid extracting plain text again for unchanged content.
A source edit never inherits a translation for an older version."
  (when context
    (let* ((state (gethash (plist-get source :key)
                           (appkit-translate-context-states context)))
           (saved (plist-get state :source)))
      (when (and state
                 (equal (plist-get source :version) (plist-get saved :version))
                 (or (not (plist-member source :text))
                     (equal (plist-get source :text) (plist-get saved :text))))
        state))))

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

(defun appkit-translate-request (context source &optional backend language force)
  "Explicitly translate SOURCE in CONTEXT with BACKEND into LANGUAGE.
SOURCE is a plist with scoped :key, immutable :version, plain :text, and
optional opaque :data for its backend.  Source data is snapshotted here.
BACKEND defaults to `appkit-translate-backend-function'; LANGUAGE defaults to
`appkit-translate-target-language'.  Backend settings must be captured by its
factory, and reflected in its :id.  Identical completed results are reused;
FORCE requests a new translation.  No fallback or automatic retry occurs."
  (unless (appkit-translate-context-live-p context)
    (user-error "Translation view is closed"))
  (setq backend (or backend (funcall appkit-translate-backend-function))
        language (string-trim (or language appkit-translate-target-language)))
  (when (string-empty-p language) (user-error "Translation language is empty"))
  (unless (and (stringp (plist-get source :text))
               (not (string-empty-p (string-trim (plist-get source :text)))))
    (user-error "There is no text to translate"))
  (let* ((key (plist-get source :key))
         (queue (appkit-translate-context-queue context))
         (old (appkit-translate-state context source)))
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
        state))))

(defun appkit-translate--hide (context state)
  "Hide STATE and revoke unfinished work, without affecting other sources."
  (when (appkit-translate--current-p context state)
    (setf (plist-get state :visible) nil)
    (when (memq (plist-get state :status) '(queued running))
      (setf (plist-get state :status) 'cancelled)
      (appkit-task-queue-cancel-key
       (appkit-translate-context-queue context)
       (plist-get (plist-get state :source) :key)))
    (appkit-translate--notify context state)))

(defun appkit-translate--again (context state &optional force)
  "Show or explicitly repeat current STATE's exact request."
  (when (appkit-translate--current-p context state)
    (appkit-translate-request context (plist-get state :source)
                              (plist-get state :backend) (plist-get state :language) force)))

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

(defun appkit-translate-insert (context source &optional prefix prefix-face)
  "Insert matching SOURCE's translation region from CONTEXT, without I/O.
Original text is the caller's responsibility and is never replaced.  PREFIX
can be a string or AppKit mutable prefix state for nested rows.  Actions bind
the exact saved request, never the current point, account or backend setting."
  (when-let* ((state (appkit-translate-state context source)))
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
             (appkit-translate--button "Hide" (lambda () (appkit-translate--hide context state)))
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
                                  (lambda () (insert (propertize line 'face face)))))))))

(provide 'appkit-translate)
;;; appkit-translate.el ends here
