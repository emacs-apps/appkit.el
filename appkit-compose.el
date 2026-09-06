;;; appkit-compose.el --- Compose editing and effect primitives  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Appkit does not define a generic Draft model.  A compose session only tracks
;; semantic source generations, captures immutable client-owned values, and
;; fences one view-local asynchronous effect.  Clients own persistence,
;; scheduling, close policy, transport outcomes, and every domain state.
;;
;; `appkit-chat-compose' is the concrete multi-item social composer.  Other
;; clients may attach these primitives to their own major modes and document
;; models without inheriting any save or submission semantics from Appkit.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-ui)

(cl-defstruct (appkit-compose-session
               (:constructor appkit-compose--make-session)
               (:copier nil)
               (:conc-name appkit-compose--))
  generation
  snapshot-function
  source-bounds-function
  state-change-function
  operation-sequence
  operation-owner
  operation-kind
  operation-generation
  operation-cancellation
  operation-cancel-requested-p
  label
  progress)

(defvar-local appkit-compose--session nil
  "Current buffer's compose editing session.")

(defvar appkit-compose--tracking-inhibited nil
  "Dynamically non-nil while generated edits must not advance a generation.")

(defmacro appkit-compose-without-tracking (&rest body)
  "Run BODY without treating its buffer edits as semantic source changes."
  (declare (indent 0) (debug t))
  `(let ((appkit-compose--tracking-inhibited t))
     ,@body))

(defun appkit-compose--check ()
  "Return the current compose session, or signal an error."
  (or (and (appkit-compose-session-p appkit-compose--session)
           appkit-compose--session)
      (error "Appkit compose session is not configured")))

(defun appkit-compose--default-snapshot ()
  "Return an owned plain-text snapshot of the current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun appkit-compose--valid-cancellation-p (object)
  "Return non-nil when OBJECT is a supported cancellation value."
  (or (null object) (functionp object) (appkit-handle-p object)))

(defun appkit-compose--retire-cancellation (object)
  "Retire cancellation OBJECT without invoking its side effect."
  (when (appkit-handle-p object)
    (appkit-retire-handle object)))

(defun appkit-compose--cancel-object (object)
  "Invoke cancellation OBJECT exactly once."
  (cond
   ((null object) nil)
   ((appkit-handle-p object) (appkit-cancel-handle object))
   ((functionp object) (funcall object))
   (t (error "Unsupported Appkit compose cancellation: %S" object))))

(defun appkit-compose--notify ()
  "Notify presentation after a compose session state change."
  (let* ((session (appkit-compose--check))
         (function (appkit-compose--state-change-function session)))
    (force-mode-line-update)
    (when function
      (funcall function session))))

(defun appkit-compose-generation ()
  "Return the current monotonic semantic source generation."
  (appkit-compose--generation (appkit-compose--check)))

(defun appkit-compose-operation-active-p ()
  "Return non-nil while one view-local compose effect is active."
  (and (appkit-compose--operation-owner (appkit-compose--check)) t))

(defun appkit-compose-operation-owner ()
  "Return the opaque owner of the current compose effect, or nil."
  (appkit-compose--operation-owner (appkit-compose--check)))

(defun appkit-compose-operation-kind ()
  "Return the client-supplied kind of the current compose effect, or nil."
  (appkit-compose--operation-kind (appkit-compose--check)))

(defun appkit-compose-operation-generation ()
  "Return the source generation captured by the current effect, or nil."
  (appkit-compose--operation-generation (appkit-compose--check)))

(defun appkit-compose-operation-cancel-requested-p ()
  "Return non-nil when cancellation was requested for the current effect."
  (and (appkit-compose--operation-cancel-requested-p
        (appkit-compose--check))
       t))

(defun appkit-compose-label ()
  "Return the presentation label of the current compose effect, or nil."
  (appkit-compose--label (appkit-compose--check)))

(defun appkit-compose-progress ()
  "Return the presentation progress of the current compose effect, or nil."
  (appkit-compose--progress (appkit-compose--check)))

(defun appkit-compose--source-bounds ()
  "Return current source bounds, or nil when the client has no source region."
  (let* ((session (appkit-compose--check))
         (function (appkit-compose--source-bounds-function session))
         (bounds (if function
                     (funcall function)
                   (cons (point-min) (point-max)))))
    (when bounds
      (unless (and (consp bounds)
                   (integer-or-marker-p (car bounds))
                   (integer-or-marker-p (cdr bounds))
                   (<= (point-min) (car bounds) (cdr bounds) (point-max)))
        (error "Appkit compose source bounds are invalid: %S" bounds))
      bounds)))

(defun appkit-compose-source-bounds ()
  "Return current immutable semantic source bounds, or nil."
  (when-let* ((bounds (appkit-compose--source-bounds)))
    (cons (car bounds) (cdr bounds))))

(defun appkit-compose--change-overlaps-source-p (beg end old-length bounds)
  "Return non-nil when a change at BEG..END touches source BOUNDS.

OLD-LENGTH has the meaning documented for `after-change-functions'."
  (let ((start (car bounds))
        (finish (cdr bounds)))
    (or (and (< beg finish) (> end start))
        (and (= beg end)
             (> (or old-length 0) 0)
             (<= start beg finish)))))

(defun appkit-compose-touch ()
  "Advance the semantic source generation and return its new value."
  (let ((session (appkit-compose--check)))
    (setf (appkit-compose--generation session)
          (1+ (appkit-compose--generation session)))
    (set-buffer-modified-p t)
    (appkit-compose--notify)
    (appkit-compose--generation session)))

(defun appkit-compose--after-change (beg end old-length)
  "Advance source state after BEG..END replaces OLD-LENGTH characters."
  (unless appkit-compose--tracking-inhibited
    (when-let* ((bounds (appkit-compose--source-bounds)))
      (when (appkit-compose--change-overlaps-source-p
             beg end old-length bounds)
        (appkit-compose-touch)))))

(defun appkit-compose-capture ()
  "Return an immutable client value with its exact source generation.

The result is a plist with `:generation' and `:value'."
  (let* ((session (appkit-compose--check))
         (function (appkit-compose--snapshot-function session)))
    (unless (functionp function)
      (error "Appkit compose snapshot function is unavailable"))
    (list :generation (appkit-compose--generation session)
          :value (funcall function))))

(cl-defun appkit-compose-operation-begin
    (kind &key generation label progress cancel-function)
  "Begin one view-local compose effect of KIND and return its owner.

GENERATION defaults to the current source generation.  LABEL and PROGRESS are
presentation only.  CANCEL-FUNCTION may be nil, an `appkit-handle', or a
zero-argument function.  Appkit does not assign transport or persistence
meaning to KIND or to eventual completion."
  (let ((session (appkit-compose--check)))
    (unless (symbolp kind)
      (error "Appkit compose operation kind must be a symbol: %S" kind))
    (when (appkit-compose-operation-active-p)
      (user-error "A compose operation is already in progress"))
    (setq generation (or generation (appkit-compose--generation session)))
    (unless (and (integerp generation)
                 (<= 0 generation (appkit-compose--generation session)))
      (error "Appkit compose operation generation is invalid: %S" generation))
    (unless (or (null label)
                (and (stringp label) (not (string-empty-p label))))
      (error "Appkit compose operation label must be nil or non-empty text"))
    (unless (or (null progress)
                (and (numberp progress) (<= 0 progress 1)))
      (error "Appkit compose progress must be nil or a 0-1 number"))
    (unless (appkit-compose--valid-cancellation-p cancel-function)
      (error "Appkit compose operation cancellation is invalid: %S"
             cancel-function))
    (let* ((sequence (1+ (appkit-compose--operation-sequence session)))
           (owner (list 'appkit-compose-operation sequence kind)))
      (setf (appkit-compose--operation-sequence session) sequence
            (appkit-compose--operation-owner session) owner
            (appkit-compose--operation-kind session) kind
            (appkit-compose--operation-generation session) generation
            (appkit-compose--operation-cancellation session) cancel-function
            (appkit-compose--operation-cancel-requested-p session) nil
            (appkit-compose--label session) label
            (appkit-compose--progress session) (and progress (float progress)))
      (appkit-compose--notify)
      owner)))

(defun appkit-compose-operation-current-p (owner)
  "Return non-nil when OWNER owns the current compose effect."
  (and owner
       (eq owner
           (appkit-compose--operation-owner (appkit-compose--check)))))

(cl-defun appkit-compose-operation-update
    (owner &key (label nil label-p)
           (progress nil progress-p)
           (cancel-function nil cancel-p))
  "Update current effect OWNER with LABEL, PROGRESS, or CANCEL-FUNCTION.

Omitted keyword arguments retain their current values.  Return non-nil when
OWNER was current; a stale owner has no effect."
  (when (appkit-compose-operation-current-p owner)
    (let ((session (appkit-compose--check)))
      (when label-p
        (unless (or (null label)
                    (and (stringp label) (not (string-empty-p label))))
          (error "Appkit compose operation label must be nil or non-empty text"))
        (setf (appkit-compose--label session) label))
      (when progress-p
        (unless (or (null progress)
                    (and (numberp progress) (<= 0 progress 1)))
          (error "Appkit compose progress must be nil or a 0-1 number"))
        (setf (appkit-compose--progress session) (and progress (float progress))))
      (when cancel-p
        (unless (appkit-compose--valid-cancellation-p cancel-function)
          (error "Appkit compose operation cancellation is invalid: %S"
                 cancel-function))
        (appkit-compose--retire-cancellation
         (appkit-compose--operation-cancellation session))
        (setf (appkit-compose--operation-cancellation session) cancel-function)
        (when (appkit-compose--operation-cancel-requested-p session)
          (setf (appkit-compose--operation-cancellation session) nil)
          (appkit-compose--cancel-object cancel-function)))
      (appkit-compose--notify)
      t)))

(defun appkit-compose-cancel-operation ()
  "Request cancellation of the current compose effect.

Return nil when no effect is active.  The owner remains current until the
client calls `appkit-compose-operation-finish'."
  (let* ((session (appkit-compose--check))
         (owner (appkit-compose--operation-owner session)))
    (when owner
      (when (appkit-compose--operation-cancel-requested-p session)
        (user-error "The current compose operation is already being canceled"))
      (setf (appkit-compose--operation-cancel-requested-p session) t)
      (let ((cancellation (appkit-compose--operation-cancellation session)))
        (when cancellation
          (setf (appkit-compose--operation-cancellation session) nil)
          (appkit-compose--cancel-object cancellation)))
      (appkit-compose--notify)
      owner)))

(defun appkit-compose-operation-finish (owner)
  "Retire current effect OWNER.

Return non-nil only when OWNER was current.  Appkit records no outcome; stale
or duplicate callbacks are inert."
  (when (appkit-compose-operation-current-p owner)
    (let* ((session (appkit-compose--check))
           (cancellation (appkit-compose--operation-cancellation session)))
      (setf (appkit-compose--operation-owner session) nil
            (appkit-compose--operation-kind session) nil
            (appkit-compose--operation-generation session) nil
            (appkit-compose--operation-cancellation session) nil
            (appkit-compose--operation-cancel-requested-p session) nil
            (appkit-compose--label session) nil
            (appkit-compose--progress session) nil)
      (appkit-compose--retire-cancellation cancellation)
      (appkit-compose--notify)
      t)))

(defun appkit-compose-progress-bar (progress &optional width)
  "Return a fixed presentation bar for PROGRESS using WIDTH columns."
  (appkit-ui-progress-bar progress width '(?= . ?>) ?\s))

(defun appkit-compose-status-text ()
  "Return presentation text for the current compose effect, or nil."
  (let* ((session (appkit-compose--check))
         (label
          (if (appkit-compose--operation-cancel-requested-p session)
              "Cancelling..."
            (appkit-compose--label session)))
         (progress (appkit-compose--progress session)))
    (when label
      (if (numberp progress)
          (format "%s  [%s]  %d%%"
                  label
                  (appkit-compose-progress-bar progress 10)
                  (round (* 100 progress)))
        label))))

(defun appkit-compose--field-text (field)
  "Return display text for status FIELD."
  (let ((label (plist-get field :label))
        (value (plist-get field :value)))
    (unless (stringp label)
      (error "Appkit compose status field needs a string label: %S" field))
    (if (null value) label (format "%s: %s" label value))))

(defun appkit-compose-status-field-string (field)
  "Return one propertized protocol-neutral compose status FIELD."
  (unless (listp field)
    (error "Appkit compose status field must be a plist: %S" field))
  (let ((text (appkit-compose--field-text field))
        (action (plist-get field :action))
        (face (plist-get field :face))
        (help-echo (plist-get field :help-echo)))
    (when (and action (not (functionp action)))
      (error "Appkit compose status field action is not callable: %S" field))
    (let ((map (and action (make-sparse-keymap))))
      (when action
        (define-key map [header-line down-mouse-1] #'ignore)
        (define-key map [header-line mouse-1]
                    (lambda () (interactive) (funcall action))))
      (apply #'propertize text
             (append (and face (list 'face face))
                     (and help-echo (list 'help-echo help-echo))
                     (and map (list 'local-map map
                                    'mouse-face 'mode-line-highlight)))))))

(defun appkit-compose-status-fields-string (fields)
  "Return compose status FIELDS as one generated line."
  (unless (or (null fields) (listp fields))
    (error "Appkit compose status fields must be a list: %S" fields))
  (mapconcat #'appkit-compose-status-field-string fields "   "))

(defun appkit-compose--shutdown ()
  "Invalidate and cancel the current compose effect."
  (when (appkit-compose-session-p appkit-compose--session)
    (let ((cancellation
           (appkit-compose--operation-cancellation appkit-compose--session)))
      (setf (appkit-compose--operation-owner appkit-compose--session) nil
            (appkit-compose--operation-kind appkit-compose--session) nil
            (appkit-compose--operation-generation appkit-compose--session) nil
            (appkit-compose--operation-cancellation appkit-compose--session) nil
            (appkit-compose--operation-cancel-requested-p
             appkit-compose--session) nil
             (appkit-compose--label appkit-compose--session) nil
             (appkit-compose--progress appkit-compose--session) nil)
      (appkit-compose--cancel-object cancellation))))

(cl-defun appkit-compose-reset (&key (generation 0))
  "Reset the current session to source GENERATION.

Any active effect is invalidated and canceled first."
  (unless (and (integerp generation) (>= generation 0))
    (error "Appkit compose generation must be non-negative"))
  (appkit-compose--shutdown)
  (let ((session (appkit-compose--check)))
    (setf (appkit-compose--generation session) generation)
    (set-buffer-modified-p nil)
    (appkit-compose--notify)
    session))

(cl-defun appkit-compose-setup
    (&key snapshot-function source-bounds-function state-change-function
          (generation 0))
  "Configure compose editing primitives in the current buffer.

SNAPSHOT-FUNCTION returns an owned client document.  SOURCE-BOUNDS-FUNCTION
optionally returns the editable source bounds; the whole buffer is used by
default.  STATE-CHANGE-FUNCTION receives the session after generation or effect
changes.  GENERATION establishes the initial source generation."
  (dolist (entry `((snapshot . ,snapshot-function)
                   (source-bounds . ,source-bounds-function)
                   (state-change . ,state-change-function)))
    (when (and (cdr entry) (not (functionp (cdr entry))))
      (error "Appkit compose %s function is not callable: %S"
             (car entry) (cdr entry))))
  (unless (and (integerp generation) (>= generation 0))
    (error "Appkit compose initial generation is invalid"))
  (when (appkit-compose-session-p appkit-compose--session)
    (appkit-compose--shutdown))
  (setq-local
   appkit-compose--session
   (appkit-compose--make-session
    :generation generation
    :snapshot-function (or snapshot-function #'appkit-compose--default-snapshot)
    :source-bounds-function source-bounds-function
    :state-change-function state-change-function
    :operation-sequence 0))
  (appkit-compose-session-mode 1)
  (set-buffer-modified-p nil)
  (appkit-compose--notify)
  appkit-compose--session)

(define-minor-mode appkit-compose-session-mode
  "Attach compose editing and effect primitives to the current buffer."
  :init-value nil
  :lighter nil
  (if appkit-compose-session-mode
      (progn
        (unless (appkit-compose-session-p appkit-compose--session)
          (setq-local
           appkit-compose--session
           (appkit-compose--make-session
            :generation 0
            :snapshot-function #'appkit-compose--default-snapshot
            :operation-sequence 0)))
        (add-hook 'after-change-functions #'appkit-compose--after-change nil t)
        (add-hook 'kill-buffer-hook #'appkit-compose--shutdown nil t))
    (remove-hook 'after-change-functions #'appkit-compose--after-change t)
    (remove-hook 'kill-buffer-hook #'appkit-compose--shutdown t)
    (appkit-compose--shutdown)
    (setq-local appkit-compose--session nil)))

(provide 'appkit-compose)

;;; appkit-compose.el ends here
