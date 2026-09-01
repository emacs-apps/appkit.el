;;; appkit-chat-history.el --- Continuous chat history windows  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Protocol-independent state for one continuous chat history window.
;;
;; Clients own protocol cursors, remote-frontier observations, read state, and
;; page-completion rules.  Appkit owns the keyed request operation, transport
;; lifecycle, exact projected-window invariants, stale callback fencing,
;; exhausted older history, stalled newer paging, strict edge slicing,
;; automatic edge gates, and passive delimiter presentation.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-surface)
(require 'appkit-scroll)

(cl-defstruct (appkit-chat-history--state
               (:constructor appkit-chat-history--state-create))
  "Protocol-independent state for one buffer-local history window."
  known-p
  empty-p
  loading
  older-loaded-p
  first-key
  last-key
  owner
  stalled-newer-key)

(cl-defstruct (appkit-chat-history-operation
               (:constructor appkit-chat-history-operation--create)
               (:copier nil))
  "One Surface-scoped history request fence."
  surface
  handle
  alive-p)

(defconst appkit-chat-history--request-key 'appkit-chat-history
  "Private history request key for a history controller request.")

(defvar-local appkit-chat-history--state nil
  "Current buffer's protocol-independent chat history state.")

(defun appkit-chat-history--new-state ()
  "Return a fresh unknown chat history state."
  (appkit-chat-history--state-create
   :known-p nil
   :empty-p nil
   :loading nil
   :older-loaded-p nil
   :first-key nil
   :last-key nil
   :owner nil
   :stalled-newer-key nil))

(defun appkit-chat-history-init-state ()
  "Initialize and return the current buffer's chat history state.

Existing state is preserved.  A newly initialized state has an unknown
window; mode construction must not imply that cached entries are contiguous."
  (unless (appkit-chat-history--state-p appkit-chat-history--state)
    (setq-local appkit-chat-history--state
                (appkit-chat-history--new-state)))
  appkit-chat-history--state)

(defun appkit-chat-history-reset-state ()
  "Cancel any active request and reset to a fresh unknown history state."
  (when (appkit-chat-history--state-p appkit-chat-history--state)
    (appkit-chat-history-request-cancel))
  (setq-local appkit-chat-history--state
              (appkit-chat-history--new-state)))

(defun appkit-chat-history-window-known-p ()
  "Return non-nil when the current buffer has one exact history window."
  (and (appkit-chat-history--state-known-p
        (appkit-chat-history-init-state))
       t))

(defun appkit-chat-history-window-partial-p ()
  "Return non-nil when newer entries lie outside the exact history window.

A non-nil last key is the exact newer cursor.  A nil last key on a known
window means that the window is attached to the live/latest edge."
  (and (appkit-chat-history-window-known-p)
       (appkit-chat-history--state-last-key
        (appkit-chat-history-init-state))
       t))

(defun appkit-chat-history-window-empty-p ()
  "Return non-nil when latest authoritatively established an empty window.

This is distinct from a known attached window whose nil first key leaves its
older edge unbounded.  An explicit empty window never projects unrelated
cached entries."
  (and (appkit-chat-history-window-known-p)
       (appkit-chat-history--state-empty-p
        (appkit-chat-history-init-state))
       t))

(defun appkit-chat-history-window-first-key ()
  "Return the exact older edge key of the current history window."
  (appkit-chat-history--state-first-key
   (appkit-chat-history-init-state)))

(defun appkit-chat-history-window-last-key ()
  "Return the exact newer edge key, or nil when attached to latest."
  (appkit-chat-history--state-last-key
   (appkit-chat-history-init-state)))

(defun appkit-chat-history-loading ()
  "Return the opaque loading kind of the current history request, or nil."
  (appkit-chat-history--state-loading
   (appkit-chat-history-init-state)))

(defun appkit-chat-history-loading-p ()
  "Return non-nil while the current history controller owns a request."
  (and (appkit-chat-history-loading) t))

(defun appkit-chat-history-older-loaded-p ()
  "Return non-nil when the current window reached its oldest edge."
  (and (appkit-chat-history--state-older-loaded-p
        (appkit-chat-history-init-state))
       t))

(defun appkit-chat-history-request-owner ()
  "Return the operation fence owning the active history request, or nil."
  (appkit-chat-history--state-owner
   (appkit-chat-history-init-state)))

(defun appkit-chat-history-window-set (first-key last-key)
  "Establish one exact history window from FIRST-KEY through LAST-KEY.

Both keys are opaque and compared only with `equal'.  Nil FIRST-KEY leaves the
older edge unbounded.  Nil LAST-KEY marks a known window attached to the
live/latest edge.  This establishes an ordinary, potentially non-empty window
and clears an authoritative-empty state.  Moving the newer edge clears a stall
recorded at the old edge."
  (let* ((state (appkit-chat-history-init-state))
         (old-last-key (appkit-chat-history--state-last-key state)))
    (setf (appkit-chat-history--state-known-p state) t
          (appkit-chat-history--state-empty-p state) nil
          (appkit-chat-history--state-first-key state) first-key
          (appkit-chat-history--state-last-key state) last-key)
    (unless (equal old-last-key last-key)
      (setf (appkit-chat-history--state-stalled-newer-key state) nil))
    state))

(defun appkit-chat-history-window-establish-empty ()
  "Establish an authoritative empty window attached to live/latest.

Unlike `appkit-chat-history-window-set' with two nil keys, this transition
guarantees that strict slicing returns no entries even if the client cache
contains unrelated islands.  Empty history has also reached its oldest edge."
  (let ((state (appkit-chat-history-init-state)))
    (setf (appkit-chat-history--state-known-p state) t
          (appkit-chat-history--state-empty-p state) t
          (appkit-chat-history--state-older-loaded-p state) t
          (appkit-chat-history--state-first-key state) nil
          (appkit-chat-history--state-last-key state) nil
          (appkit-chat-history--state-stalled-newer-key state) nil)
    state))

(defun appkit-chat-history-window-seed-live (key)
  "Seed authoritative empty history with its first live entry KEY.

Return the current state when the empty-to-live transition happened, or nil
when the window was not authoritatively empty.  KEY must be non-nil.  The new
one-entry window remains attached to latest and retains the fact that no older
history preceded it."
  (unless key
    (error "Appkit chat history live seed key must be non-nil"))
  (when (appkit-chat-history-window-empty-p)
    (let ((state (appkit-chat-history-init-state)))
      (setf (appkit-chat-history--state-empty-p state) nil
            (appkit-chat-history--state-first-key state) key
            (appkit-chat-history--state-last-key state) nil
            (appkit-chat-history--state-stalled-newer-key state) nil)
      state)))

(defun appkit-chat-history-window-clear ()
  "Make the current history window unknown without changing request ownership.

Exact edges, older exhaustion, and a newer stall only describe the discarded
window, so they are cleared together."
  (let ((state (appkit-chat-history-init-state)))
    (setf (appkit-chat-history--state-known-p state) nil
          (appkit-chat-history--state-empty-p state) nil
          (appkit-chat-history--state-older-loaded-p state) nil
          (appkit-chat-history--state-first-key state) nil
          (appkit-chat-history--state-last-key state) nil
          (appkit-chat-history--state-stalled-newer-key state) nil)
    state))

(defun appkit-chat-history-older-loaded-set (value)
  "Set whether the current history window reached its oldest edge to VALUE."
  (setf (appkit-chat-history--state-older-loaded-p
         (appkit-chat-history-init-state))
        (and value t)))

(defun appkit-chat-history-newer-stalled-set (&optional key)
  "Record that newer paging made no progress at KEY.

When KEY is nil, use the current exact newer edge.  The stall suppresses only
automatic retry at that same edge; moving the window edge clears it."
  (let* ((state (appkit-chat-history-init-state))
         (stalled-key (or key (appkit-chat-history--state-last-key state))))
    (setf (appkit-chat-history--state-stalled-newer-key state) stalled-key)
    stalled-key))

(defun appkit-chat-history-newer-stalled-clear ()
  "Clear the current window's automatic newer-paging stall."
  (setf (appkit-chat-history--state-stalled-newer-key
         (appkit-chat-history-init-state))
        nil))

(defun appkit-chat-history-newer-stalled-p ()
  "Return non-nil when newer paging stalled at the current exact edge."
  (let ((state (appkit-chat-history-init-state)))
    (and (appkit-chat-history-window-partial-p)
         (equal (appkit-chat-history--state-stalled-newer-key state)
                (appkit-chat-history--state-last-key state))
         t)))

(defun appkit-chat-history-request-start (surface kind)
  "Start SURFACE's keyed history request of opaque KIND."
  (unless (appkit-surface-live-p surface)
    (error "Appkit chat history request requires a live Surface"))
  (unless kind
    (error "Appkit chat history request kind must be non-nil"))
  (with-current-buffer (appkit-surface-buffer surface)
    (appkit-chat-history-request-cancel)
    (let ((operation
           (appkit-chat-history-operation--create
            :surface surface :alive-p t))
          (state (appkit-chat-history-init-state)))
      (setf (appkit-chat-history--state-loading state) kind
            (appkit-chat-history--state-owner state) operation)
      operation)))


(defun appkit-chat-history-request-bind-handle (owner handle)
  "Bind transport HANDLE to current history OWNER."
  (when (appkit-handle-p handle)
    (if (appkit-chat-history-request-current-p owner)
        (setf (appkit-chat-history-operation-handle owner) handle)
      (when (appkit-handle-alive-p handle)
        (appkit-cancel-handle handle))))
  handle)

(defun appkit-chat-history-request-current-p (owner)
  "Return non-nil when OWNER is the current history request fence."
  (and (appkit-chat-history-operation-p owner)
       (appkit-chat-history-operation-alive-p owner)
       (appkit-surface-live-p
        (appkit-chat-history-operation-surface owner))
       (eq owner
           (appkit-chat-history--state-owner
            (appkit-chat-history-init-state)))))

(defun appkit-chat-history-request-end (owner)
  "Finish current OWNER and return non-nil when settlement won."
  (when (appkit-chat-history-request-current-p owner)
    (setf (appkit-chat-history-operation-alive-p owner) nil
          (appkit-chat-history-operation-handle owner) nil)
    (let ((state (appkit-chat-history-init-state)))
      (setf (appkit-chat-history--state-loading state) nil
            (appkit-chat-history--state-owner state) nil))
    t))

(defun appkit-chat-history-request-cancel ()
  "Cancel and clear the active history request fence."
  (let* ((state (appkit-chat-history-init-state))
         (owner (appkit-chat-history--state-owner state)))
    (setf (appkit-chat-history--state-loading state) nil
          (appkit-chat-history--state-owner state) nil)
    (when (appkit-chat-history-operation-p owner)
      (setf (appkit-chat-history-operation-alive-p owner) nil)
      (when-let* ((handle (appkit-chat-history-operation-handle owner))
                  ((appkit-handle-alive-p handle)))
        (appkit-cancel-handle handle))
      (setf (appkit-chat-history-operation-handle owner) nil))
    owner))

(defun appkit-chat-history--slice-result (valid-p entries reason)
  "Return a strict slice result from VALID-P, ENTRIES, and REASON."
  (list :valid-p (and valid-p t)
        :entries entries
        :reason reason))

(defun appkit-chat-history-window-slice (entries key-function)
  "Strictly slice ordered ENTRIES to the current exact history window.

KEY-FUNCTION returns each entry's stable opaque key.  The return value is a
plist with `:valid-p', `:entries', and `:reason'.  A known unbounded window may
legitimately produce `(:valid-p t :entries nil ...)', while an unknown window
or a missing exact edge is explicitly invalid.

Possible invalid reasons are `unknown-window', `missing-first-key',
`missing-last-key', and `reversed-edges'.  Clients must call this before
filtering hidden protocol entries; otherwise a hidden exact boundary would be
mistaken for a discontinuity."
  (unless (listp entries)
    (error "Appkit chat history entries must be a list: %S" entries))
  (unless (functionp key-function)
    (error "Appkit chat history key function is invalid: %S" key-function))
  (if (not (appkit-chat-history-window-known-p))
      (appkit-chat-history--slice-result nil nil 'unknown-window)
    (if (appkit-chat-history-window-empty-p)
        (appkit-chat-history--slice-result t nil nil)
      (let* ((entries (copy-sequence entries))
             (keys (mapcar key-function entries))
             (first-key (appkit-chat-history-window-first-key))
             (last-key (appkit-chat-history-window-last-key))
             (first-index (and first-key
                               (seq-position keys first-key #'equal))))
        (cond
         ((and first-key (null first-index))
          (appkit-chat-history--slice-result nil nil 'missing-first-key))
         (t
          (let* ((start-index (or first-index 0))
                 (window-entries (nthcdr start-index entries))
                 (window-keys (nthcdr start-index keys))
                 (last-index (and last-key
                                  (seq-position window-keys last-key #'equal))))
            (cond
             ((null last-key)
              (appkit-chat-history--slice-result t window-entries nil))
             (last-index
              (appkit-chat-history--slice-result
               t (seq-take window-entries (1+ last-index)) nil))
             ((seq-position keys last-key #'equal)
              (appkit-chat-history--slice-result nil nil 'reversed-edges))
             (t
              (appkit-chat-history--slice-result
               nil nil 'missing-last-key))))))))))

(defun appkit-chat-history-autoload-older-p (position start threshold)
  "Return non-nil when POSITION should trigger older paging near START.

THRESHOLD is a character distance; nil disables automatic paging.  The gate
also requires a known window, no active request, and an older edge that is not
known to be exhausted."
  (and (appkit-scroll-near-start-p position start threshold)
       (appkit-chat-history-window-known-p)
       (not (appkit-chat-history-loading-p))
       (not (appkit-chat-history-older-loaded-p))))

(cl-defun appkit-chat-history-autoload-newer-p
    (position end threshold &optional (allowed-p t))
  "Return non-nil when POSITION should trigger newer paging near END.

THRESHOLD is a character distance; nil disables automatic paging.  ALLOWED-P
lets a client suppress paging while its composer or another application-owned
interaction is active.  The gate also requires a partial known window, no
active request, and no recorded stall at its current exact newer edge."
  (and allowed-p
       (appkit-scroll-near-end-p position end threshold)
       (appkit-chat-history-window-partial-p)
       (not (appkit-chat-history-loading-p))
       (not (appkit-chat-history-newer-stalled-p))))

(cl-defun appkit-chat-history-delimiter-string
    (width &key loading-text
           (complete-character ?─)
           (partial-character ?·)
           (face 'shadow))
  "Return a passive WIDTH-column delimiter for the current history window.

WIDTH is the exact number of display columns in the result.
COMPLETE-CHARACTER fills a known window attached to latest;
PARTIAL-CHARACTER fills an unknown or partial window.  While a request is
active, non-empty LOADING-TEXT is centered in that rule.  FACE is applied to
the complete result when non-nil."
  (unless (and (integerp width) (>= width 0))
    (error "Appkit chat history delimiter width is invalid: %S" width))
  (unless (characterp complete-character)
    (error "Appkit complete delimiter must be a character: %S"
           complete-character))
  (unless (characterp partial-character)
    (error "Appkit partial delimiter must be a character: %S"
           partial-character))
  (unless (or (null loading-text) (stringp loading-text))
    (error "Appkit chat history loading text must be a string: %S"
           loading-text))
  (let* ((fill-character
          (if (and (appkit-chat-history-window-known-p)
                   (not (appkit-chat-history-window-partial-p)))
              complete-character
            partial-character))
         (label
          (and (appkit-chat-history-loading-p)
               (not (string-empty-p (or loading-text "")))
               (truncate-string-to-width
                (concat " " loading-text " ") width nil nil "…")))
         (label-width (if label (string-width label) 0))
         (remaining (max 0 (- width label-width)))
         (left (/ remaining 2))
         (right (- remaining left))
         (result
          (if label
              (concat (make-string left fill-character)
                      label
                      (make-string right fill-character))
            (make-string width fill-character))))
    (if face
        (propertize result 'face face)
      result)))

(provide 'appkit-chat-history)

;;; appkit-chat-history.el ends here
