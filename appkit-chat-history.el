;;; appkit-chat-history.el --- Continuous chat history windows  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-apps/appkit.el

;;; Commentary:

;; One continuous slice of a conversation, as an immutable value that lives in
;; a Surface model.  Transitions return new windows; nothing here owns
;; requests.  Clients own protocol cursors, remote frontiers and page rules;
;; this module owns exact edges, older exhaustion, newer stalls, strict
;; slicing, automatic paging gates and the passive edge delimiter.
;;
;; The `appkit-chat-history-' functions keep one window in a buffer-local
;; variable for clients that have not moved it into their model yet.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-scroll)

(cl-defstruct (appkit-chat-window
               (:constructor appkit-chat-window--create)
               (:copier appkit-chat-window--copy))
  "Immutable exact history window.
KNOWN-P is nil until a page or live entry establishes edges.  A nil
FIRST-KEY leaves the older edge unbounded; a nil LAST-KEY attaches the window
to the live edge.  EMPTY-P marks an authoritative empty conversation."
  known-p empty-p older-loaded-p first-key last-key stalled-newer-key)

(defconst appkit-chat-window-unknown (appkit-chat-window--create)
  "The window before anything is known.")

(defconst appkit-chat-window-empty
  (appkit-chat-window--create :known-p t :empty-p t :older-loaded-p t)
  "An authoritative empty window attached to the live edge.
Strict slicing returns no entries even if a cache holds unrelated islands.")

(defun appkit-chat-window-partial-p (window)
  "Return non-nil when newer entries lie outside known WINDOW."
  (and (appkit-chat-window-known-p window)
       (appkit-chat-window-last-key window)
       t))

(defun appkit-chat-window-set (window first-key last-key)
  "Return WINDOW with exact edges FIRST-KEY and LAST-KEY.
Keys are opaque and compared with `equal'.  Moving the newer edge clears a
stall recorded at the old one."
  (let ((next (appkit-chat-window--copy window)))
    (setf (appkit-chat-window-known-p next) t
          (appkit-chat-window-empty-p next) nil
          (appkit-chat-window-first-key next) first-key
          (appkit-chat-window-last-key next) last-key)
    (unless (equal last-key (appkit-chat-window-last-key window))
      (setf (appkit-chat-window-stalled-newer-key next) nil))
    next))

(defun appkit-chat-window-seed-live (window key)
  "Return empty WINDOW seeded with its first live entry KEY.
Any other WINDOW is returned unchanged.  The seeded window stays attached to
the live edge and keeps the fact that nothing older exists."
  (if (appkit-chat-window-empty-p window)
      (appkit-chat-window--create :known-p t :older-loaded-p t :first-key key)
    window))

(defun appkit-chat-window-with-older-loaded (window value)
  "Return WINDOW recording whether its oldest edge was reached as VALUE."
  (let ((next (appkit-chat-window--copy window)))
    (setf (appkit-chat-window-older-loaded-p next) (and value t))
    next))

(defun appkit-chat-window-stall-newer (window &optional key)
  "Return WINDOW recording that newer paging made no progress at KEY.
KEY defaults to the newer edge.  Only automatic paging at that edge stops."
  (let ((next (appkit-chat-window--copy window)))
    (setf (appkit-chat-window-stalled-newer-key next)
          (or key (appkit-chat-window-last-key window)))
    next))

(defun appkit-chat-window-unstall (window)
  "Return WINDOW without a recorded newer-paging stall."
  (let ((next (appkit-chat-window--copy window)))
    (setf (appkit-chat-window-stalled-newer-key next) nil)
    next))

(defun appkit-chat-window-newer-stalled-p (window)
  "Return non-nil when newer paging stalled at partial WINDOW's edge."
  (and (appkit-chat-window-partial-p window)
       (equal (appkit-chat-window-stalled-newer-key window)
              (appkit-chat-window-last-key window))
       t))

(defun appkit-chat-window-rekey (window old-key new-key)
  "Return WINDOW with edge OLD-KEY renamed to NEW-KEY."
  (let ((first (appkit-chat-window-first-key window))
        (last (appkit-chat-window-last-key window)))
    (if (and (appkit-chat-window-known-p window)
             (or (equal first old-key) (equal last old-key)))
        (appkit-chat-window-set window
                                (if (equal first old-key) new-key first)
                                (if (equal last old-key) new-key last))
      window)))

(defun appkit-chat-window--slice-result (valid-p entries reason)
  "Return a strict slice result from VALID-P, ENTRIES and REASON."
  (list :valid-p (and valid-p t) :entries entries :reason reason))

(defun appkit-chat-window-slice (window entries key-function)
  "Strictly slice ordered ENTRIES to WINDOW.

KEY-FUNCTION returns each entry's opaque key.  Return a plist with
`:valid-p', `:entries' and `:reason'.  An unknown window or a missing edge is
invalid, with reason `unknown-window', `missing-first-key',
`missing-last-key' or `reversed-edges'; never guess a discontinuity.  Slice
before filtering hidden entries, or a hidden edge looks missing."
  (cond
   ((not (appkit-chat-window-known-p window))
    (appkit-chat-window--slice-result nil nil 'unknown-window))
   ((appkit-chat-window-empty-p window)
    (appkit-chat-window--slice-result t nil nil))
   (t
    (let* ((keys (mapcar key-function entries))
           (first-key (appkit-chat-window-first-key window))
           (last-key (appkit-chat-window-last-key window))
           (first-index (and first-key (seq-position keys first-key #'equal))))
      (if (and first-key (null first-index))
          (appkit-chat-window--slice-result nil nil 'missing-first-key)
        (let* ((start (or first-index 0))
               (window-entries (nthcdr start entries))
               (last-index (and last-key
                                (seq-position (nthcdr start keys) last-key #'equal))))
          (cond
           ((null last-key)
            (appkit-chat-window--slice-result t (copy-sequence window-entries) nil))
           (last-index
            (appkit-chat-window--slice-result
             t (seq-take window-entries (1+ last-index)) nil))
           ((seq-position keys last-key #'equal)
            (appkit-chat-window--slice-result nil nil 'reversed-edges))
           (t (appkit-chat-window--slice-result nil nil 'missing-last-key)))))))))

(defun appkit-chat-window-autoload-older-p (window loading position start threshold)
  "Return non-nil when POSITION near START should page WINDOW older.
LOADING is non-nil while a request is active.  THRESHOLD is a character
distance; nil disables automatic paging."
  (and (not loading)
       (appkit-chat-window-known-p window)
       (not (appkit-chat-window-older-loaded-p window))
       (appkit-scroll-near-start-p position start threshold)))

(cl-defun appkit-chat-window-autoload-newer-p
    (window loading position end threshold &optional (allowed-p t))
  "Return non-nil when POSITION near END should page partial WINDOW newer.
LOADING is non-nil while a request is active.  ALLOWED-P lets a client hold
paging while its composer is in use."
  (and allowed-p
       (not loading)
       (appkit-chat-window-partial-p window)
       (not (appkit-chat-window-newer-stalled-p window))
       (appkit-scroll-near-end-p position end threshold)))

(cl-defun appkit-chat-window-delimiter
    (window loading width &key loading-text (complete-character ?─)
            (partial-character ?·) (face 'shadow))
  "Return a passive WIDTH-column delimiter for WINDOW.
COMPLETE-CHARACTER fills a known window attached to the live edge,
PARTIAL-CHARACTER anything else.  While LOADING, a non-empty LOADING-TEXT is
centered.  FACE applies to the whole result when non-nil."
  (let* ((fill (if (and (appkit-chat-window-known-p window)
                        (not (appkit-chat-window-partial-p window)))
                   complete-character
                 partial-character))
         (label (and loading
                     (not (string-empty-p (or loading-text "")))
                     (truncate-string-to-width
                      (concat " " loading-text " ") width nil nil "…")))
         (remaining (max 0 (- width (if label (string-width label) 0))))
         (left (/ remaining 2))
         (result (if label
                     (concat (make-string left fill) label
                             (make-string (- remaining left) fill))
                   (make-string width fill))))
    (if face (propertize result 'face face) result)))

;;; Buffer-local window

(defconst appkit-chat-history-effect-key 'appkit-chat-history
  "Surface Effect key for the single continuous history request.")

(defvar-local appkit-chat-history--window appkit-chat-window-unknown
  "Current buffer's history window.")

(defvar-local appkit-chat-history--loading nil
  "Opaque kind of the current buffer's active history request, or nil.")

(defun appkit-chat-history-reset-state ()
  "Reset the current buffer to an unknown window with no request."
  (setq-local appkit-chat-history--window appkit-chat-window-unknown
              appkit-chat-history--loading nil))

(defun appkit-chat-history-window-known-p ()
  "Return non-nil when the current buffer has an exact window."
  (appkit-chat-window-known-p appkit-chat-history--window))

(defun appkit-chat-history-window-partial-p ()
  "Return non-nil when newer entries lie outside the current window."
  (appkit-chat-window-partial-p appkit-chat-history--window))

(defun appkit-chat-history-window-empty-p ()
  "Return non-nil when the current window is authoritatively empty."
  (appkit-chat-window-empty-p appkit-chat-history--window))

(defun appkit-chat-history-window-first-key ()
  "Return the current window's older edge key."
  (appkit-chat-window-first-key appkit-chat-history--window))

(defun appkit-chat-history-window-last-key ()
  "Return the current window's newer edge key, or nil at the live edge."
  (appkit-chat-window-last-key appkit-chat-history--window))

(defun appkit-chat-history-older-loaded-p ()
  "Return non-nil when the current window reached its oldest edge."
  (appkit-chat-window-older-loaded-p appkit-chat-history--window))

(defun appkit-chat-history-loading ()
  "Return the kind of the active history request, or nil."
  appkit-chat-history--loading)

(defun appkit-chat-history-loading-p ()
  "Return non-nil while a history request is active."
  (and appkit-chat-history--loading t))

(defun appkit-chat-history-loading-set (kind)
  "Set the active request KIND, or nil when settled."
  (setq-local appkit-chat-history--loading kind))

(defun appkit-chat-history-window-set (first-key last-key)
  "Set the current window's exact edges to FIRST-KEY and LAST-KEY."
  (setq-local appkit-chat-history--window
              (appkit-chat-window-set appkit-chat-history--window first-key last-key)))

(defun appkit-chat-history-window-establish-empty ()
  "Make the current window authoritatively empty."
  (setq-local appkit-chat-history--window appkit-chat-window-empty))

(defun appkit-chat-history-window-seed-live (key)
  "Seed an empty current window with live KEY; return nil if not empty."
  (when (appkit-chat-window-empty-p appkit-chat-history--window)
    (setq-local appkit-chat-history--window
                (appkit-chat-window-seed-live appkit-chat-history--window key))))

(defun appkit-chat-history-window-clear ()
  "Make the current window unknown, keeping any active request."
  (setq-local appkit-chat-history--window appkit-chat-window-unknown))

(defun appkit-chat-history-older-loaded-set (value)
  "Record whether the current window reached its oldest edge as VALUE."
  (setq-local appkit-chat-history--window
              (appkit-chat-window-with-older-loaded appkit-chat-history--window value)))

(defun appkit-chat-history-newer-stalled-set (&optional key)
  "Record a newer-paging stall at KEY, defaulting to the newer edge."
  (setq-local appkit-chat-history--window
              (appkit-chat-window-stall-newer appkit-chat-history--window key))
  (appkit-chat-window-stalled-newer-key appkit-chat-history--window))

(defun appkit-chat-history-newer-stalled-clear ()
  "Clear the current window's newer-paging stall."
  (setq-local appkit-chat-history--window
              (appkit-chat-window-unstall appkit-chat-history--window)))

(defun appkit-chat-history-newer-stalled-p ()
  "Return non-nil when newer paging stalled at the current edge."
  (appkit-chat-window-newer-stalled-p appkit-chat-history--window))

(defun appkit-chat-history-window-slice (entries key-function)
  "Strictly slice ENTRIES to the current window; see `appkit-chat-window-slice'."
  (appkit-chat-window-slice appkit-chat-history--window entries key-function))

(defun appkit-chat-history-autoload-older-p (position start threshold)
  "Return non-nil when POSITION near START should page the window older."
  (appkit-chat-window-autoload-older-p
   appkit-chat-history--window appkit-chat-history--loading position start threshold))

(cl-defun appkit-chat-history-autoload-newer-p
    (position end threshold &optional (allowed-p t))
  "Return non-nil when POSITION near END should page the window newer."
  (appkit-chat-window-autoload-newer-p
   appkit-chat-history--window appkit-chat-history--loading
   position end threshold allowed-p))

(defun appkit-chat-history-delimiter-string (width &rest options)
  "Return the current window's WIDTH-column delimiter.
OPTIONS are those of `appkit-chat-window-delimiter'."
  (apply #'appkit-chat-window-delimiter
         appkit-chat-history--window appkit-chat-history--loading width options))

(provide 'appkit-chat-history)

;;; appkit-chat-history.el ends here
