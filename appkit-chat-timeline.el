;;; appkit-chat-timeline.el --- Shared projected chat timeline  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Persistent chat timeline controller shared by disco-room and other clients.
;; Clients project protocol messages into `appkit-chat-timeline-row' values.
;; Every state change uses one keyed reconciliation path: the projection may be
;; rebuilt in full, while EWOC only redraws rows whose payload, render context,
;; or dependency state changed.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'seq)
(require 'subr-x)
(require 'appkit-chatbuf)
(require 'appkit-projection)
(require 'appkit-position)
(require 'appkit-core)
(require 'appkit-surface)
(require 'appkit-scroll)

(cl-defstruct (appkit-chat-timeline-row
               (:include appkit-projection-row)
               (:constructor appkit-chat-timeline-row-create))
  "One protocol-independent projected chat timeline row.")

(cl-defstruct (appkit-chat-timeline--state
               (:constructor appkit-chat-timeline--state-create))
  projection
  after-mutation-function
  mutation-depth
  deferred-keys
  scroll-observer)

(defvar-local appkit-chat-timeline--state nil
  "Projected timeline state owned by the current Surface host.")

(defun appkit-chat-timeline--surface ()
  "Return the live Generated Surface owning the current timeline."
  (let ((surface (appkit-current-surface)))
    (or (and (appkit-surface-p surface) surface)
        (error "Appkit chat timeline requires a live Surface"))))

(defun appkit-chat-timeline--current-state ()
  "Return the current buffer's timeline state, or nil."
  (and (appkit-surface-p (appkit-current-surface))
       (appkit-chat-timeline--state-p appkit-chat-timeline--state)
       appkit-chat-timeline--state))

(defun appkit-chat-timeline-reset ()
  "Discard projected timeline state in the current Surface host."
  (appkit-chat-timeline--surface)
  (when-let* ((state (appkit-chat-timeline--current-state))
              (observer (appkit-chat-timeline--state-scroll-observer state)))
    (appkit-scroll-observer-cancel observer))
  (setq-local appkit-chat-timeline--state nil))

(defun appkit-chat-timeline--projection (&optional state)
  "Return the shared projection embedded in chat timeline STATE."
  (appkit-chat-timeline--state-projection
   (or state (appkit-chat-timeline--require-state))))

(defun appkit-chat-timeline-live-p ()
  "Return non-nil when the current buffer owns a live timeline."
  (when-let* ((state (appkit-chat-timeline--current-state)))
    (appkit-projection--engine-ewoc
     (appkit-chat-timeline--projection state))))

(defun appkit-chat-timeline-ewoc ()
  "Return the current shared EWOC, or nil before timeline initialization."
  (when-let* ((state (appkit-chat-timeline--current-state)))
    (appkit-projection--engine-ewoc
     (appkit-chat-timeline--projection state))))

(defun appkit-chat-timeline-keys ()
  "Return projected row keys in display order."
  (if-let* ((state (appkit-chat-timeline--current-state)))
      (appkit-projection--keys
       (appkit-chat-timeline--projection state))
    nil))

(defun appkit-chat-timeline-node (key)
  "Return current EWOC node identified by KEY, or nil."
  (when-let* ((state (appkit-chat-timeline--current-state)))
    (appkit-projection--node
     (appkit-chat-timeline--projection state) key)))

(defun appkit-chat-timeline-row (key)
  "Return current projected row identified by KEY, or nil."
  (when-let* ((state (appkit-chat-timeline--current-state)))
    (appkit-projection--row
     (appkit-chat-timeline--projection state) key)))

(defun appkit-chat-timeline-context (key)
  "Return render context belonging to projected row KEY."
  (when-let* ((row (appkit-chat-timeline-row key)))
    (appkit-chat-timeline-row-context row)))

(defun appkit-chat-timeline--require-state ()
  "Return current projected timeline state or signal an invariant error."
  (or (appkit-chat-timeline--current-state)
      (error "Appkit chat timeline has not been initialized")))

(cl-defun appkit-chat-timeline-ensure
    (&key printer anchor-property header footer after-mutation-function)
  "Ensure the current Surface host owns one projected timeline."
  (appkit-chat-timeline--surface)
  (let ((current (appkit-chat-timeline--current-state)))
    (if current
        (let ((projection (appkit-chat-timeline--projection current)))
          (when printer
            (setf (appkit-projection--engine-printer projection) printer))
          (when anchor-property
            (setf (appkit-projection--engine-anchor-property projection)
                  anchor-property))
          (setf (appkit-chat-timeline--state-after-mutation-function current)
                after-mutation-function))
      (unless (functionp printer)
        (error "Appkit chat timeline requires a row printer"))
      (let (projection)
        (appkit-chatbuf-with-generated-update
          (erase-buffer)
          (setq projection
                (appkit-projection-create
                 printer anchor-property
                 :header header :footer footer :no-separator-p t)))
        (setq-local
         appkit-chat-timeline--state
         (appkit-chat-timeline--state-create
          :projection projection
          :after-mutation-function after-mutation-function
          :mutation-depth 0 :deferred-keys nil))))
    (appkit-chat-timeline-ewoc)))

(defun appkit-chat-timeline-scroll-observer ()
  "Return the current timeline's scroll observer, or nil."
  (when-let* ((state (appkit-chat-timeline--current-state)))
    (appkit-chat-timeline--state-scroll-observer state)))

(cl-defun appkit-chat-timeline-install-history-observer
    (surface &key start-function end-function)
  "Install SURFACE's timeline history observer and return it."
  (unless (appkit-surface-live-p surface)
    (error "Cannot observe history for a dead Surface"))
  (with-current-buffer (appkit-surface-buffer surface)
    (unless (eq surface (appkit-current-surface))
      (error "Cannot observe history for a detached Surface"))
    (let* ((state (appkit-chat-timeline--require-state))
           (current (appkit-chat-timeline--state-scroll-observer state)))
      (when (appkit-scroll-observer-p current)
        (appkit-scroll-observer-cancel current))
      (let ((observer
             (appkit-scroll-observer-install
              surface
              :end-boundary-function
              #'appkit-chat-timeline-footer-start-position
              :start-function start-function
              :end-function end-function)))
        (setf (appkit-chat-timeline--state-scroll-observer state) observer)
        observer))))

(cl-defun appkit-chat-timeline-project
    (entries key-function &key context-function dependencies-function)
  "Project ENTRIES into protocol-independent timeline rows.

KEY-FUNCTION receives one entry.  CONTEXT-FUNCTION, when non-nil, receives the
previous entry and current entry.  DEPENDENCIES-FUNCTION receives the current
entry and returns opaque resource keys whose changes should redraw the row."
  (appkit-projection-project
   entries key-function
   :context-function context-function
   :dependencies-function dependencies-function
   :row-function #'appkit-chat-timeline-row-create))

(defun appkit-chat-timeline-dependent-keys (resource-keys)
  "Return current row keys depending on any of RESOURCE-KEYS."
  (appkit-projection--dependent-keys
   (appkit-chat-timeline--projection) resource-keys))

(defun appkit-chat-timeline--footer-region-bounds ()
  "Return current EWOC footer bounds before the prompt, or nil."
  (when-let* ((ewoc (appkit-chat-timeline-ewoc))
              (start (ewoc-location (ewoc--footer ewoc))))
    (let ((end (or (appkit-chatbuf-prompt-start-position)
                   (appkit-chatbuf-input-start-position)
                   (point-max))))
      (when (<= start end)
        (cons start end)))))

(defun appkit-chat-timeline-footer-start-position ()
  "Return the current EWOC footer start as an integer, or nil.

EWOC owns its boundary as a marker.  Do not leak that mutable representation
through this position API: history gates deliberately accept numeric geometry
only."
  (when-let* ((start (car-safe
                      (appkit-chat-timeline--footer-region-bounds))))
    (if (markerp start)
        (marker-position start)
      start)))

(defun appkit-chat-timeline-window-visible-end-position (window)
  "Return WINDOW's visible timeline end in the current buffer, or nil.

The window end may extend through the EWOC footer into the trailing composer.
Clamp it to the footer boundary so clients can use this value for history-edge
decisions without treating application-owned input as timeline content.

Unlike point, this value follows mouse-wheel, scroll-bar, and indirect-window
scrolling even when those operations leave the window point unchanged."
  (appkit-scroll-window-visible-end-position
   window (appkit-chat-timeline-footer-start-position)))

(defun appkit-chat-timeline--position-zone-state (position preserve-window-start)
  "Capture semantic state for POSITION in the current timeline.

PRESERVE-WINDOW-START is forwarded for message-zone snapshots."
  (let ((position (min (point-max) (max (point-min) position))))
    (cond
     ((appkit-chatbuf-point-in-input-p position)
      (list :zone 'input
            :offset (- position (appkit-chatbuf-input-start-position))))
     ((appkit-chatbuf-point-in-prompt-p position)
      (list :zone 'prompt
            :offset (- position (appkit-chatbuf-prompt-start-position))))
     ((when-let* ((bounds (appkit-chat-timeline--footer-region-bounds)))
        (and (<= (car bounds) position)
             (<= position (cdr bounds))))
      (list :zone 'footer
            :offset (- position
                       (car (appkit-chat-timeline--footer-region-bounds)))))
     (t
      (save-excursion
        (goto-char position)
        (list :zone 'message
              :snapshot
              (appkit-position-capture
               :anchor-property
               (appkit-projection--engine-anchor-property
                (appkit-chat-timeline--projection))
               :preserve-window-start preserve-window-start)))))))

(defun appkit-chat-timeline--restore-zone-state (position-state rekeys)
  "Restore POSITION-STATE, remapping semantic anchors through REKEYS."
  (pcase (plist-get position-state :zone)
    ('input
     (when-let* ((start (appkit-chatbuf-input-start-position))
                 (end (appkit-chatbuf-input-logical-end-position)))
       (goto-char (min end
                       (max start
                            (+ start (or (plist-get position-state :offset) 0)))))))
    ('prompt
     (when-let* ((start (appkit-chatbuf-prompt-start-position))
                 (end (appkit-chatbuf-input-start-position)))
       (goto-char (min (max start (1- end))
                       (+ start (or (plist-get position-state :offset) 0))))))
    ('footer
     (when-let* ((bounds (appkit-chat-timeline--footer-region-bounds)))
       (goto-char (min (cdr bounds)
                       (+ (car bounds)
                          (or (plist-get position-state :offset) 0))))))
    (_
     (when-let* ((snapshot (plist-get position-state :snapshot)))
       (appkit-position-restore snapshot rekeys))))
  (point))

(cl-defun appkit-chat-timeline-run-preserving-position (mutator &key rekeys)
  "Run MUTATOR as one undo-free chat timeline transaction.

Point, active mark, viewport, footer position, composer position, and window
points inside the composer are restored afterwards.  REKEYS is an alist mapping
old semantic row keys to new keys."
  (let ((state (appkit-chat-timeline--require-state)))
    (if (> (or (appkit-chat-timeline--state-mutation-depth state) 0) 0)
        (funcall mutator)
      (let* ((window-input-offsets
              (appkit-chatbuf-capture-window-input-offsets))
             (point-state
              (appkit-chat-timeline--position-zone-state (point) t))
             (mark-state
              (and mark-active
                   (appkit-chat-timeline--position-zone-state (mark t) nil))))
        (setf (appkit-chat-timeline--state-mutation-depth state) 1)
        (unwind-protect
            (appkit-chatbuf-with-generated-update
              (unwind-protect
                  (funcall mutator)
                (appkit-chatbuf-protect-generated-content)))
          (setf (appkit-chat-timeline--state-mutation-depth state) 0)
          (appkit-chat-timeline--restore-zone-state point-state rekeys)
          (appkit-chatbuf-restore-window-input-offsets window-input-offsets)
          (if mark-state
              (let ((mark-position
                     (save-excursion
                       (appkit-chat-timeline--restore-zone-state mark-state rekeys)
                       (point))))
                (set-marker (mark-marker) mark-position)
                (setq mark-active t
                      deactivate-mark nil))
            (setq mark-active nil
                  deactivate-mark t))
          (when-let* ((after-mutation
                       (appkit-chat-timeline--state-after-mutation-function state)))
            (funcall after-mutation)))))))

(defun appkit-chat-timeline--set-header (ewoc header)
  "Set EWOC HEADER without touching its footer or trailing composer."
  ;; `ewoc-set-hf' refreshes both sentinels.  A chat buffer keeps its prompt
  ;; and editable input after the footer, so refreshing the sentinels
  ;; separately is the only operation with the right ownership boundary.
  (ewoc--set-buffer-bind-dll-let* ewoc
                                  ((node (ewoc--header ewoc))
                                   (printer (ewoc--hf-pp ewoc)))
                                  (unless (equal (ewoc--node-data node) header)
                                    (setf (ewoc--node-data node) header)
                                    (ewoc--refresh-node printer node dll))))

(defun appkit-chat-timeline--set-footer (ewoc footer)
  "Set EWOC FOOTER without touching its trailing composer."
  (let* ((prompt-marker appkit-chatbuf--prompt-marker)
         (prompt-live-p
          (and (markerp prompt-marker)
               (eq (marker-buffer prompt-marker) (current-buffer))))
         (old-insertion-type
          (and prompt-live-p (marker-insertion-type prompt-marker))))
    ;; Refreshing a footer deletes its old text and inserts the replacement at
    ;; the same boundary.  Make the prompt marker advance over that insertion;
    ;; otherwise a growing footer leaves the marker inside EWOC-owned text.
    (unwind-protect
        (progn
          (when prompt-live-p
            (set-marker-insertion-type prompt-marker t))
          (ewoc--set-buffer-bind-dll-let* ewoc
                                          ((node (ewoc--footer ewoc))
                                           (printer (ewoc--hf-pp ewoc)))
                                          (unless (equal (ewoc--node-data node) footer)
                                            (setf (ewoc--node-data node) footer)
                                            (ewoc--refresh-node printer node dll))))
      (when prompt-live-p
        (set-marker-insertion-type prompt-marker old-insertion-type)))))

(cl-defun appkit-chat-timeline-set-frame
    (header footer &key bind-input-function
            (composer-visible-p nil composer-visible-p-supplied-p))
  "Set timeline HEADER and FOOTER without rebuilding live input.

The EWOC frame and the trailing composer are separate regions.  Header and
footer changes are applied to their sentinel nodes in place, like telega's
chat buffer, so message or metadata redisplay can never delete and recreate
the user's input.

When BIND-INPUT-FUNCTION is non-nil, call it only when the composer needs to
be created, removed, or when COMPOSER-VISIBLE-P was not supplied.  Supplying
COMPOSER-VISIBLE-P gives stable input visibility semantics."
  (let* ((state (appkit-chat-timeline--require-state))
         (ewoc (appkit-projection--engine-ewoc
                (appkit-chat-timeline--projection state)))
         (footer-start (ewoc-location (ewoc--footer ewoc)))
         (prompt-start (appkit-chatbuf-prompt-start-position))
         (input-start (appkit-chatbuf-input-start-position))
         (composer-present-p (or prompt-start input-start))
         (composer-bound-p
          (and (appkit-chatbuf-prompt-button-live-p)
               (<= footer-start prompt-start input-start)))
         (bind-composer-p
          (and (functionp bind-input-function)
               (or (not composer-visible-p-supplied-p)
                   (if composer-visible-p
                       (not composer-bound-p)
                     composer-present-p)))))
    (when (and composer-present-p (not composer-bound-p))
      (error "Appkit composer boundary is outside the timeline footer"))
    (appkit-chat-timeline-run-preserving-position
     (lambda ()
       (appkit-chat-timeline--set-header ewoc header)
       (appkit-chat-timeline--set-footer ewoc footer)
       (when bind-composer-p
         (funcall bind-input-function))))))

(cl-defun appkit-chat-timeline-sync
    (rows &key force-keys changed-resources rekeys)
  "Synchronize the live timeline with projected ROWS.

FORCE-KEYS redraws presentation-only changes.  CHANGED-RESOURCES redraws rows
whose dependency lists mention those opaque resource keys.  REKEYS maps old
row keys to newly projected keys while preserving node and cursor identity."
  (appkit-chat-timeline-run-preserving-position
   (lambda ()
     (appkit-projection--reconcile
      (appkit-chat-timeline--projection) rows
      :force-keys force-keys
      :changed-dependencies changed-resources
      :rekeys rekeys))
   :rekeys rekeys)
  (appkit-chat-timeline-keys))

(cl-defun appkit-chat-timeline-invalidate (keys &key defer-while-mark-active)
  "Redraw existing rows identified by KEYS.

When DEFER-WHILE-MARK-ACTIVE is non-nil, queue keys until
`appkit-chat-timeline-flush-deferred' is called with no active region."
  (let* ((state (appkit-chat-timeline--require-state))
         (keys (delete-dups (delq nil (copy-sequence keys)))))
    (cond
     ((null keys) nil)
     ((and defer-while-mark-active mark-active)
      (setf (appkit-chat-timeline--state-deferred-keys state)
            (delete-dups
             (append keys
                     (appkit-chat-timeline--state-deferred-keys state))))
      'deferred)
     (t
      (appkit-chat-timeline-run-preserving-position
       (lambda ()
         (appkit-projection--invalidate
          (appkit-chat-timeline--projection state) keys)))
      t))))

(defun appkit-chat-timeline-flush-deferred ()
  "Redraw deferred keys when no region is active."
  (let ((state (appkit-chat-timeline--require-state)))
    (when (and (not mark-active)
               (appkit-chat-timeline--state-deferred-keys state))
      (let ((keys (prog1 (appkit-chat-timeline--state-deferred-keys state)
                    (setf (appkit-chat-timeline--state-deferred-keys state) nil))))
        (appkit-chat-timeline-invalidate keys)))))

(defun appkit-chat-timeline-refresh ()
  "Refresh all projected rows while preserving chat position."
  (let ((state (appkit-chat-timeline--require-state)))
    (appkit-chat-timeline-run-preserving-position
     (lambda ()
       (ewoc-refresh
        (appkit-projection--engine-ewoc
         (appkit-chat-timeline--projection state)))))))

(defun appkit-chat-timeline-key-at-point (&optional position)
  "Return semantic timeline key at POSITION or point."
  (let* ((position (or position (point)))
         (property
          (appkit-projection--engine-anchor-property
           (appkit-chat-timeline--projection))))
    (and property
         (or (get-text-property position property)
             (save-excursion
               (goto-char position)
               (get-text-property (line-beginning-position) property))))))

(defun appkit-chat-timeline-key-position (key)
  "Return first buffer position carrying semantic row KEY, or nil."
  (let ((property
         (appkit-projection--engine-anchor-property
          (appkit-chat-timeline--projection))))
    (and property
         (appkit-position-find-property-value
          (point-min) (point-max) property key))))

(provide 'appkit-chat-timeline)

;;; appkit-chat-timeline.el ends here
