;;; appkit-loop.el --- UI-free serialized runtime loop  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; A main-thread, owner-independent message loop.  It provides an O(1)
;; bounded mailbox, frozen-cutoff passes, synchronous barrier tickets, and a
;; terminal fault boundary.  Application/Surface commands and presentation
;; phases build on this kernel; this module deliberately contains no buffer or
;; transport behavior.

;;; Code:

(require 'cl-lib)
(require 'appkit-cleanup)

(cl-defstruct (appkit-loop-accepted
               (:constructor appkit-loop-accept (model))
               (:copier nil))
  "A successful transition carrying its next MODEL."
  model)

(cl-defstruct (appkit-loop-rejected
               (:constructor appkit-loop-reject (reason))
               (:copier nil))
  "A normal domain rejection carrying REASON."
  reason)

(cl-defstruct (appkit-loop-ticket
               (:constructor appkit-loop--ticket-create)
               (:copier nil))
  "One synchronous send barrier."
  state
  outcome
  sequence
  revision)

(cl-defstruct (appkit-loop-envelope
               (:constructor appkit-loop--envelope-create)
               (:copier nil))
  sequence
  incarnation
  payload
  reply-route
  ticket)

(cl-defstruct (appkit-loop-fault
               (:constructor appkit-loop--fault-create)
               (:copier nil))
  condition
  sequence
  payload
  revision)

(cl-defstruct (appkit-loop-pass
               (:constructor appkit-loop--pass-create)
               (:copier nil))
  "Committed transition statistics for one completed input pass."
  processed
  accepted
  start-revision
  end-revision)

(cl-defstruct (appkit-loop
               (:constructor appkit-loop--create)
               (:copier nil)
               (:conc-name appkit-loop--))
  owner-identity
  update
  after-pass
  on-fault
  model
  incarnation
  revision
  data-head
  data-tail
  data-count
  data-capacity
  send-reserve
  message-limit
  next-sequence
  draining-p
  sync-driving-p
  scheduled-handle
  status
  pass-tickets
  fault)

(defvar appkit-loop--active-loop nil
  "Loop whose serialized pass currently owns runtime execution.")

(defun appkit-loop--check (loop)
  "Return LOOP, or signal a type error."
  (unless (appkit-loop-p loop)
    (signal 'wrong-type-argument (list 'appkit-loop-p loop)))
  loop)

(defun appkit-loop--assert-main-thread ()
  "Signal when the current thread is not Emacs's main thread."
  (unless (eq (current-thread) main-thread)
    (error "Appkit runtime mutation is restricted to the main Emacs thread")))

(defun appkit-loop-owner-identity (loop)
  "Return LOOP's opaque stable owner identity."
  (appkit-loop--owner-identity (appkit-loop--check loop)))

(defun appkit-loop-model (loop)
  "Return LOOP's current committed model."
  (appkit-loop--model (appkit-loop--check loop)))

(defun appkit-loop-incarnation (loop)
  "Return LOOP's current incarnation."
  (appkit-loop--incarnation (appkit-loop--check loop)))

(defun appkit-loop-revision (loop)
  "Return LOOP's current committed revision."
  (appkit-loop--revision (appkit-loop--check loop)))

(defun appkit-loop-status (loop)
  "Return LOOP's lifecycle status."
  (appkit-loop--status (appkit-loop--check loop)))

(defun appkit-loop-fault (loop)
  "Return LOOP's terminal fault record, or nil."
  (appkit-loop--fault (appkit-loop--check loop)))

(defun appkit-loop-pending-count (loop)
  "Return the number of queued envelopes in LOOP."
  (appkit-loop--data-count (appkit-loop--check loop)))

(cl-defun appkit-loop-create
    (&key (owner-identity (make-symbol "appkit-owner-"))
          model update after-pass on-fault (mailbox-capacity 64)
          (send-reserve 1) (message-limit 32))
  "Create a running UI-free loop with initial MODEL and UPDATE.

OWNER-IDENTITY is the stable opaque identity captured by exact runtime
addresses.  UPDATE receives the current model and one message.  It must return
either `appkit-loop-accept' or `appkit-loop-reject'.  AFTER-PASS, when non-nil,
receives the loop and committed `appkit-loop-pass' statistics after a pass
accepts at least one transition.  ON-FAULT receives the loop and fault record
after admission closes and pending tickets terminate; it may signal `error' or
`quit' but must not use `throw'.  MAILBOX-CAPACITY bounds ordinary posts.
SEND-RESERVE provides additional admission reserved for synchronous barrier
sends.  MESSAGE-LIMIT is the hard maximum processed by one pass."
  (appkit-loop--assert-main-thread)
  (unless (functionp update)
    (signal 'wrong-type-argument (list 'functionp update)))
  (dolist (callback (list after-pass on-fault))
    (unless (or (null callback) (functionp callback))
      (signal 'wrong-type-argument (list 'functionp callback))))
  (dolist (entry `((,mailbox-capacity . mailbox-capacity)
                   (,send-reserve . send-reserve)
                   (,message-limit . message-limit)))
    (unless (and (integerp (car entry)) (> (car entry) 0))
      (error "Appkit loop %s must be positive: %S"
             (cdr entry) (car entry))))
  (unless owner-identity
    (error "Appkit loop owner identity must be non-nil"))
  (appkit-loop--create
   :owner-identity owner-identity
   :update update
   :after-pass after-pass
   :on-fault on-fault
   :model model
   :incarnation 1
   :revision 0
   :data-head nil
   :data-tail nil
   :data-count 0
   :data-capacity mailbox-capacity
   :send-reserve send-reserve
   :message-limit message-limit
   :next-sequence 0
   :draining-p nil
   :sync-driving-p nil
   :scheduled-handle nil
   :status 'running
   :pass-tickets nil
   :fault nil))

(defun appkit-loop--cancel-scheduled (loop)
  "Cancel LOOP's pending scheduler handle, if any."
  (when-let* ((timer (appkit-loop--scheduled-handle loop)))
    (setf (appkit-loop--scheduled-handle loop) nil)
    (when (timerp timer)
      (cancel-timer timer))))

(defun appkit-loop--scheduled-pass (loop)
  "Run one scheduled pass for LOOP."
  (when (appkit-loop-p loop)
    (setf (appkit-loop--scheduled-handle loop) nil)
    (when (eq (appkit-loop--status loop) 'running)
      (appkit-loop-run-pass loop))))

(defun appkit-loop--schedule (loop)
  "Ensure LOOP has one scheduled pass when it has pending work."
  (when (and (eq (appkit-loop--status loop) 'running)
             (> (appkit-loop--data-count loop) 0)
             (not (appkit-loop--draining-p loop))
             (not (appkit-loop--sync-driving-p loop))
             (null (appkit-loop--scheduled-handle loop)))
    (setf (appkit-loop--scheduled-handle loop)
          (run-at-time 0 nil #'appkit-loop--scheduled-pass loop))))

(defun appkit-loop--enqueue
    (loop payload ticket limit &optional incarnation reply-route)
  "Append PAYLOAD and optional TICKET to LOOP below LIMIT.

INCARNATION defaults to LOOP's current incarnation.  REPLY-ROUTE is opaque
metadata retained for a directed reply."
  (if (>= (appkit-loop--data-count loop) limit)
      'full
    (let* ((sequence (appkit-loop--next-sequence loop))
           (envelope
            (appkit-loop--envelope-create
             :sequence sequence
             :incarnation (or incarnation (appkit-loop--incarnation loop))
             :payload payload
             :reply-route reply-route
             :ticket ticket))
           (cell (list envelope)))
      (if (appkit-loop--data-tail loop)
          (setcdr (appkit-loop--data-tail loop) cell)
        (setf (appkit-loop--data-head loop) cell))
      (setf (appkit-loop--data-tail loop) cell
            (appkit-loop--data-count loop)
            (1+ (appkit-loop--data-count loop))
            (appkit-loop--next-sequence loop)
            (1+ sequence))
      (when ticket
        (setf (appkit-loop-ticket-sequence ticket) sequence))
      (appkit-loop--schedule loop)
      'enqueued)))

(defun appkit-loop--admission-status (loop)
  "Return LOOP's non-running admission outcome, or nil."
  (pcase (appkit-loop--status loop)
    ('faulted 'faulted)
    ((or 'stopping 'stopped) 'stopped)
    (_ nil)))

(defun appkit-loop--post-addressed
    (loop message incarnation &optional reply-route)
  "Post MESSAGE to LOOP only at exact INCARNATION.

REPLY-ROUTE, when non-nil, is retained as opaque envelope metadata."
  (appkit-loop--assert-main-thread)
  (appkit-loop--check loop)
  (if (/= incarnation (appkit-loop--incarnation loop))
      'stale
    (or (appkit-loop--admission-status loop)
        (appkit-loop--enqueue
         loop message nil (appkit-loop--data-capacity loop)
         incarnation reply-route))))

(defun appkit-loop-post (loop message)
  "Try to enqueue MESSAGE in LOOP and return its admission outcome.

The result is one of `enqueued', `full', `stopped', or `faulted'.  A failed
admission does not allocate a sequence number."
  (appkit-loop--post-addressed
   loop message (appkit-loop-incarnation loop)))

(defun appkit-loop--dequeue (loop)
  "Remove and return LOOP's first envelope."
  (when-let* ((cell (appkit-loop--data-head loop)))
    (setf (appkit-loop--data-head loop) (cdr cell)
          (appkit-loop--data-count loop)
          (1- (appkit-loop--data-count loop)))
    (unless (appkit-loop--data-head loop)
      (setf (appkit-loop--data-tail loop) nil))
    (car cell)))

(defun appkit-loop--complete-ticket (ticket state outcome &optional revision)
  "Complete pending TICKET once with STATE, OUTCOME, and REVISION."
  (when (and ticket (eq (appkit-loop-ticket-state ticket) 'pending))
    (setf (appkit-loop-ticket-state ticket) state
          (appkit-loop-ticket-outcome ticket) outcome
          (appkit-loop-ticket-revision ticket) revision)
    t))

(defun appkit-loop--purge (loop ticket-state outcome)
  "Clear LOOP's mailbox, completing tickets with TICKET-STATE and OUTCOME."
  (let ((cell (appkit-loop--data-head loop)))
    (setf (appkit-loop--data-head loop) nil
          (appkit-loop--data-tail loop) nil
          (appkit-loop--data-count loop) 0)
    (while cell
      (appkit-loop--complete-ticket
       (appkit-loop-envelope-ticket (car cell)) ticket-state outcome)
      (setq cell (cdr cell)))))

(defun appkit-loop--complete-pass-tickets (loop state outcome)
  "Complete LOOP's accepted pass tickets with STATE and OUTCOME."
  (let ((entries (nreverse (appkit-loop--pass-tickets loop))))
    (setf (appkit-loop--pass-tickets loop) nil)
    (dolist (entry entries)
      (appkit-loop--complete-ticket
       (car entry) state outcome
       (and (eq state 'accepted) (cdr entry))))))

(defun appkit-loop--enter-fault (loop condition envelope)
  "Fault LOOP for CONDITION while processing ENVELOPE."
  (unless (memq (appkit-loop--status loop) '(faulted stopping stopped))
    (let ((fault
           (appkit-loop--fault-create
            :condition condition
            :sequence (and envelope
                           (appkit-loop-envelope-sequence envelope))
            :payload (and envelope (appkit-loop-envelope-payload envelope))
            :revision (appkit-loop--revision loop))))
      (setf (appkit-loop--status loop) 'faulted
            (appkit-loop--fault loop) fault)
      (appkit-loop--cancel-scheduled loop)
      (when envelope
        (appkit-loop--complete-ticket
         (appkit-loop-envelope-ticket envelope) 'faulted fault))
      (appkit-loop--complete-pass-tickets loop 'faulted fault)
      (appkit-loop--purge loop 'faulted fault)
      (when-let* ((on-fault (appkit-loop--on-fault loop)))
        (condition-case cleanup-condition
            (funcall on-fault loop fault)
          ((error quit)
           (appkit--warn-cleanup-conditions
            (list cleanup-condition) 'appkit-loop))))))
  (appkit-loop--fault loop))

(defun appkit-loop--apply-transition (loop envelope)
  "Apply ENVELOPE to LOOP, containing unexpected exits as faults."
  (if (/= (appkit-loop-envelope-incarnation envelope)
          (appkit-loop--incarnation loop))
      (appkit-loop--complete-ticket
       (appkit-loop-envelope-ticket envelope) 'superseded nil)
    (let (completed-p condition result)
      (unwind-protect
          (condition-case err
              (progn
                (setq result
                      (funcall (appkit-loop--update loop)
                               (appkit-loop--model loop)
                               (appkit-loop-envelope-payload envelope))
                      completed-p t))
            ((error quit)
             (setq condition err
                   completed-p t)))
        (unless completed-p
          (appkit-loop--enter-fault
           loop '(error "Appkit loop update exited nonlocally") envelope)))
      (cond
       (condition
        (appkit-loop--enter-fault loop condition envelope))
       ((not (eq (appkit-loop--status loop) 'running)) nil)
       ((appkit-loop-accepted-p result)
        (let ((revision (1+ (appkit-loop--revision loop)))
              (ticket (appkit-loop-envelope-ticket envelope)))
          (setf (appkit-loop--model loop)
                (appkit-loop-accepted-model result)
                (appkit-loop--revision loop) revision)
          (when ticket
            (push (cons ticket revision) (appkit-loop--pass-tickets loop)))
          'accepted))
       ((appkit-loop-rejected-p result)
        (appkit-loop--complete-ticket
         (appkit-loop-envelope-ticket envelope)
         'rejected (appkit-loop-rejected-reason result))
        'rejected)
       (t
        (appkit-loop--enter-fault
         loop
         (list 'error
               (format "Appkit loop update returned invalid result: %S"
                       result))
         envelope)
        'faulted)))))

(defun appkit-loop--finish-pass (loop pass)
  "Run LOOP's post-commit phase for PASS and complete accepted tickets."
  (let (completed-p condition)
    (unwind-protect
        (condition-case err
            (progn
              (when-let* ((after-pass (appkit-loop--after-pass loop)))
                (funcall after-pass loop pass))
              (setq completed-p t))
          ((error quit)
           (setq condition err
                 completed-p t)))
      (unless completed-p
        (appkit-loop--enter-fault
         loop '(error "Appkit loop after-pass exited nonlocally") nil)))
    (cond
     (condition
      (appkit-loop--enter-fault loop condition nil))
     ((eq (appkit-loop--status loop) 'running)
      (appkit-loop--complete-pass-tickets loop 'accepted nil)))))

(defun appkit-loop-run-pass (loop)
  "Run at most one frozen, bounded mailbox pass for LOOP.

Return the number of envelopes removed.  Messages enqueued during this pass
remain for a later pass even when the count limit has not been reached."
  (appkit-loop--assert-main-thread)
  (appkit-loop--check loop)
  (when (or appkit-loop--active-loop (appkit-loop--draining-p loop))
    (error "Appkit loop pass is not reentrant"))
  (let ((appkit-loop--active-loop loop))
    (if (not (eq (appkit-loop--status loop) 'running))
        0
      (appkit-loop--cancel-scheduled loop)
      (let ((cutoff
             (when-let* ((tail (appkit-loop--data-tail loop)))
               (appkit-loop-envelope-sequence (car tail))))
            (limit (appkit-loop--message-limit loop))
            (start-revision (appkit-loop--revision loop))
            (processed 0)
            (accepted 0))
        (setf (appkit-loop--draining-p loop) t)
        (setf (appkit-loop--pass-tickets loop) nil)
        (unwind-protect
            (progn
              (while (and cutoff
                          (eq (appkit-loop--status loop) 'running)
                          (< processed limit)
                          (appkit-loop--data-head loop)
                          (<= (appkit-loop-envelope-sequence
                               (car (appkit-loop--data-head loop)))
                              cutoff))
                (let* ((envelope (appkit-loop--dequeue loop))
                       (outcome
                        (appkit-loop--apply-transition loop envelope)))
                  (setq processed (1+ processed))
                  (when (eq outcome 'accepted)
                    (setq accepted (1+ accepted)))))
              (when (and (> accepted 0)
                         (eq (appkit-loop--status loop) 'running))
                (appkit-loop--finish-pass
                 loop
                 (appkit-loop--pass-create
                  :processed processed
                  :accepted accepted
                  :start-revision start-revision
                  :end-revision (appkit-loop--revision loop)))))
          (setf (appkit-loop--draining-p loop) nil)
          (appkit-loop--schedule loop))
        processed))))

(defun appkit-loop--resignal-fault (loop)
  "Re-signal LOOP's stored fault condition."
  (let ((condition
         (appkit-loop-fault-condition (appkit-loop--fault loop))))
    (signal (car condition) (cdr condition))))

(defun appkit-loop-send (loop message)
  "Synchronously send MESSAGE through LOOP and return its terminal ticket.

A send uses reserved capacity and drives as many bounded passes as necessary
without crossing messages queued after its ticket.  Return `reentrant-send'
when called from a pass, `busy' when all send reserve is occupied, and
`stopped' or `faulted' when LOOP cannot admit work.  An admitted send that
encounters a runtime fault re-signals the stored condition."
  (appkit-loop--assert-main-thread)
  (appkit-loop--check loop)
  (cond
   ((or appkit-loop--active-loop (appkit-loop--draining-p loop))
    'reentrant-send)
   ((appkit-loop--admission-status loop))
   ((>= (appkit-loop--data-count loop)
        (+ (appkit-loop--data-capacity loop)
           (appkit-loop--send-reserve loop)))
    'busy)
   (t
    (let ((ticket (appkit-loop--ticket-create :state 'pending)))
      (appkit-loop--cancel-scheduled loop)
      (setf (appkit-loop--sync-driving-p loop) t)
      (unwind-protect
          (progn
            (appkit-loop--enqueue
             loop message ticket
             (+ (appkit-loop--data-capacity loop)
                (appkit-loop--send-reserve loop)))
            (while (and (eq (appkit-loop-ticket-state ticket) 'pending)
                        (eq (appkit-loop--status loop) 'running))
              (appkit-loop-run-pass loop)))
        (setf (appkit-loop--sync-driving-p loop) nil)
        (appkit-loop--schedule loop))
      (if (eq (appkit-loop-ticket-state ticket) 'faulted)
          (appkit-loop--resignal-fault loop)
        ticket)))))

(defun appkit-loop-stop (loop)
  "Stop LOOP, discard pending messages, and terminate pending tickets.

Return non-nil when this call performs the transition.  Repeated calls are
inert."
  (appkit-loop--assert-main-thread)
  (appkit-loop--check loop)
  (when (or appkit-loop--active-loop (appkit-loop--draining-p loop))
    (error "Cannot stop an Appkit loop from an active pass"))
  (unless (eq (appkit-loop--status loop) 'stopped)
    (setf (appkit-loop--status loop) 'stopping
          (appkit-loop--incarnation loop)
          (1+ (appkit-loop--incarnation loop)))
    (appkit-loop--cancel-scheduled loop)
    (appkit-loop--purge loop 'stopped nil)
    (setf (appkit-loop--status loop) 'stopped)
    t))

(provide 'appkit-loop)

;;; appkit-loop.el ends here
