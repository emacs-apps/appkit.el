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
  origin
  source-address
  source-revision
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

(cl-defstruct (appkit-loop-lane
               (:constructor nil)
               (:constructor appkit-loop--lane-create (capacity))
               (:copier nil)
               (:conc-name appkit-loop--lane-))
  "One bounded FIFO lane owned by an Appkit loop."
  head
  tail
  (count 0)
  capacity)

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
  data-lane
  control-lane
  send-reserve
  message-limit
  next-sequence
  scheduled-handle
  status
  fault)

(defvar appkit-loop--active-loop nil
  "Loop whose serialized pass currently owns runtime execution.")

(defvar appkit-loop--current-envelope nil
  "Envelope currently executing in the active serialized loop.")

(defvar appkit-loop--pass-context nil
  "Facade-owned snapshot shared by callbacks in the active pass.")

(defvar appkit-loop--send-context nil
  "Cons of the loop and ticket owned by the current synchronous send.")

(defvar appkit-loop--pass-tickets nil
  "Accepted tickets awaiting completion in the current pass.")

(defun appkit-loop--check (loop)
  "Return LOOP, or signal a type error."
  (unless (appkit-loop-p loop)
    (signal 'wrong-type-argument (list 'appkit-loop-p loop)))
  loop)

(defun appkit-loop--assert-main-thread ()
  "Signal when the current thread is not Emacs's main thread."
  (unless (eq (current-thread) main-thread)
    (error "Appkit runtime mutation is restricted to the main Emacs thread")))

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
  "Return the total number of queued data and control envelopes in LOOP."
  (setq loop (appkit-loop--check loop))
  (+ (appkit-loop--lane-count (appkit-loop--control-lane loop))
     (appkit-loop--lane-count (appkit-loop--data-lane loop))))

(cl-defun appkit-loop-create
    (&key (owner-identity (make-symbol "appkit-owner-"))
          model update after-pass on-fault (mailbox-capacity 64)
          (control-capacity 16) (send-reserve 1) (message-limit 32))
  "Create a running UI-free loop with initial MODEL and UPDATE.

OWNER-IDENTITY is the stable opaque identity captured by exact runtime
addresses.  UPDATE receives the current model and one message.  It must return
either `appkit-loop-accept' or `appkit-loop-reject'.  AFTER-PASS, when non-nil,
receives the loop and committed `appkit-loop-pass' statistics after a pass
accepts at least one transition.  ON-FAULT receives the loop and fault record
after admission closes and pending tickets terminate; it may signal `error' or
`quit' but must not use `throw'.  MAILBOX-CAPACITY bounds ordinary posts.
CONTROL-CAPACITY independently bounds required runtime work.  SEND-RESERVE
provides additional data admission reserved for synchronous barrier sends.
MESSAGE-LIMIT is the hard maximum processed across both lanes by one pass."
  (appkit-loop--assert-main-thread)
  (unless (functionp update)
    (signal 'wrong-type-argument (list 'functionp update)))
  (dolist (callback (list after-pass on-fault))
    (unless (or (null callback) (functionp callback))
      (signal 'wrong-type-argument (list 'functionp callback))))
  (dolist (entry `((,mailbox-capacity . mailbox-capacity)
                   (,control-capacity . control-capacity)
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
   :data-lane (appkit-loop--lane-create mailbox-capacity)
   :control-lane (appkit-loop--lane-create control-capacity)
   :send-reserve send-reserve
   :message-limit message-limit
   :next-sequence 0
   :scheduled-handle nil
   :status 'running
   :fault nil))

(defun appkit-loop--lane-room-p (lane &optional reserve)
  "Return non-nil when LANE can admit one item using optional RESERVE."
  (< (appkit-loop--lane-count lane)
     (+ (appkit-loop--lane-capacity lane) (or reserve 0))))

(defun appkit-loop--lane-enqueue (lane cell)
  "Append CELL to LANE."
  (if (appkit-loop--lane-tail lane)
      (setcdr (appkit-loop--lane-tail lane) cell)
    (setf (appkit-loop--lane-head lane) cell))
  (setf (appkit-loop--lane-tail lane) cell
        (appkit-loop--lane-count lane)
        (1+ (appkit-loop--lane-count lane))))

(defun appkit-loop--lane-cutoff (lane maximum)
  "Return LANE's frozen tail sequence, capped by optional MAXIMUM."
  (when-let* ((tail (appkit-loop--lane-tail lane)))
    (let ((sequence
           (appkit-loop-envelope-sequence (car tail))))
      (if maximum (min sequence maximum) sequence))))

(defun appkit-loop--lane-dequeue-through (lane cutoff)
  "Remove and return LANE's first envelope when at or before CUTOFF."
  (when-let* ((cell (appkit-loop--lane-head lane)))
    (when (and cutoff
               (<= (appkit-loop-envelope-sequence (car cell)) cutoff))
      (setf (appkit-loop--lane-head lane) (cdr cell)
            (appkit-loop--lane-count lane)
            (1- (appkit-loop--lane-count lane)))
      (unless (appkit-loop--lane-head lane)
        (setf (appkit-loop--lane-tail lane) nil))
      (car cell))))

(defun appkit-loop--lane-purge (lane)
  "Clear LANE and return its former linked list."
  (prog1 (appkit-loop--lane-head lane)
    (setf (appkit-loop--lane-head lane) nil
          (appkit-loop--lane-tail lane) nil
          (appkit-loop--lane-count lane) 0)))

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
  "Ensure LOOP has one scheduled pass when either lane has pending work."
  (when (and (eq (appkit-loop--status loop) 'running)
             (> (appkit-loop-pending-count loop) 0)
             (not (eq appkit-loop--active-loop loop))
             (not (eq (car-safe appkit-loop--send-context) loop))
             (null (appkit-loop--scheduled-handle loop)))
    (setf (appkit-loop--scheduled-handle loop)
          (run-at-time 0 nil #'appkit-loop--scheduled-pass loop))))

(defun appkit-loop--make-envelope-cell
    (loop payload ticket incarnation reply-route
          origin source-address source-revision)
  "Allocate one queued envelope cell and its LOOP sequence."
  (let* ((sequence (appkit-loop--next-sequence loop))
         (envelope
          (appkit-loop--envelope-create
           :sequence sequence
           :incarnation incarnation
           :payload payload
           :origin origin
           :source-address source-address
           :source-revision source-revision
           :reply-route reply-route
           :ticket ticket)))
    (setf (appkit-loop--next-sequence loop) (1+ sequence))
    (when ticket
      (setf (appkit-loop-ticket-sequence ticket) sequence))
    (list envelope)))

(defun appkit-loop--enqueue-data
    (loop payload ticket &optional incarnation reply-route
          origin source-address source-revision)
  "Append PAYLOAD and optional TICKET to LOOP's bounded data lane."
  (let ((lane (appkit-loop--data-lane loop))
        (reserve (and ticket (appkit-loop--send-reserve loop))))
    (if (not (appkit-loop--lane-room-p lane reserve))
        'full
      (appkit-loop--lane-enqueue
       lane
       (appkit-loop--make-envelope-cell
        loop payload ticket
        (or incarnation (appkit-loop--incarnation loop))
        reply-route origin source-address source-revision))
      (appkit-loop--schedule loop)
      'enqueued)))

(defun appkit-loop--enqueue-control
    (loop payload incarnation &optional reply-route
          origin source-address source-revision)
  "Append PAYLOAD to LOOP's exactly bounded control lane."
  (let ((lane (appkit-loop--control-lane loop)))
    (if (not (appkit-loop--lane-room-p lane))
        'full
      (appkit-loop--lane-enqueue
       lane
       (appkit-loop--make-envelope-cell
        loop payload nil incarnation reply-route
        origin source-address source-revision))
      (appkit-loop--schedule loop)
      'enqueued)))

(defun appkit-loop--admission-status (loop)
  "Return LOOP's non-running admission outcome, or nil."
  (pcase (appkit-loop--status loop)
    ('faulted 'faulted)
    ((or 'stopping 'stopped) 'stopped)
    (_ nil)))

(defun appkit-loop--post-addressed
    (loop message incarnation &optional reply-route
          origin source-address source-revision)
  "Post MESSAGE to LOOP's data lane only at exact INCARNATION.

Optional arguments retain routed delivery metadata in the admitted envelope."
  (appkit-loop--assert-main-thread)
  (appkit-loop--check loop)
  (or (appkit-loop--admission-status loop)
      (if (/= incarnation (appkit-loop--incarnation loop))
          'stale
        (appkit-loop--enqueue-data
         loop message nil incarnation reply-route
         origin source-address source-revision))))

(defun appkit-loop--post-control-addressed
    (loop message incarnation &optional reply-route
          origin source-address source-revision)
  "Post runtime MESSAGE to LOOP's control lane at exact INCARNATION."
  (appkit-loop--assert-main-thread)
  (appkit-loop--check loop)
  (or (appkit-loop--admission-status loop)
      (if (/= incarnation (appkit-loop--incarnation loop))
          'stale
        (appkit-loop--enqueue-control
         loop message incarnation reply-route
         origin source-address source-revision))))(defun appkit-loop-post (loop message)
  "Try to enqueue MESSAGE in LOOP's data lane and return its outcome.\n\nThis is an external ingress for host events and callback adapters.  Client\nphases must return a closed post command; posting from any active pass signals\na contract error.  The result is one of `enqueued', `full', `stopped', or\n`faulted'.  A failed admission does not allocate a sequence number."
  (when appkit-loop--active-loop
    (error "Appkit loop posts are not allowed from an active pass"))
  (appkit-loop--post-addressed loop message
                               (appkit-loop-incarnation loop)))



(defun appkit-loop--complete-ticket (ticket state outcome &optional revision)
  "Complete pending TICKET once with STATE, OUTCOME, and REVISION."
  (when (and ticket (eq (appkit-loop-ticket-state ticket) 'pending))
    (setf (appkit-loop-ticket-state ticket) state
          (appkit-loop-ticket-outcome ticket) outcome
          (appkit-loop-ticket-revision ticket) revision)
    t))

(defun appkit-loop--purge (loop ticket-state outcome)
  "Clear both LOOP lanes, completing data tickets with TICKET-STATE and OUTCOME."
  (let ((data (appkit-loop--lane-purge (appkit-loop--data-lane loop))))
    (appkit-loop--lane-purge (appkit-loop--control-lane loop))
    (while data
      (appkit-loop--complete-ticket
       (appkit-loop-envelope-ticket (car data)) ticket-state outcome)
      (setq data (cdr data)))))

(defun appkit-loop--complete-pass-tickets (state outcome)
  "Complete current pass tickets with STATE and OUTCOME."
  (let ((entries (nreverse appkit-loop--pass-tickets)))
    (setq appkit-loop--pass-tickets nil)
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
      (appkit-loop--complete-pass-tickets 'faulted fault)
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
                (let ((appkit-loop--current-envelope envelope))
                  (setq result
                        (funcall (appkit-loop--update loop)
                                 (appkit-loop--model loop)
                                 (appkit-loop-envelope-payload envelope))
                        completed-p t)))
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
            (push (cons ticket revision) appkit-loop--pass-tickets))
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
      (appkit-loop--complete-pass-tickets 'accepted nil)))))

(defun appkit-loop-run-pass (loop)
  "Run at most one frozen, bounded two-lane pass for LOOP.

Both lane cutoffs are frozen before processing begins.  Already-arrived
control envelopes drain before data envelopes.  Work admitted during the pass
waits for a later pass, and MESSAGE-LIMIT bounds the two lanes together.
Return the total number of envelopes removed."
  (appkit-loop--assert-main-thread)
  (appkit-loop--check loop)
  (when appkit-loop--active-loop
    (error "Appkit loop pass is not reentrant"))
  (let ((processed 0))
    (unwind-protect
        (let ((appkit-loop--active-loop loop)
              (appkit-loop--current-envelope nil)
              (appkit-loop--pass-context nil)
              (appkit-loop--pass-tickets nil))
          (when (eq (appkit-loop--status loop) 'running)
            (appkit-loop--cancel-scheduled loop)
            (let* ((send-context
                    (and (eq (car-safe appkit-loop--send-context) loop)
                         appkit-loop--send-context))
                   (sync-cutoff
                    (and send-context
                         (appkit-loop-ticket-sequence (cdr send-context))))
                   (control-lane (appkit-loop--control-lane loop))
                   (data-lane (appkit-loop--data-lane loop))
                   (control-cutoff
                    (appkit-loop--lane-cutoff control-lane sync-cutoff))
                   (data-cutoff
                    (appkit-loop--lane-cutoff data-lane sync-cutoff))
                   (limit (appkit-loop--message-limit loop))
                   (start-revision (appkit-loop--revision loop))
                   (accepted 0)
                   (lane control-lane)
                   (cutoff control-cutoff)
                   (phase 0)
                   envelope)
              (while (< phase 2)
                (while
                    (and (eq (appkit-loop--status loop) 'running)
                         (< processed limit)
                         (setq envelope
                               (appkit-loop--lane-dequeue-through
                                lane cutoff)))
                  (let ((outcome
                         (appkit-loop--apply-transition loop envelope)))
                    (setq processed (1+ processed))
                    (when (eq outcome 'accepted)
                      (setq accepted (1+ accepted)))))
                (setq phase (1+ phase)
                      lane data-lane
                      cutoff data-cutoff))
              (when (and (> accepted 0)
                         (eq (appkit-loop--status loop) 'running))
                (appkit-loop--finish-pass
                 loop
                 (appkit-loop--pass-create
                  :processed processed
                  :accepted accepted
                  :start-revision start-revision
                  :end-revision (appkit-loop--revision loop)))))))
      (appkit-loop--schedule loop))
    processed))

(defun appkit-loop--resignal-fault (loop)
  "Re-signal LOOP's stored fault condition."
  (let ((condition
         (appkit-loop-fault-condition (appkit-loop--fault loop))))
    (signal (car condition) (cdr condition))))

(defun appkit-loop-send (loop message)
  "Synchronously send MESSAGE through LOOP and return its terminal ticket.

A send uses reserved capacity and drives as many bounded passes as necessary
without crossing messages queued after its ticket.  Return `reentrant-send'
when called from a pass or another send, `busy' when all send reserve is
occupied, and `stopped' or `faulted' when LOOP cannot admit work.  An admitted
send that encounters a runtime fault re-signals the stored condition."
  (appkit-loop--assert-main-thread)
  (appkit-loop--check loop)
  (cond
   ((or appkit-loop--active-loop appkit-loop--send-context)
    'reentrant-send)
   ((appkit-loop--admission-status loop))
   ((not
     (appkit-loop--lane-room-p
      (appkit-loop--data-lane loop) (appkit-loop--send-reserve loop)))
    'busy)
   (t
    (let ((ticket (appkit-loop--ticket-create :state 'pending)))
      (appkit-loop--cancel-scheduled loop)
      (unwind-protect
          (let ((appkit-loop--send-context (cons loop ticket)))
            (unless (eq (appkit-loop--enqueue-data loop message ticket)
                        'enqueued)
              (error "Appkit loop send admission invariant failed"))
            (while (and (eq (appkit-loop-ticket-state ticket) 'pending)
                        (eq (appkit-loop--status loop) 'running))
              (appkit-loop-run-pass loop)))
        (appkit-loop--schedule loop))
      (if (eq (appkit-loop-ticket-state ticket) 'faulted)
          (appkit-loop--resignal-fault loop)
        ticket)))))

(defun appkit-loop--begin-stop (loop)
  "Atomically close LOOP admission and revoke all queued authority.

Return non-nil only for the caller that changes LOOP to `stopping'."
  (appkit-loop--assert-main-thread)
  (appkit-loop--check loop)
  (unless (memq (appkit-loop--status loop) '(stopping stopped))
    (when appkit-loop--active-loop
      (error "Cannot stop an Appkit loop from an active pass"))
    (setf (appkit-loop--status loop) 'stopping
          (appkit-loop--incarnation loop)
          (1+ (appkit-loop--incarnation loop)))
    (appkit-loop--cancel-scheduled loop)
    (appkit-loop--complete-pass-tickets 'stopped nil)
    (appkit-loop--purge loop 'stopped nil)
    t))

(defun appkit-loop--finish-stop (loop)
  "Change LOOP from `stopping' to `stopped'."
  (appkit-loop--assert-main-thread)
  (appkit-loop--check loop)
  (when (eq (appkit-loop--status loop) 'stopping)
    (setf (appkit-loop--status loop) 'stopped)
    t))

(defun appkit-loop-stop (loop)
  "Stop LOOP, discard queued work, and terminate pending tickets.

Return non-nil when this call performs both stop phases.  Reentrant or repeated
calls are inert."
  (when (appkit-loop--begin-stop loop)
    (appkit-loop--finish-stop loop)
    t))

(provide 'appkit-loop)

;;; appkit-loop.el ends here
