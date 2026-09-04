;;; appkit-effect.el --- Managed finite asynchronous work  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Owner-scoped finite work for `appkit-loop'.  Effect callbacks only stage
;; owned payload and enqueue an internal wake value.  Observation and
;; settlement mappers run later, while the loop processes that value.  Keyed
;; replacement revokes delivery authority before invoking physical cleanup.

;;; Code:

(require 'cl-lib)
(require 'appkit-loop)

(cl-defstruct (appkit-cancellation
               (:constructor appkit-cancellation-create)
               (:copier nil))
  "A concrete cancellation capability returned by an Effect starter."
  kind
  cancel)

(cl-defstruct (appkit-effect-spec
               (:constructor appkit-effect--spec-create)
               (:copier nil))
  key
  input
  start
  observe
  observation-policy
  observation-pending-limit
  success
  failure
  cancellation-requirement)

(cl-defstruct (appkit-effect-runtime
               (:constructor appkit-effect--runtime-create)
               (:copier nil)
               (:conc-name appkit-effect--runtime-))
  loop
  instances
  max-active
  alive-p)

(cl-defstruct (appkit-effect-instance
               (:constructor appkit-effect--instance-create)
               (:copier nil)
               (:conc-name appkit-effect--instance-))
  runtime
  spec
  key
  token
  state
  starting-p
  cancellation
  abort-requested-p
  next-sequence
  observation-head
  observation-tail
  observation-count
  latest-observation
  coalesced-observations
  settlement
  wake-pending-p
  retry-timer)

(cl-defstruct (appkit-effect-observation
               (:constructor appkit-effect--observation-create)
               (:copier nil))
  sequence
  payload)

(cl-defstruct (appkit-effect-settlement
               (:constructor appkit-effect--settlement-create)
               (:copier nil))
  sequence
  kind
  payload)

(cl-defstruct (appkit-effect-delivery
               (:constructor appkit-effect--delivery-create)
               (:copier nil))
  runtime
  key
  token)

(defconst appkit-effect-stale
  (make-symbol "appkit-effect-stale")
  "Result returned when an internal Effect delivery has lost authority.")

(defun appkit-effect--assert-main-thread ()
  "Signal unless the current thread is Emacs's main thread."
  (unless (eq (current-thread) main-thread)
    (error "Appkit Effect mutation is restricted to the main Emacs thread")))

(defun appkit-effect--positive-integer-p (value)
  "Return non-nil when VALUE is a positive integer."
  (and (integerp value) (> value 0)))

(cl-defun appkit-effect-create
    (&key key input start observe observation-policy observation-pending-limit
          success failure (cancellation-requirement 'logical))
  "Create one finite Effect specification.

START receives an opaque context, INPUT, and observe/resolve/reject gates.
SUCCESS and FAILURE map terminal payloads to domain messages.  OBSERVE, when
present, maps nonterminal payloads.  OBSERVATION-POLICY is one of `lossless',
`latest', or `coalesce-by-key'.  Lossless and keyed coalescing require a
positive OBSERVATION-PENDING-LIMIT.  CANCELLATION-REQUIREMENT is `logical' or
`transport'."
  (unless (functionp start)
    (signal 'wrong-type-argument (list 'functionp start)))
  (unless (functionp success)
    (signal 'wrong-type-argument (list 'functionp success)))
  (unless (functionp failure)
    (signal 'wrong-type-argument (list 'functionp failure)))
  (unless (memq cancellation-requirement '(logical transport))
    (error "Unsupported Effect cancellation requirement: %S"
           cancellation-requirement))
  (if observe
      (progn
        (unless (functionp observe)
          (signal 'wrong-type-argument (list 'functionp observe)))
        (unless (memq observation-policy
                      '(lossless latest coalesce-by-key))
          (error "Unsupported Effect observation policy: %S"
                 observation-policy))
        (when (memq observation-policy '(lossless coalesce-by-key))
          (unless (appkit-effect--positive-integer-p
                   observation-pending-limit)
            (error "Effect %S observation pending limit must be positive"
                   key))))
    (when (or observation-policy observation-pending-limit)
      (error "Effect %S declares observation policy without a mapper" key)))
  (appkit-effect--spec-create
   :key key
   :input input
   :start start
   :observe observe
   :observation-policy observation-policy
   :observation-pending-limit observation-pending-limit
   :success success
   :failure failure
   :cancellation-requirement cancellation-requirement))

(defun appkit-effect--runtime-check (runtime)
  "Return RUNTIME, or signal a type error."
  (unless (appkit-effect-runtime-p runtime)
    (signal 'wrong-type-argument (list 'appkit-effect-runtime-p runtime)))
  runtime)

(defun appkit-effect-runtime-live-p (runtime)
  "Return non-nil when RUNTIME and its target loop can accept Effect work."
  (and (appkit-effect-runtime-p runtime)
       (appkit-effect--runtime-alive-p runtime)
       (eq (appkit-loop-status (appkit-effect--runtime-loop runtime))
           'running)))

(defun appkit-effect-runtime-count (runtime)
  "Return the number of active Effect instances in RUNTIME."
  (hash-table-count
   (appkit-effect--runtime-instances
    (appkit-effect--runtime-check runtime))))

(defun appkit-effect-runtime-create (loop &optional max-active)
  "Create an Effect runtime targeting LOOP.

MAX-ACTIVE defaults to 32 and bounds the keyed instance registry."
  (appkit-effect--assert-main-thread)
  (unless (and (appkit-loop-p loop) (eq (appkit-loop-status loop) 'running))
    (error "Cannot attach an Effect runtime to a non-running loop"))
  (let ((limit (or max-active 32)))
    (unless (appkit-effect--positive-integer-p limit)
      (error "Effect active limit must be positive: %S" limit))
    (appkit-effect--runtime-create
     :loop loop
     :instances (make-hash-table :test #'equal)
     :max-active limit
     :alive-p t)))

(defun appkit-effect--current-p (instance)
  "Return non-nil when INSTANCE retains keyed delivery authority."
  (let* ((runtime (appkit-effect--instance-runtime instance))
         (key (appkit-effect--instance-key instance)))
    (and (appkit-effect-runtime-live-p runtime)
         (eq instance
             (gethash key (appkit-effect--runtime-instances runtime)))
         (memq (appkit-effect--instance-state instance)
               '(starting active settlement-pending)))))

(defun appkit-effect-runtime-current-p (runtime key)
  "Return non-nil when RUNTIME has a current instance under KEY."
  (appkit-effect--runtime-check runtime)
  (let ((instance (gethash key (appkit-effect--runtime-instances runtime))))
    (and instance (appkit-effect--current-p instance))))

(defun appkit-effect--next-sequence (instance)
  "Allocate and return INSTANCE's next callback sequence."
  (let ((sequence (appkit-effect--instance-next-sequence instance)))
    (setf (appkit-effect--instance-next-sequence instance) (1+ sequence))
    sequence))

(defun appkit-effect--cancel-retry (instance)
  "Cancel INSTANCE's pending wake retry."
  (when-let* ((timer (appkit-effect--instance-retry-timer instance)))
    (setf (appkit-effect--instance-retry-timer instance) nil)
    (when (timerp timer)
      (cancel-timer timer))))

(defun appkit-effect--invoke-cancellation (instance)
  "Invoke and clear INSTANCE's concrete cancellation capability."
  (when-let* ((capability (appkit-effect--instance-cancellation instance)))
    (setf (appkit-effect--instance-cancellation instance) nil)
    (when-let* ((cancel (appkit-cancellation-cancel capability)))
      (funcall cancel))))

(defun appkit-effect--clear-pending (instance)
  "Clear all callback staging retained by INSTANCE."
  (setf (appkit-effect--instance-observation-head instance) nil
        (appkit-effect--instance-observation-tail instance) nil
        (appkit-effect--instance-observation-count instance) 0
        (appkit-effect--instance-latest-observation instance) nil
        (appkit-effect--instance-coalesced-observations instance) nil
        (appkit-effect--instance-settlement instance) nil
        (appkit-effect--instance-wake-pending-p instance) nil))

(defun appkit-effect--revoke (instance &optional cancel-p)
  "Revoke INSTANCE and optionally invoke physical cancellation."
  (let* ((runtime (appkit-effect--instance-runtime instance))
         (instances (appkit-effect--runtime-instances runtime))
         (key (appkit-effect--instance-key instance)))
    (when (eq instance (gethash key instances))
      (remhash key instances))
    (setf (appkit-effect--instance-state instance) 'revoked)
    (appkit-effect--cancel-retry instance)
    (appkit-effect--clear-pending instance)
    (if cancel-p
        (appkit-effect--invoke-cancellation instance)
      (setf (appkit-effect--instance-cancellation instance) nil))))

(defun appkit-effect--warn-cancellation (instance condition)
  "Report CONDITION raised while asynchronously cancelling INSTANCE."
  (display-warning
   'appkit-effect
   (format "Effect %S cancellation failed: %s"
           (appkit-effect--instance-key instance)
           (error-message-string condition))
   :warning))

(defun appkit-effect--revoke-from-gate (instance)
  "Revoke INSTANCE from an asynchronous callback without leaking errors."
  (condition-case condition
      (appkit-effect--revoke instance t)
    ((error quit)
     (appkit-effect--warn-cancellation instance condition))))

(defun appkit-effect--retry-wake (instance)
  "Retry delivery of INSTANCE's staged callback data."
  (setf (appkit-effect--instance-retry-timer instance) nil)
  (when (appkit-effect--current-p instance)
    (appkit-effect--request-wake instance)))

(defun appkit-effect--request-wake (instance)
  "Ensure INSTANCE has one internal delivery queued or scheduled for retry."
  (when (and (appkit-effect--current-p instance)
             (not (appkit-effect--instance-starting-p instance))
             (not (appkit-effect--instance-wake-pending-p instance))
             (null (appkit-effect--instance-retry-timer instance)))
    (let* ((runtime (appkit-effect--instance-runtime instance))
           (outcome
            (appkit-loop-post
             (appkit-effect--runtime-loop runtime)
             (appkit-effect--delivery-create
              :runtime runtime
              :key (appkit-effect--instance-key instance)
              :token (appkit-effect--instance-token instance)))))
      (pcase outcome
        ('enqueued
         (setf (appkit-effect--instance-wake-pending-p instance) t))
        ('full
         (setf (appkit-effect--instance-retry-timer instance)
               (run-at-time 0.001 nil #'appkit-effect--retry-wake instance)))
        ((or 'faulted 'stopped)
         (appkit-effect--revoke-from-gate instance))
        (_
         (error "Unexpected Effect wake admission outcome: %S" outcome))))))

(defun appkit-effect--append-lossless-observation (instance observation)
  "Append OBSERVATION to INSTANCE's lossless staging queue."
  (let ((cell (list observation)))
    (if (appkit-effect--instance-observation-tail instance)
        (setcdr (appkit-effect--instance-observation-tail instance) cell)
      (setf (appkit-effect--instance-observation-head instance) cell))
    (setf (appkit-effect--instance-observation-tail instance) cell
          (appkit-effect--instance-observation-count instance)
          (1+ (appkit-effect--instance-observation-count instance)))))

(defun appkit-effect--overflow (instance sequence)
  "Settle INSTANCE with observation overflow at SEQUENCE."
  (setf (appkit-effect--instance-state instance) 'settlement-pending
        (appkit-effect--instance-settlement instance)
        (appkit-effect--settlement-create
         :sequence sequence
         :kind 'failure
         :payload '(observation-overflow))
        (appkit-effect--instance-abort-requested-p instance) t)
  (unless (appkit-effect--instance-starting-p instance)
    (condition-case condition
        (appkit-effect--invoke-cancellation instance)
      ((error quit)
       (appkit-effect--warn-cancellation instance condition))))
  (appkit-effect--request-wake instance)
  nil)

(defun appkit-effect--observe (instance payload)
  "Admit one observation PAYLOAD for INSTANCE."
  (appkit-effect--assert-main-thread)
  (when (and (appkit-effect--current-p instance)
             (memq (appkit-effect--instance-state instance)
                   '(starting active)))
    (let* ((spec (appkit-effect--instance-spec instance))
           (mapper (appkit-effect-spec-observe spec)))
      (unless mapper
        (error "Effect %S does not accept observations"
               (appkit-effect--instance-key instance)))
      (let* ((sequence (appkit-effect--next-sequence instance))
             (observation
              (appkit-effect--observation-create
               :sequence sequence :payload payload))
             (policy (appkit-effect-spec-observation-policy spec)))
        (pcase policy
          ('lossless
           (if (>= (appkit-effect--instance-observation-count instance)
                   (appkit-effect-spec-observation-pending-limit spec))
               (appkit-effect--overflow instance sequence)
             (appkit-effect--append-lossless-observation
              instance observation)))
          ('latest
           (setf (appkit-effect--instance-latest-observation instance)
                 observation))
          ('coalesce-by-key
           (let* ((key (car payload))
                  (table
                   (or (appkit-effect--instance-coalesced-observations instance)
                       (setf
                        (appkit-effect--instance-coalesced-observations instance)
                        (make-hash-table :test #'equal))))
                  (existing (gethash key table)))
             (if (and (null existing)
                      (>= (hash-table-count table)
                          (appkit-effect-spec-observation-pending-limit spec)))
                 (appkit-effect--overflow instance sequence)
               (puthash key observation table))))
          (_ (error "Invalid Effect observation policy: %S" policy)))
        (when (memq (appkit-effect--instance-state instance)
                    '(starting active))
          (appkit-effect--request-wake instance)
          t)))))

(defun appkit-effect--settle (instance kind payload)
  "Admit INSTANCE's first terminal KIND with PAYLOAD."
  (appkit-effect--assert-main-thread)
  (when (and (appkit-effect--current-p instance)
             (memq (appkit-effect--instance-state instance)
                   '(starting active)))
    (setf (appkit-effect--instance-state instance) 'settlement-pending
          (appkit-effect--instance-settlement instance)
          (appkit-effect--settlement-create
           :sequence (appkit-effect--next-sequence instance)
           :kind kind
           :payload payload))
    (appkit-effect--request-wake instance)
    t))

(defun appkit-effect--valid-cancellation-p (spec capability settled-p)
  "Return non-nil when CAPABILITY satisfies SPEC and SETTLED-P."
  (cond
   ((null capability) settled-p)
   ((not (appkit-cancellation-p capability)) nil)
   ((eq (appkit-effect-spec-cancellation-requirement spec) 'logical)
    (and (memq (appkit-cancellation-kind capability) '(logical transport))
         (or (null (appkit-cancellation-cancel capability))
             (functionp (appkit-cancellation-cancel capability)))))
   ((eq (appkit-effect-spec-cancellation-requirement spec) 'transport)
    (and (eq (appkit-cancellation-kind capability) 'transport)
         (functionp (appkit-cancellation-cancel capability))))
   (t nil)))

(defun appkit-effect--cleanup-late-capability (instance capability)
  "Cancel CAPABILITY returned after INSTANCE lost authority."
  (when (appkit-cancellation-p capability)
    (setf (appkit-effect--instance-cancellation instance) capability)
    (appkit-effect--invoke-cancellation instance)))

(defun appkit-effect-runtime-start (runtime spec)
  "Start SPEC as RUNTIME's current keyed Effect and return its instance.

An existing instance with an equal key is revoked and cancelled first.  The
new instance occupies its keyed slot before its starter can synchronously call
an output gate."
  (appkit-effect--assert-main-thread)
  (appkit-effect--runtime-check runtime)
  (unless (appkit-effect-runtime-live-p runtime)
    (error "Cannot start an Effect in a stopped runtime"))
  (unless (appkit-effect-spec-p spec)
    (signal 'wrong-type-argument (list 'appkit-effect-spec-p spec)))
  (let* ((key (appkit-effect-spec-key spec))
         (instances (appkit-effect--runtime-instances runtime))
         (old (gethash key instances)))
    (when old
      (appkit-effect--revoke old t))
    (when (>= (hash-table-count instances)
              (appkit-effect--runtime-max-active runtime))
      (error "Effect active limit reached: %S"
             (appkit-effect--runtime-max-active runtime)))
    (let* ((instance
            (appkit-effect--instance-create
             :runtime runtime
             :spec spec
             :key key
             :token (make-symbol "appkit-effect-token")
             :state 'starting
             :starting-p t
             :cancellation nil
             :abort-requested-p nil
             :next-sequence 0
             :observation-head nil
             :observation-tail nil
             :observation-count 0
             :latest-observation nil
             :coalesced-observations nil
             :settlement nil
             :wake-pending-p nil
             :retry-timer nil))
           (context (list 'appkit-effect-context
                          (appkit-loop-incarnation
                           (appkit-effect--runtime-loop runtime))
                          key))
           completed-p
           condition
           capability)
      (puthash key instance instances)
      (unwind-protect
          (condition-case err
              (progn
                (setq capability
                      (funcall
                       (appkit-effect-spec-start spec)
                       context
                       (appkit-effect-spec-input spec)
                       (lambda (&rest payload)
                         (appkit-effect--observe instance payload))
                       (lambda (&rest payload)
                         (appkit-effect--settle instance 'success payload))
                       (lambda (&rest payload)
                         (appkit-effect--settle instance 'failure payload)))
                      completed-p t))
            ((error quit)
             (setq condition err
                   completed-p t)))
        (unless completed-p
          (appkit-effect--revoke instance t)))
      (when condition
        (appkit-effect--revoke instance t)
        (signal (car condition) (cdr condition)))
      (setf (appkit-effect--instance-starting-p instance) nil)
      (cond
       ((not (appkit-effect--current-p instance))
        (appkit-effect--cleanup-late-capability instance capability))
       ((not
         (appkit-effect--valid-cancellation-p
          spec capability
          (eq (appkit-effect--instance-state instance) 'settlement-pending)))
        (when (appkit-cancellation-p capability)
          (setf (appkit-effect--instance-cancellation instance) capability))
        (appkit-effect--revoke instance t)
        (error "Effect %S starter returned invalid cancellation capability: %S"
               key capability))
       (t
        (setf (appkit-effect--instance-cancellation instance) capability)
        (unless (eq (appkit-effect--instance-state instance)
                    'settlement-pending)
          (setf (appkit-effect--instance-state instance) 'active))
        (when (appkit-effect--instance-abort-requested-p instance)
          (condition-case abort-condition
              (appkit-effect--invoke-cancellation instance)
            ((error quit)
             (appkit-effect--warn-cancellation instance abort-condition))))
        (when (appkit-effect--pending-p instance)
          (appkit-effect--request-wake instance))))
      instance)))

(defun appkit-effect-runtime-cancel (runtime key)
  "Revoke and cancel RUNTIME's current Effect under KEY."
  (appkit-effect--assert-main-thread)
  (appkit-effect--runtime-check runtime)
  (when-let* ((instance
               (gethash key (appkit-effect--runtime-instances runtime))))
    (appkit-effect--revoke instance t)
    t))

(defun appkit-effect--first-coalesced (instance)
  "Return INSTANCE's earliest retained keyed observation."
  (let (selected)
    (when-let* ((table
                 (appkit-effect--instance-coalesced-observations instance)))
      (maphash
       (lambda (_key observation)
         (when (or (null selected)
                   (< (appkit-effect-observation-sequence observation)
                      (appkit-effect-observation-sequence selected)))
           (setq selected observation)))
       table))
    selected))

(defun appkit-effect--first-observation (instance)
  "Return INSTANCE's next retained observation, or nil."
  (pcase (appkit-effect-spec-observation-policy
          (appkit-effect--instance-spec instance))
    ('lossless (car (appkit-effect--instance-observation-head instance)))
    ('latest (appkit-effect--instance-latest-observation instance))
    ('coalesce-by-key (appkit-effect--first-coalesced instance))
    (_ nil)))

(defun appkit-effect--pop-observation (instance observation)
  "Remove OBSERVATION from INSTANCE's policy-specific staging."
  (pcase (appkit-effect-spec-observation-policy
          (appkit-effect--instance-spec instance))
    ('lossless
     (let ((cell (appkit-effect--instance-observation-head instance)))
       (unless (eq (car cell) observation)
         (error "Effect lossless observation queue is corrupt"))
       (setf (appkit-effect--instance-observation-head instance) (cdr cell)
             (appkit-effect--instance-observation-count instance)
             (1- (appkit-effect--instance-observation-count instance)))
       (unless (appkit-effect--instance-observation-head instance)
         (setf (appkit-effect--instance-observation-tail instance) nil))))
    ('latest
     (unless (eq (appkit-effect--instance-latest-observation instance)
                 observation)
       (error "Effect latest observation slot is corrupt"))
     (setf (appkit-effect--instance-latest-observation instance) nil))
    ('coalesce-by-key
     (let* ((payload (appkit-effect-observation-payload observation))
            (key (car payload))
            (table (appkit-effect--instance-coalesced-observations instance)))
       (unless (eq (gethash key table) observation)
         (error "Effect coalesced observation table is corrupt"))
       (remhash key table)))
    (_ (error "Effect has no observation policy"))))

(defun appkit-effect--pending-p (instance)
  "Return non-nil when INSTANCE retains callback data to deliver."
  (or (appkit-effect--first-observation instance)
      (appkit-effect--instance-settlement instance)))

(defun appkit-effect-runtime-consume (runtime delivery)
  "Consume one internal DELIVERY and return one mapped domain message.

Return `appkit-effect-stale' without calling a mapper when DELIVERY no longer
names RUNTIME's current keyed instance."
  (appkit-effect--assert-main-thread)
  (appkit-effect--runtime-check runtime)
  (unless (appkit-effect-delivery-p delivery)
    (signal 'wrong-type-argument (list 'appkit-effect-delivery-p delivery)))
  (let* ((key (appkit-effect-delivery-key delivery))
         (instance
          (and (eq runtime (appkit-effect-delivery-runtime delivery))
               (gethash key (appkit-effect--runtime-instances runtime)))))
    (if (not (and instance
                  (eq (appkit-effect--instance-token instance)
                      (appkit-effect-delivery-token delivery))
                  (appkit-effect--current-p instance)))
        appkit-effect-stale
      (setf (appkit-effect--instance-wake-pending-p instance) nil)
      (let ((observation (appkit-effect--first-observation instance))
            (settlement (appkit-effect--instance-settlement instance))
            (spec (appkit-effect--instance-spec instance)))
        (cond
         (observation
          (appkit-effect--pop-observation instance observation)
          (when (appkit-effect--pending-p instance)
            (appkit-effect--request-wake instance))
          (apply (appkit-effect-spec-observe spec)
                 (appkit-effect-spec-input spec)
                 (appkit-effect-observation-payload observation)))
         (settlement
          ;; Retire authority before calling client mapping code.
          (remhash key (appkit-effect--runtime-instances runtime))
          (setf (appkit-effect--instance-state instance) 'terminal
                (appkit-effect--instance-settlement instance) nil
                (appkit-effect--instance-cancellation instance) nil)
          (appkit-effect--cancel-retry instance)
          (apply (if (eq (appkit-effect-settlement-kind settlement) 'success)
                     (appkit-effect-spec-success spec)
                   (appkit-effect-spec-failure spec))
                 (appkit-effect-spec-input spec)
                 (appkit-effect-settlement-payload settlement)))
         (t appkit-effect-stale))))))

(defun appkit-effect-runtime-dispatch
    (runtime client-update model input)
  "Dispatch INPUT through RUNTIME before CLIENT-UPDATE.

Internal Effect delivery is mapped to one domain message in this loop turn.
Stale delivery becomes a normal rejection without calling CLIENT-UPDATE."
  (if (appkit-effect-delivery-p input)
      (let ((message (appkit-effect-runtime-consume runtime input)))
        (if (eq message appkit-effect-stale)
            (appkit-loop-reject 'stale-effect-output)
          (funcall client-update model message)))
    (funcall client-update model input)))

(defun appkit-effect-runtime-stop (runtime)
  "Revoke and cancel every instance owned by RUNTIME.

Return non-nil on the first stop.  Cleanup continues after individual
cancellation failures, then re-signals the first condition."
  (appkit-effect--assert-main-thread)
  (appkit-effect--runtime-check runtime)
  (when (appkit-effect--runtime-alive-p runtime)
    (let (instances first-condition)
      (maphash (lambda (_key instance) (push instance instances))
               (appkit-effect--runtime-instances runtime))
      (clrhash (appkit-effect--runtime-instances runtime))
      (setf (appkit-effect--runtime-alive-p runtime) nil)
      (dolist (instance instances)
        (condition-case condition
            (appkit-effect--revoke instance t)
          ((error quit)
           (unless first-condition
             (setq first-condition condition)))))
      (when first-condition
        (signal (car first-condition) (cdr first-condition)))
      t)))

(provide 'appkit-effect)

;;; appkit-effect.el ends here
