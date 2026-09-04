;;; appkit-source.el --- Declarative long-lived producers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; App/Surface-scoped desired Source reconciliation, bounded multi-emission
;; staging, terminal tombstones, quiescent replacement, and outbound gates.

;;; Code:

(require 'cl-lib)
(require 'appkit-cleanup)
(require 'appkit-effect)
(require 'appkit-loop)

(defconst appkit-source-stale 'appkit-source-stale)
(defconst appkit-source-companion 'appkit-source-companion)
(defconst appkit-source-outbound-pending 'appkit-source-outbound-pending)

(cl-defstruct (appkit-source-spec
               (:constructor appkit-source-spec--create)
               (:copier nil))
  key identity input start event closed outbound emission-policy pending-limit
  outbound-pending-limit cancellation-requirement)

(cl-defstruct (appkit-source-cancellation
               (:constructor appkit-source-cancellation--create)
               (:copier nil))
  kind cancel)

(cl-defstruct (appkit-source--item
               (:constructor appkit-source--item-create)
               (:copier nil))
  sequence kind payload mapper mapper-input token)

(cl-defstruct (appkit-source--intent
               (:constructor appkit-source--intent-create)
               (:copier nil))
  token mapper input state)

(cl-defstruct (appkit-source--instance
               (:constructor appkit-source--instance-create)
               (:copier nil))
  runtime spec token state cancellation ready-p quiescence-posted-p
  event-head event-tail event-count latest-event coalesced-events
  result-head result-tail close-item intents intent-order intent-count)

(cl-defstruct (appkit-source-delivery
               (:constructor appkit-source--delivery-create)
               (:copier nil))
  runtime)

(cl-defstruct (appkit-source--quiescent-delivery
               (:constructor appkit-source--quiescent-delivery-create)
               (:copier nil))
  runtime instance token)

(cl-defstruct (appkit-source-runtime
               (:constructor appkit-source--runtime-create)
               (:copier nil))
  loop instances desired pending-starts max-sources
  ready-head ready-tail ready-count wake-pending-p
  orphan-head orphan-tail sequence alive-p)

(cl-defun appkit-source-spec-create
    (&key key identity input start event closed outbound
          (emission-policy 'lossless) pending-limit outbound-pending-limit
          (cancellation-requirement 'logical))
  "Create and validate one declarative Source specification."
  (unless key (error "Source key must be non-nil"))
  (unless identity (error "Source identity must be non-nil"))
  (dolist (callback (list start event closed))
    (unless (functionp callback)
      (signal 'wrong-type-argument (list 'functionp callback))))
  (unless (or (null outbound) (functionp outbound))
    (signal 'wrong-type-argument (list 'functionp outbound)))
  (unless (memq emission-policy '(lossless latest coalesce-by-key))
    (error "Unsupported Source emission policy: %S" emission-policy))
  (pcase emission-policy
    ((or 'lossless 'coalesce-by-key)
     (unless (and (integerp pending-limit) (> pending-limit 0))
       (error "Source %S requires a positive pending limit" emission-policy)))
    ('latest
     (when (and pending-limit (not (eq pending-limit 1)))
       (error "Latest Source pending limit is fixed at one"))))
  (if outbound
      (unless (and (integerp outbound-pending-limit)
                   (> outbound-pending-limit 0))
        (error "Outbound Source requires a positive pending limit"))
    (when outbound-pending-limit
      (error "Source without outbound adapter cannot set an outbound limit")))
  (unless (memq cancellation-requirement
                '(logical transport quiescent-transport))
    (error "Unsupported Source cancellation requirement: %S"
           cancellation-requirement))
  (appkit-source-spec--create
   :key key :identity identity :input input :start start :event event
   :closed closed :outbound outbound :emission-policy emission-policy
   :pending-limit pending-limit
   :outbound-pending-limit outbound-pending-limit
   :cancellation-requirement cancellation-requirement))

(cl-defun appkit-source-cancellation-create (&key kind cancel)
  "Create a concrete Source cancellation capability."
  (unless (memq kind '(logical transport quiescent-transport))
    (error "Unsupported Source cancellation kind: %S" kind))
  (pcase kind
    ('logical
     (unless (or (null cancel) (functionp cancel))
       (signal 'wrong-type-argument (list 'functionp cancel))))
    ('transport
     (unless (functionp cancel)
       (error "Transport Source cancellation must be callable")))
    ('quiescent-transport
     (unless (functionp cancel)
       (error "Quiescent Source cancellation must be callable"))))
  (appkit-source-cancellation--create :kind kind :cancel cancel))

(defun appkit-source-runtime-create (loop &optional max-sources)
  "Create a Source runtime owned by LOOP."
  (unless (appkit-loop-p loop)
    (signal 'wrong-type-argument (list 'appkit-loop-p loop)))
  (let ((limit (or max-sources 32)))
    (unless (and (integerp limit) (> limit 0))
      (error "Source limit must be positive: %S" limit))
    (appkit-source--runtime-create
     :loop loop
     :instances (make-hash-table :test #'equal)
     :desired (make-hash-table :test #'equal)
     :max-sources limit
     :ready-count 0
     :sequence 0
     :alive-p t)))

(defun appkit-source--runtime-check (runtime)
  "Return RUNTIME or signal a type error."
  (unless (appkit-source-runtime-p runtime)
    (signal 'wrong-type-argument (list 'appkit-source-runtime-p runtime)))
  runtime)

(defun appkit-source--next-sequence (runtime)
  "Allocate one callback ordering sequence from RUNTIME."
  (prog1 (appkit-source-runtime-sequence runtime)
    (setf (appkit-source-runtime-sequence runtime)
          (1+ (appkit-source-runtime-sequence runtime)))))

(defun appkit-source--current-p (instance)
  "Return non-nil when INSTANCE still owns its Source key."
  (let* ((runtime (appkit-source--instance-runtime instance))
         (spec (appkit-source--instance-spec instance)))
    (and (appkit-source-runtime-alive-p runtime)
         (eq instance
             (gethash (appkit-source-spec-key spec)
                      (appkit-source-runtime-instances runtime))))))

(defun appkit-source--instance-pending-p (instance)
  "Return non-nil when INSTANCE retains a mapped delivery."
  (or (appkit-source--first-event instance)
      (appkit-source--instance-result-head instance)
      (appkit-source--instance-close-item instance)))

(defun appkit-source--enqueue-ready (instance)
  "Queue INSTANCE once behind its Source runtime wake."
  (unless (appkit-source--instance-ready-p instance)
    (let* ((runtime (appkit-source--instance-runtime instance))
           (cell (list instance)))
      (setf (appkit-source--instance-ready-p instance) t)
      (if (appkit-source-runtime-ready-tail runtime)
          (setcdr (appkit-source-runtime-ready-tail runtime) cell)
        (setf (appkit-source-runtime-ready-head runtime) cell))
      (setf (appkit-source-runtime-ready-tail runtime) cell
            (appkit-source-runtime-ready-count runtime)
            (1+ (appkit-source-runtime-ready-count runtime))))))

(defun appkit-source--ensure-wake (runtime)
  "Ensure RUNTIME has one bounded control wake for staged callback data."
  (when (and (appkit-source-runtime-alive-p runtime)
             (not (appkit-source-runtime-wake-pending-p runtime))
             (or (appkit-source-runtime-ready-head runtime)
                 (appkit-source-runtime-orphan-head runtime)))
    (let* ((loop (appkit-source-runtime-loop runtime))
           (outcome
            (appkit-loop--post-control-addressed
             loop (appkit-source--delivery-create :runtime runtime)
             (appkit-loop-incarnation loop))))
      (pcase outcome
        ('enqueued
         (setf (appkit-source-runtime-wake-pending-p runtime) t))
        ('full
         (appkit-loop--enter-fault
          loop '(error "Source runtime control lane is full") nil))))))

(defun appkit-source--request-wake (instance)
  "Make pending INSTANCE callback data visible to a later pass."
  (when (and (appkit-source--current-p instance)
             (appkit-source--instance-pending-p instance))
    (appkit-source--enqueue-ready instance)
    (appkit-source--ensure-wake (appkit-source--instance-runtime instance))))

(defun appkit-source--append-result (instance item)
  "Append outbound result ITEM to INSTANCE."
  (let ((cell (list item)))
    (if (appkit-source--instance-result-tail instance)
        (setcdr (appkit-source--instance-result-tail instance) cell)
      (setf (appkit-source--instance-result-head instance) cell))
    (setf (appkit-source--instance-result-tail instance) cell)))

(defun appkit-source--append-orphan (runtime item)
  "Append standalone result ITEM to RUNTIME."
  (let ((cell (list item)))
    (if (appkit-source-runtime-orphan-tail runtime)
        (setcdr (appkit-source-runtime-orphan-tail runtime) cell)
      (setf (appkit-source-runtime-orphan-head runtime) cell))
    (setf (appkit-source-runtime-orphan-tail runtime) cell)
    (appkit-source--ensure-wake runtime)))
(defun appkit-source--stage-runtime-fault (runtime condition)
  "Stage runtime contract CONDITION for faulting RUNTIME's owner in a pass."
  (appkit-source--append-orphan
   runtime
   (appkit-source--item-create
    :sequence (appkit-source--next-sequence runtime)
    :kind 'fault :payload condition)))

(defun appkit-source--fault-instance (instance condition)
  "Revoke INSTANCE and stage owner fault CONDITION through its mailbox."
  (let* ((runtime (appkit-source--instance-runtime instance))
         (spec (appkit-source--instance-spec instance))
         (key (appkit-source-spec-key spec)))
    (when (eq instance
              (gethash key (appkit-source-runtime-instances runtime)))
      (remhash key (appkit-source-runtime-instances runtime)))
    (condition-case cleanup-condition
        (appkit-source--revoke instance)
      ((error quit)
       (appkit--warn-cleanup-conditions
        (list cleanup-condition) 'appkit-source)))
    (appkit-source--stage-runtime-fault runtime condition)))

(defun appkit-source--first-coalesced-event (instance)
  "Return the oldest retained coalesced event for INSTANCE."
  (let (selected)
    (maphash
     (lambda (_key item)
       (when (or (null selected)
                 (< (appkit-source--item-sequence item)
                    (appkit-source--item-sequence selected)))
         (setq selected item)))
     (appkit-source--instance-coalesced-events instance))
    selected))

(defun appkit-source--first-event (instance)
  "Return INSTANCE's next policy-specific event item."
  (pcase (appkit-source-spec-emission-policy
          (appkit-source--instance-spec instance))
    ('lossless (car (appkit-source--instance-event-head instance)))
    ('latest (appkit-source--instance-latest-event instance))
    ('coalesce-by-key (appkit-source--first-coalesced-event instance))))

(defun appkit-source--pop-event (instance item)
  "Remove event ITEM from INSTANCE's staging area."
  (pcase (appkit-source-spec-emission-policy
          (appkit-source--instance-spec instance))
    ('lossless
     (let ((cell (appkit-source--instance-event-head instance)))

       (unless (eq item (car cell))
         (error "Source lossless event queue is corrupt"))
       (setf (appkit-source--instance-event-head instance) (cdr cell)
             (appkit-source--instance-event-count instance)
             (1- (appkit-source--instance-event-count instance)))
       (unless (appkit-source--instance-event-head instance)
         (setf (appkit-source--instance-event-tail instance) nil))))
    ('latest
     (unless (eq item (appkit-source--instance-latest-event instance))
       (error "Source latest event slot is corrupt"))
     (setf (appkit-source--instance-latest-event instance) nil))
    ('coalesce-by-key
     (let* ((payload (appkit-source--item-payload item))
            (key (car payload))
            (table (appkit-source--instance-coalesced-events instance)))
       (unless (eq item (gethash key table))
         (error "Source coalesced event table is corrupt"))
       (remhash key table)))
    (_ (error "Source has no emission policy"))))

(defun appkit-source--stage-event (instance payload)
  "Stage one owned event PAYLOAD under INSTANCE's bounded policy."
  (when (and (appkit-source--current-p instance)
             (eq (appkit-source--instance-state instance) 'active))
    (let* ((runtime (appkit-source--instance-runtime instance))
           (spec (appkit-source--instance-spec instance))
           (item
            (appkit-source--item-create
             :sequence (appkit-source--next-sequence runtime)
             :kind 'event :payload payload)))
      (pcase (appkit-source-spec-emission-policy spec)
        ('lossless
         (if (>= (appkit-source--instance-event-count instance)
                 (appkit-source-spec-pending-limit spec))
             (appkit-source--overflow instance)
           (let ((cell (list item)))
             (if (appkit-source--instance-event-tail instance)
                 (setcdr (appkit-source--instance-event-tail instance) cell)
               (setf (appkit-source--instance-event-head instance) cell))
             (setf (appkit-source--instance-event-tail instance) cell
                   (appkit-source--instance-event-count instance)
                   (1+ (appkit-source--instance-event-count instance))))))
        ('latest
         (setf (appkit-source--instance-latest-event instance) item))
        ('coalesce-by-key
         (let* ((key (car payload))
                (table (appkit-source--instance-coalesced-events instance)))
           (cond
            ((null key)
             (appkit-source--fault-instance
              instance
              '(error "Coalesced Source emission requires a non-nil key")))
            ((or (gethash key table)
                 (< (hash-table-count table)
                    (appkit-source-spec-pending-limit spec)))
             (puthash key item table))
            (t (appkit-source--overflow instance))))))
      (appkit-source--request-wake instance)
      t)))

(defun appkit-source--settle-intents-closed (instance)
  "Stage `closed' for every outstanding outbound intent in creation order."
  (dolist (intent (appkit-source--instance-intent-order instance))
    (when (eq (appkit-source--intent-state intent) 'pending)
      (setf (appkit-source--intent-state intent) 'settled)
      (appkit-source--append-result
       instance
       (appkit-source--item-create
        :sequence
        (appkit-source--next-sequence
         (appkit-source--instance-runtime instance))
        :kind 'outbound
        :mapper (appkit-source--intent-mapper intent)
        :mapper-input (appkit-source--intent-input intent)
        :payload 'closed
        :token (appkit-source--intent-token intent))))))

(defun appkit-source--close (instance reason &optional cancel-p)
  "Close current INSTANCE once with owned REASON and optional cancellation."
  (when (and (appkit-source--current-p instance)
             (eq (appkit-source--instance-state instance) 'active))
    (setf (appkit-source--instance-state instance) 'closing)
    (appkit-source--settle-intents-closed instance)
    (setf (appkit-source--instance-close-item instance)
          (appkit-source--item-create
           :sequence
           (appkit-source--next-sequence
            (appkit-source--instance-runtime instance))
           :kind 'closed :payload reason))
    (when cancel-p
      (appkit-source--invoke-cancellation instance #'ignore))
    (appkit-source--request-wake instance)
    t))

(defun appkit-source--overflow (instance)
  "Close INSTANCE through the runtime-visible overflow path."
  (condition-case condition
      (appkit-source--close instance '(source-overflow) t)
    ((error quit)
     (appkit-source--fault-instance instance condition))))

(defun appkit-source--valid-cancellation-p (spec capability terminal-p)
  "Return non-nil when CAPABILITY satisfies SPEC and TERMINAL-P."
  (if (null capability)
      terminal-p
    (and
     (appkit-source-cancellation-p capability)
     (pcase (appkit-source-spec-cancellation-requirement spec)
       ('logical
        (and (memq (appkit-source-cancellation-kind capability)
                   '(logical transport quiescent-transport))
             (or (null (appkit-source-cancellation-cancel capability))
                 (functionp (appkit-source-cancellation-cancel capability)))))
       ('transport
        (and (memq (appkit-source-cancellation-kind capability)
                   '(transport quiescent-transport))
             (functionp (appkit-source-cancellation-cancel capability))))
       ('quiescent-transport
        (and (eq (appkit-source-cancellation-kind capability)
                 'quiescent-transport)
             (functionp (appkit-source-cancellation-cancel capability))))))))

(defun appkit-source--invoke-cancellation (instance quiesced)
  "Invoke and clear INSTANCE cancellation, reporting quiescence to QUIESCED."
  (when-let* ((capability (appkit-source--instance-cancellation instance)))
    (setf (appkit-source--instance-cancellation instance) nil)
    (when-let* ((cancel (appkit-source-cancellation-cancel capability)))
      (if (eq (appkit-source-cancellation-kind capability)
              'quiescent-transport)
          (funcall cancel quiesced)
        (funcall cancel)
        (funcall quiesced)))))

(defun appkit-source--clear-instance (instance)
  "Discard every pending mapper and callback value held by INSTANCE."
  (setf (appkit-source--instance-ready-p instance) nil
        (appkit-source--instance-event-head instance) nil
        (appkit-source--instance-event-tail instance) nil
        (appkit-source--instance-event-count instance) 0
        (appkit-source--instance-latest-event instance) nil
        (appkit-source--instance-result-head instance) nil
        (appkit-source--instance-result-tail instance) nil
        (appkit-source--instance-close-item instance) nil
        (appkit-source--instance-intent-order instance) nil
        (appkit-source--instance-intent-count instance) 0)
  (when-let* ((table (appkit-source--instance-coalesced-events instance)))
    (clrhash table))
  (clrhash (appkit-source--instance-intents instance)))

(defun appkit-source--remove-ready-instance (runtime instance)
  "Remove stale INSTANCE occurrences from RUNTIME's bounded ready queue."
  (let ((queue (delq instance (appkit-source-runtime-ready-head runtime))))
    (setf (appkit-source-runtime-ready-head runtime) queue
          (appkit-source-runtime-ready-tail runtime) (last queue)
          (appkit-source-runtime-ready-count runtime) (length queue)
          (appkit-source--instance-ready-p instance) nil)))

(defun appkit-source--revoke (instance &optional quiesced)
  "Revoke INSTANCE mapping authority before physical cleanup."
  (let ((runtime (appkit-source--instance-runtime instance)))
    (setf (appkit-source--instance-state instance) 'revoked)
    (appkit-source--remove-ready-instance runtime instance)
    (appkit-source--clear-instance instance)
    (appkit-source--invoke-cancellation instance (or quiesced #'ignore))))

(defun appkit-source--start (runtime spec)
  "Start SPEC if it remains desired and its key is currently vacant."
  (let* ((key (appkit-source-spec-key spec))
         (desired (gethash key (appkit-source-runtime-desired runtime))))
    (when (and (appkit-source-runtime-alive-p runtime)
               (eq desired spec)
               (null (gethash key (appkit-source-runtime-instances runtime))))
      (let* ((instance
              (appkit-source--instance-create
               :runtime runtime :spec spec
               :token (make-symbol "appkit-source-instance-")
               :state 'active
               :event-count 0
               :coalesced-events (make-hash-table :test #'equal)
               :intents (make-hash-table :test #'eq)
               :intent-count 0))
             (token (appkit-source--instance-token instance))
             capability completed-p condition)
        (puthash key instance (appkit-source-runtime-instances runtime))
        (unwind-protect
            (condition-case err
                (progn
                  (setq capability
                        (funcall
                         (appkit-source-spec-start spec)
                         (list 'appkit-source-context token)
                         (appkit-source-spec-input spec)
                         (lambda (&rest payload)
                           (appkit-source--stage-event instance payload))
                         (lambda (&rest reason)
                           (appkit-source--close instance reason)))
                        completed-p t))
              ((error quit)
               (setq condition err completed-p t)))
          (unless completed-p
            (when (eq instance
                      (gethash key
                               (appkit-source-runtime-instances runtime)))
              (remhash key (appkit-source-runtime-instances runtime)))
            (appkit-source--revoke instance)))
        (when condition
          (when (eq instance
                    (gethash key (appkit-source-runtime-instances runtime)))
            (remhash key (appkit-source-runtime-instances runtime)))
          (appkit-source--revoke instance)
          (signal (car condition) (cdr condition)))
        (unless
            (appkit-source--valid-cancellation-p
             spec capability
             (memq (appkit-source--instance-state instance)
                   '(closing terminal revoked)))
          (when (appkit-source-cancellation-p capability)
            (setf (appkit-source--instance-cancellation instance) capability))
          (when (eq instance
                    (gethash key (appkit-source-runtime-instances runtime)))
            (remhash key (appkit-source-runtime-instances runtime)))
          (appkit-source--revoke instance)
          (error "Source %S returned invalid cancellation capability" key))
        (if (eq (appkit-source--instance-state instance) 'active)
            (setf (appkit-source--instance-cancellation instance) capability)
          (when (appkit-source-cancellation-p capability)
            (setf (appkit-source--instance-cancellation instance) capability)
            (appkit-source--invoke-cancellation instance #'ignore)))
        instance))))

(defun appkit-source--proper-bounded-list-p (value limit)
  "Return non-nil when VALUE is a proper list no longer than LIMIT."
  (let ((tail value) (count 0))
    (while (and (consp tail) (<= count limit))
      (setq tail (cdr tail) count (1+ count)))
    (and (null tail) (<= count limit))))

(defun appkit-source--validate-desired (runtime specs)
  "Return a unique desired table for bounded SPECS."
  (unless (appkit-source--proper-bounded-list-p
           specs (appkit-source-runtime-max-sources runtime))
    (error "Desired Source limit exceeded"))
  (let ((table (make-hash-table :test #'equal)))
    (dolist (spec specs table)
      (unless (appkit-source-spec-p spec)
        (signal 'wrong-type-argument (list 'appkit-source-spec-p spec)))
      (let ((key (appkit-source-spec-key spec)))
        (when (gethash key table)
          (error "Desired Sources duplicate key %S" key))
        (puthash key spec table)))))

(defun appkit-source--queue-start (runtime spec)
  "Retain SPEC once in RUNTIME's next start phase."
  (unless (memq spec (appkit-source-runtime-pending-starts runtime))
    (setf (appkit-source-runtime-pending-starts runtime)
          (append (appkit-source-runtime-pending-starts runtime)
                  (list spec)))))

(defun appkit-source--post-quiescent (instance token)
  "Post quiescence for revoked INSTANCE and TOKEN to its owner loop."
  (let* ((runtime (appkit-source--instance-runtime instance))
         (loop (appkit-source-runtime-loop runtime)))
    (when (and (appkit-source-runtime-alive-p runtime)
               (eq token (appkit-source--instance-token instance))
               (not (appkit-source--instance-quiescence-posted-p instance)))
      (setf (appkit-source--instance-quiescence-posted-p instance) t)
      (let ((outcome
             (appkit-loop--post-control-addressed
              loop
              (appkit-source--quiescent-delivery-create
               :runtime runtime :instance instance :token token)
              (appkit-loop-incarnation loop))))
        (when (eq outcome 'full)
          (appkit-loop--enter-fault
           loop '(error "Source quiescence control lane is full") nil))
        (eq outcome 'enqueued)))))

(defun appkit-source--begin-quiescent-replacement (instance)
  "Revoke INSTANCE and await physical quiescence before successor admission."
  (when (eq (appkit-source--instance-state instance) 'active)
    (let ((runtime (appkit-source--instance-runtime instance))
          (token (appkit-source--instance-token instance)))
      (setf (appkit-source--instance-state instance) 'stopping)
      (appkit-source--remove-ready-instance runtime instance)
      (appkit-source--clear-instance instance)
      (appkit-source--invoke-cancellation
       instance
       (lambda () (appkit-source--post-quiescent instance token))))))

(defun appkit-source-runtime-reconcile (runtime specs)
  "Reconcile RUNTIME once against final committed desired SPECS."
  (appkit-source--runtime-check runtime)
  (unless (appkit-source-runtime-alive-p runtime)
    (error "Cannot reconcile a stopped Source runtime"))
  (let ((desired (appkit-source--validate-desired runtime specs))
        (instances (appkit-source-runtime-instances runtime)))
    (setf (appkit-source-runtime-desired runtime) desired
          (appkit-source-runtime-pending-starts runtime) nil)
    (maphash
     (lambda (key instance)
       (let ((spec (gethash key desired))
             (current (appkit-source--instance-spec instance)))
         (cond
          ((null spec)
           (remhash key instances)
           (appkit-source--revoke instance))
          ((equal (appkit-source-spec-identity current)
                  (appkit-source-spec-identity spec)))
          ((and (eq (appkit-source--instance-state instance) 'active)
                (eq (appkit-source-spec-cancellation-requirement current)
                    'quiescent-transport))
           (appkit-source--begin-quiescent-replacement instance))
          ((eq (appkit-source--instance-state instance) 'stopping))
          (t
           (remhash key instances)
           (appkit-source--revoke instance)
           (appkit-source--queue-start runtime spec)))))
     (copy-hash-table instances))
    (maphash
     (lambda (key spec)
       (unless (gethash key instances)
         (appkit-source--queue-start runtime spec)))
     desired)
    t))

(defun appkit-source-runtime-start-pending (runtime)
  "Start RUNTIME Source successors admitted by the completed pass."
  (appkit-source--runtime-check runtime)
  (let ((pending (appkit-source-runtime-pending-starts runtime)))
    (setf (appkit-source-runtime-pending-starts runtime) nil)
    (dolist (spec pending)
      (appkit-source--start runtime spec))))

(defun appkit-source--stage-orphan-outcome (runtime mapper input outcome)
  "Stage one source-intent MAPPER call without a current Source instance."
  (appkit-source--append-orphan
   runtime
   (appkit-source--item-create
    :sequence (appkit-source--next-sequence runtime)
    :kind 'outbound :mapper mapper :mapper-input input :payload outcome)))

(defun appkit-source--stage-intent-outcome (instance intent outcome)
  "Settle current outbound INTENT once with validated OUTCOME."
  (when (and (appkit-source--current-p instance)
             (eq (appkit-source--instance-state instance) 'active)
             (eq (appkit-source--intent-state intent) 'pending)
             (eq intent
                 (gethash (appkit-source--intent-token intent)
                          (appkit-source--instance-intents instance))))
    (if (not (memq outcome '(accepted backpressured closed stale)))
        (progn
          (setf (appkit-source--intent-state intent) 'settled)
          (appkit-source--fault-instance
           instance
           (list 'error
                 (format "Source outbound callback returned invalid outcome: %S"
                         outcome))))
      (setf (appkit-source--intent-state intent) 'settled)
      (appkit-source--append-result
       instance
       (appkit-source--item-create
        :sequence
        (appkit-source--next-sequence
         (appkit-source--instance-runtime instance))
        :kind 'outbound
        :mapper (appkit-source--intent-mapper intent)
        :mapper-input (appkit-source--intent-input intent)
        :payload outcome
        :token (appkit-source--intent-token intent)))
      (appkit-source--request-wake instance))
    t))

(defun appkit-source-runtime-execute-intent
    (runtime key expected-identity payload result-mapper)
  "Execute one committed outbound intent against RUNTIME's current Source."
  (appkit-source--runtime-check runtime)
  (let ((instance (gethash key (appkit-source-runtime-instances runtime))))
    (cond
     ((or (null instance)
          (not (equal expected-identity
                      (appkit-source-spec-identity
                       (appkit-source--instance-spec instance)))))
      (appkit-source--stage-orphan-outcome
       runtime result-mapper payload 'stale))
     ((not (eq (appkit-source--instance-state instance) 'active))
      (appkit-source--stage-orphan-outcome
       runtime result-mapper payload 'closed))
     ((null (appkit-source-spec-outbound
             (appkit-source--instance-spec instance)))
      (error "Source %S has no outbound adapter" key))
     ((>= (appkit-source--instance-intent-count instance)
          (appkit-source-spec-outbound-pending-limit
           (appkit-source--instance-spec instance)))
      (appkit-source--stage-orphan-outcome
       runtime result-mapper payload 'backpressured))
     (t
      (let* ((spec (appkit-source--instance-spec instance))
             (token (make-symbol "appkit-source-intent-"))
             (intent
              (appkit-source--intent-create
               :token token :mapper result-mapper :input payload
               :state 'pending))
             returned completed-p condition)
        (puthash token intent (appkit-source--instance-intents instance))
        (setf (appkit-source--instance-intent-order instance)
              (append (appkit-source--instance-intent-order instance)
                      (list intent))
              (appkit-source--instance-intent-count instance)
              (1+ (appkit-source--instance-intent-count instance)))
        (unwind-protect
            (condition-case err
                (progn
                  (setq returned
                        (funcall
                         (appkit-source-spec-outbound spec)
                         (list 'appkit-source-outbound-context token)
                         (appkit-source-spec-input spec)
                         payload
                         (lambda (outcome)
                           (appkit-source--stage-intent-outcome
                            instance intent outcome)))
                        completed-p t))
              ((error quit)
               (setq condition err completed-p t)))
          (unless completed-p
            (remhash token (appkit-source--instance-intents instance))
            (setf (appkit-source--instance-intent-order instance)
                  (delq intent
                        (appkit-source--instance-intent-order instance))
                  (appkit-source--instance-intent-count instance)
                  (1- (appkit-source--instance-intent-count instance)))))
        (when condition
          (remhash token (appkit-source--instance-intents instance))
          (setf (appkit-source--instance-intent-order instance)
                (delq intent (appkit-source--instance-intent-order instance))
                (appkit-source--instance-intent-count instance)
                (1- (appkit-source--instance-intent-count instance)))
          (signal (car condition) (cdr condition)))
        (cond
         ((eq returned appkit-source-outbound-pending) nil)
         ((memq returned '(accepted backpressured closed stale))
          (unless (appkit-source--stage-intent-outcome
                   instance intent returned)
            (error "Source outbound adapter settled an intent twice")))
         (t
          (remhash token (appkit-source--instance-intents instance))
          (setf (appkit-source--instance-intent-order instance)
                (delq intent (appkit-source--instance-intent-order instance))
                (appkit-source--instance-intent-count instance)
                (1- (appkit-source--instance-intent-count instance)))
          (error "Source outbound adapter returned invalid outcome: %S"
                 returned))))))))

(defun appkit-source--pop-ready (runtime)
  "Pop one ready Source instance from RUNTIME."
  (when-let* ((cell (appkit-source-runtime-ready-head runtime)))
    (let ((instance (car cell))
          (rest (cdr cell)))
      (setf (appkit-source-runtime-ready-head runtime) rest
            (appkit-source-runtime-ready-tail runtime) (last rest)
            (appkit-source-runtime-ready-count runtime)
            (1- (appkit-source-runtime-ready-count runtime))
            (appkit-source--instance-ready-p instance) nil)
      instance)))

(defun appkit-source--first-item (instance)
  "Return INSTANCE's earliest retained event/result/close item."
  (let ((items (delq nil
                     (list (appkit-source--first-event instance)
                           (car (appkit-source--instance-result-head instance))
                           (appkit-source--instance-close-item instance))))
        selected)
    (dolist (item items selected)
      (when (or (null selected)
                (< (appkit-source--item-sequence item)
                   (appkit-source--item-sequence selected)))
        (setq selected item)))))

(defun appkit-source--pop-item (instance item)
  "Remove ITEM from INSTANCE and retire consumed outbound bookkeeping."
  (pcase (appkit-source--item-kind item)
    ('event (appkit-source--pop-event instance item))
    ('outbound
     (let ((cell (appkit-source--instance-result-head instance)))
       (unless (eq item (car cell))
         (error "Source outbound result queue is corrupt"))
       (setf (appkit-source--instance-result-head instance) (cdr cell))
       (unless (appkit-source--instance-result-head instance)
         (setf (appkit-source--instance-result-tail instance) nil)))
     (when-let* ((token (appkit-source--item-token item))
                 (intent
                  (gethash token
                           (appkit-source--instance-intents instance))))
       (remhash token (appkit-source--instance-intents instance))
       (setf (appkit-source--instance-intent-order instance)
             (delq intent (appkit-source--instance-intent-order instance))
             (appkit-source--instance-intent-count instance)
             (1- (appkit-source--instance-intent-count instance)))))
    ('closed
     (unless (eq item (appkit-source--instance-close-item instance))
       (error "Source close slot is corrupt"))
     (setf (appkit-source--instance-close-item instance) nil
           (appkit-source--instance-state instance) 'terminal
           (appkit-source--instance-cancellation instance) nil))))

(defun appkit-source--map-item (instance item)
  "Map one consumed INSTANCE ITEM to a domain message."
  (let ((spec (appkit-source--instance-spec instance)))
    (pcase (appkit-source--item-kind item)
      ('event
       (apply (appkit-source-spec-event spec)
              (appkit-source-spec-input spec)
              (appkit-source--item-payload item)))
      ('outbound
       (funcall (appkit-source--item-mapper item)
                (appkit-source--item-mapper-input item)
                (appkit-source--item-payload item)))
      ('closed
       (apply (appkit-source-spec-closed spec)
              (appkit-source-spec-input spec)
              (appkit-source--item-payload item))))))

(defun appkit-source-runtime-consume (runtime delivery)
  "Consume DELIVERY and return one mapped message or a runtime sentinel."
  (appkit-source--runtime-check runtime)
  (cond
   ((appkit-source--quiescent-delivery-p delivery)
    (let ((instance (appkit-source--quiescent-delivery-instance delivery)))
      (if (not
           (and (eq runtime
                    (appkit-source--quiescent-delivery-runtime delivery))
                (eq (appkit-source--quiescent-delivery-token delivery)
                    (appkit-source--instance-token instance))
                (appkit-source--current-p instance)
                (eq (appkit-source--instance-state instance) 'stopping)))
          appkit-source-stale
        (let* ((key (appkit-source-spec-key
                     (appkit-source--instance-spec instance)))
               (desired (gethash key (appkit-source-runtime-desired runtime))))
          (remhash key (appkit-source-runtime-instances runtime))
          (setf (appkit-source--instance-state instance) 'revoked)
          (when desired (appkit-source--queue-start runtime desired))
          appkit-source-companion))))
   ((not (and (appkit-source-delivery-p delivery)
              (eq runtime (appkit-source-delivery-runtime delivery))))
    appkit-source-stale)
   (t
    (setf (appkit-source-runtime-wake-pending-p runtime) nil)
    (let (instance item)
      (while (and (setq instance (appkit-source--pop-ready runtime))
                  (not (and (appkit-source--current-p instance)
                            (setq item (appkit-source--first-item instance))))))
      (cond
       (item
        (appkit-source--pop-item instance item)
        (when (appkit-source--instance-pending-p instance)
          (appkit-source--enqueue-ready instance))
        (appkit-source--ensure-wake runtime)
        (appkit-source--map-item instance item))
       ((appkit-source-runtime-orphan-head runtime)
        (let* ((cell (appkit-source-runtime-orphan-head runtime))
               (orphan (car cell))
               (rest (cdr cell)))
          (setf (appkit-source-runtime-orphan-head runtime) rest
                (appkit-source-runtime-orphan-tail runtime) (last rest))
          (appkit-source--ensure-wake runtime)
          (if (eq (appkit-source--item-kind orphan) 'fault)
              (let ((condition (appkit-source--item-payload orphan)))
                (signal (car condition) (cdr condition)))
            (funcall (appkit-source--item-mapper orphan)
                     (appkit-source--item-mapper-input orphan)
                     (appkit-source--item-payload orphan)))))
       (t
        (appkit-source--ensure-wake runtime)
        appkit-source-stale))))))

(defun appkit-source-runtime-dispatch (runtime client-update model input)
  "Dispatch internal Source INPUT before ordinary CLIENT-UPDATE."
  (if (or (appkit-source-delivery-p input)
          (appkit-source--quiescent-delivery-p input))
      (let ((message (appkit-source-runtime-consume runtime input)))
        (cond
         ((eq message appkit-source-stale)
          (appkit-loop-reject 'stale-source-output))
         ((eq message appkit-source-companion)
          (appkit-loop-companion-accept))
         (t (funcall client-update model message))))
    (funcall client-update model input)))

(defun appkit-source-runtime-stop (runtime)
  "Revoke all Source instances and mapping authority in RUNTIME."
  (appkit-source--runtime-check runtime)
  (when (appkit-source-runtime-alive-p runtime)
    (let (instances conditions)
      (maphash (lambda (_key instance) (push instance instances))
               (appkit-source-runtime-instances runtime))
      (clrhash (appkit-source-runtime-instances runtime))
      (clrhash (appkit-source-runtime-desired runtime))
      (setf (appkit-source-runtime-alive-p runtime) nil
            (appkit-source-runtime-pending-starts runtime) nil)
      (appkit--run-cleanup-items
       instances #'appkit-source--revoke
       (lambda (condition) (push condition conditions)))
      (setf (appkit-source-runtime-ready-head runtime) nil
            (appkit-source-runtime-ready-tail runtime) nil
            (appkit-source-runtime-ready-count runtime) 0
            (appkit-source-runtime-orphan-head runtime) nil
            (appkit-source-runtime-orphan-tail runtime) nil
            (appkit-source-runtime-wake-pending-p runtime) nil)
      (setq conditions (nreverse conditions))
      (appkit--warn-cleanup-conditions (cdr conditions) 'appkit-source)
      (when-let* ((condition (car conditions)))
        (signal (car condition) (cdr condition)))
      t)))

(provide 'appkit-source)

;;; appkit-source.el ends here
