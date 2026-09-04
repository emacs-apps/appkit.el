;;; appkit-resource.el --- Managed presentation resource companion  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;;; Commentary:

;; App-scoped logical presentation resources over a bounded process-wide
;; acquisition broker.  Renderer demands are committed only after successful
;; buffer presentation.  Completion uses internal control envelopes and never
;; invokes client update or advances the domain revision.

;;; Code:

(require 'cl-lib)
(require 'appkit-cleanup)
(require 'appkit-effect)
(require 'appkit-loop)

(declare-function appkit-app-identity "appkit-app")
(declare-function appkit-app-live-p "appkit-app")
(declare-function appkit-app-loop "appkit-app")
(declare-function appkit-app-resource-coordinator "appkit-app")
(declare-function appkit-surface-app "appkit-surface")
(declare-function appkit-surface-live-p "appkit-surface")
(declare-function appkit-surface-loop "appkit-surface")
(declare-function appkit-surface-renderer "appkit-surface")
(declare-function appkit-generated-renderer-resource-request "appkit-surface")

(cl-defstruct (appkit-resource-demand
               (:constructor appkit-resource-demand-create)
               (:copier nil))
  "One owned presentation-resource acquisition demand."
  key input loader acquisition-identity sharing-policy cache-policy)

(cl-defstruct (appkit-resource-interest
               (:constructor appkit-resource-interest-create)
               (:copier nil))
  "One resource KEY retained by projected ROW-KEYS."
  key row-keys)

(cl-defstruct (appkit-resource-interest-update
               (:constructor appkit-resource-interest-update-create)
               (:copier nil))
  "A Renderer interest update whose MODE is `unchanged' or `replace'."
  mode entries)

(cl-defstruct (appkit-render-result
               (:constructor appkit-render-result-create)
               (:copier nil))
  "Closed companion result returned by a successful Renderer phase."
  resource-demands resource-interest-update)

(cl-defstruct (appkit-resource-state
               (:constructor appkit-resource--state-create)
               (:copier nil))
  "Stable presentation state for one logical resource."
  status value reason)

(cl-defstruct (appkit-resource--entry
               (:constructor appkit-resource--entry-create)
               (:copier nil))
  coordinator key input demand state acquisition token)

(cl-defstruct (appkit-resource--coordinator
               (:constructor appkit-resource--coordinator-create-internal)
               (:copier nil))
  app entries interests max-entries max-interests alive-p)

(cl-defstruct (appkit-resource--acquisition
               (:constructor appkit-resource--acquisition-create)
               (:copier nil))
  broker identity input loader token state leases cancellation queued-p
  value reason)

(cl-defstruct (appkit-resource--broker
               (:constructor appkit-resource--broker-create)
               (:copier nil))
  acquisitions active-count max-active queue-head queue-tail queue-count
  max-queued)

(cl-defstruct (appkit-resource--coordinator-delivery
               (:constructor appkit-resource--coordinator-delivery-create)
               (:copier nil))
  coordinator entry token status payload)

(cl-defstruct (appkit-resource--surface-delivery
               (:constructor appkit-resource--surface-delivery-create)
               (:copier nil))
  coordinator surface incarnation keys)

(defconst appkit-resource-default-per-render-limit 32)
(defconst appkit-resource-default-entry-limit 256)
(defconst appkit-resource-default-interest-limit 512)
(defconst appkit-resource-default-broker-active-limit 32)
(defconst appkit-resource-default-broker-queue-limit 128)

(defvar appkit-resource--broker
  (appkit-resource--broker-create
   :acquisitions (make-hash-table :test #'equal)
   :active-count 0
   :max-active appkit-resource-default-broker-active-limit
   :queue-count 0
   :max-queued appkit-resource-default-broker-queue-limit)
  "Process-wide bounded physical acquisition broker.")

(defun appkit-resource--proper-bounded-list-p (value limit)
  "Return non-nil when VALUE is a proper list no longer than LIMIT."
  (let ((remaining value) (count 0))
    (while (and (consp remaining) (<= count limit))
      (setq remaining (cdr remaining)
            count (1+ count)))
    (and (null remaining) (<= count limit))))

(defun appkit-resource--validate-demand (demand)
  "Return validated resource DEMAND."
  (unless (appkit-resource-demand-p demand)
    (signal 'wrong-type-argument (list 'appkit-resource-demand-p demand)))
  (unless (appkit-resource-demand-key demand)
    (error "Resource demand key must be non-nil"))
  (unless (functionp (appkit-resource-demand-loader demand))
    (error "Resource demand loader is not callable"))
  (unless (appkit-resource-demand-acquisition-identity demand)
    (error "Resource acquisition identity must be non-nil"))
  (unless (memq (or (appkit-resource-demand-sharing-policy demand)
                    'app-private)
                '(app-private shared))
    (error "Unsupported resource sharing policy: %S"
           (appkit-resource-demand-sharing-policy demand)))
  (unless (memq (or (appkit-resource-demand-cache-policy demand) 'retain)
                '(retain while-interested))
    (error "Unsupported resource cache policy: %S"
           (appkit-resource-demand-cache-policy demand)))
  demand)

(defun appkit-resource-coordinator-create (app &optional entry-limit interest-limit)
  "Create APP's logical resource coordinator."
  (appkit-resource--coordinator-create-internal
   :app app
   :entries (make-hash-table :test #'equal)
   :interests (make-hash-table :test #'eq)
   :max-entries (or entry-limit appkit-resource-default-entry-limit)
   :max-interests (or interest-limit appkit-resource-default-interest-limit)
   :alive-p t))

(defun appkit-resource--broker-key (coordinator demand)
  "Return physical broker identity for COORDINATOR and DEMAND."
  (pcase (or (appkit-resource-demand-sharing-policy demand) 'app-private)
    ('shared (list 'shared
                   (appkit-resource-demand-acquisition-identity demand)))
    ('app-private
     (list 'app-private
           (appkit-app-identity
            (appkit-resource--coordinator-app coordinator))
           (appkit-resource-demand-acquisition-identity demand)))))

(defun appkit-resource--entry-current-p (entry)
  "Return non-nil when ENTRY still owns its coordinator key and token."
  (let ((coordinator (appkit-resource--entry-coordinator entry)))
    (and (appkit-resource--coordinator-alive-p coordinator)
         (eq entry
             (gethash (appkit-resource--entry-key entry)
                      (appkit-resource--coordinator-entries coordinator))))))

(defun appkit-resource--post-coordinator (entry status payload)
  "Post ENTRY completion STATUS and PAYLOAD to its App coordinator."
  (when (appkit-resource--entry-current-p entry)
    (let* ((coordinator (appkit-resource--entry-coordinator entry))
           (app (appkit-resource--coordinator-app coordinator))
           (loop (appkit-app-loop app))
           (outcome
            (appkit-loop--post-control-addressed
             loop
             (appkit-resource--coordinator-delivery-create
              :coordinator coordinator
              :entry entry
              :token (appkit-resource--entry-token entry)
              :status status
              :payload payload)
             (appkit-loop-incarnation loop))))
      (when (eq outcome 'full)
        (appkit-loop--enter-fault
         loop '(error "Resource coordinator completion lane is full") nil)))))

(defun appkit-resource--broker-remove-queued (acquisition)
  "Remove queued ACQUISITION from its broker."
  (when (appkit-resource--acquisition-queued-p acquisition)
    (let* ((broker (appkit-resource--acquisition-broker acquisition))
           (queue (appkit-resource--broker-queue-head broker)))
      (setq queue (delq acquisition queue))
      (setf (appkit-resource--broker-queue-head broker) queue
            (appkit-resource--broker-queue-tail broker) (last queue)
            (appkit-resource--broker-queue-count broker) (length queue)
            (appkit-resource--acquisition-queued-p acquisition) nil))))

(defun appkit-resource--broker-retire (acquisition cancel-p)
  "Retire ACQUISITION, release its slot, and continue queued work."
  (let ((broker (appkit-resource--acquisition-broker acquisition)))
    (appkit-resource--broker-remove-queued acquisition)
    (when (eq (appkit-resource--acquisition-state acquisition) 'active)
      (setf (appkit-resource--broker-active-count broker)
            (1- (appkit-resource--broker-active-count broker))))
    (setf (appkit-resource--acquisition-state acquisition) 'retired)
    (unwind-protect
        (when (and cancel-p
                   (appkit-resource--acquisition-cancellation acquisition))
          (let ((capability
                 (appkit-resource--acquisition-cancellation acquisition)))
            (setf (appkit-resource--acquisition-cancellation acquisition) nil)
            (when-let* ((cancel (appkit-cancellation-cancel capability)))
              (funcall cancel))))
      (remhash (appkit-resource--acquisition-identity acquisition)
               (appkit-resource--broker-acquisitions broker))
      (appkit-resource--broker-start-next broker))))

(defun appkit-resource--broker-start-next (broker)
  "Start queued broker work while BROKER has capacity."
  (while (and (< (appkit-resource--broker-active-count broker)
                 (appkit-resource--broker-max-active broker))
              (appkit-resource--broker-queue-head broker))
    (let* ((acquisition (car (appkit-resource--broker-queue-head broker)))
           (rest (cdr (appkit-resource--broker-queue-head broker))))
      (setf (appkit-resource--broker-queue-head broker) rest
            (appkit-resource--broker-queue-tail broker) (last rest)
            (appkit-resource--broker-queue-count broker)
            (1- (appkit-resource--broker-queue-count broker))
            (appkit-resource--acquisition-queued-p acquisition) nil)
      (appkit-resource--broker-start acquisition))))

(defun appkit-resource--broker-settle (acquisition token status payload)
  "Settle ACQUISITION identified by TOKEN with STATUS and PAYLOAD."
  (when (and (eq token (appkit-resource--acquisition-token acquisition))
             (eq (appkit-resource--acquisition-state acquisition) 'active))
    (let ((broker (appkit-resource--acquisition-broker acquisition)))
      (setf (appkit-resource--acquisition-state acquisition) status
            (appkit-resource--acquisition-value acquisition)
            (and (eq status 'ready) payload)
            (appkit-resource--acquisition-reason acquisition)
            (and (eq status 'failed) payload)
            (appkit-resource--acquisition-cancellation acquisition) nil
            (appkit-resource--broker-active-count broker)
            (1- (appkit-resource--broker-active-count broker)))
      (dolist (entry (copy-sequence
                      (appkit-resource--acquisition-leases acquisition)))
        (appkit-resource--post-coordinator entry status payload))
      (appkit-resource--broker-start-next broker)
      t)))
(defun appkit-resource--broker-fault (acquisition condition)
  "Retire ACQUISITION and route invariant CONDITION through each App gate."
  (let ((entries
         (copy-sequence (appkit-resource--acquisition-leases acquisition))))
    (appkit-resource--broker-retire acquisition t)
    (dolist (entry entries)
      (appkit-resource--post-coordinator entry 'fault condition))))

(defun appkit-resource--broker-start (acquisition)
  "Start physical ACQUISITION and validate its cancellation capability."
  (let* ((broker (appkit-resource--acquisition-broker acquisition))
         (token (appkit-resource--acquisition-token acquisition))
         capability completed-p condition)
    (setf (appkit-resource--acquisition-state acquisition) 'active
          (appkit-resource--broker-active-count broker)
          (1+ (appkit-resource--broker-active-count broker)))
    (unwind-protect
        (condition-case err
            (progn
              (setq capability
                    (funcall
                     (appkit-resource--acquisition-loader acquisition)
                     (list 'appkit-resource-context token)
                     (appkit-resource--acquisition-input acquisition)
                     (lambda (value)
                       (appkit-resource--broker-settle
                        acquisition token 'ready value))
                     (lambda (reason)
                       (appkit-resource--broker-settle
                        acquisition token 'failed reason)))
                    completed-p t))
          ((error quit)
           (setq condition err completed-p t)))
      (unless completed-p
        (appkit-resource--broker-retire acquisition t)))
    (when condition
      (appkit-resource--broker-fault acquisition condition))
    (unless condition
      (cond
       ((memq (appkit-resource--acquisition-state acquisition) '(ready failed))
        (when (appkit-cancellation-p capability)
          (when-let* ((cancel (appkit-cancellation-cancel capability)))
            (funcall cancel))))
       ((not (and (appkit-cancellation-p capability)
                  (functionp (appkit-cancellation-cancel capability))))
        (appkit-resource--broker-fault
         acquisition
         '(error "Resource loader returned invalid cancellation capability")))
       (t
        (setf (appkit-resource--acquisition-cancellation acquisition)
              capability))))))

(defun appkit-resource--broker-acquire (entry)
  "Acquire or reuse physical work for logical ENTRY."
  (let* ((coordinator (appkit-resource--entry-coordinator entry))
         (demand (appkit-resource--entry-demand entry))
         (broker appkit-resource--broker)
         (identity (appkit-resource--broker-key coordinator demand))
         (existing
          (gethash identity
                   (appkit-resource--broker-acquisitions broker))))
    (if existing
        (if (not
             (and (equal (appkit-resource--acquisition-input existing)
                         (appkit-resource-demand-input demand))
                  (eq (appkit-resource--acquisition-loader existing)
                      (appkit-resource-demand-loader demand))))
            (appkit-resource--post-coordinator
             entry 'fault
             (list 'error
                   (format "Resource acquisition identity conflict: %S"
                           identity)))
          (push entry (appkit-resource--acquisition-leases existing))
          (setf (appkit-resource--entry-acquisition entry) existing)
          (when (memq (appkit-resource--acquisition-state existing)
                      '(ready failed))
            (appkit-resource--post-coordinator
             entry
             (appkit-resource--acquisition-state existing)
             (if (eq (appkit-resource--acquisition-state existing) 'ready)
                 (appkit-resource--acquisition-value existing)
               (appkit-resource--acquisition-reason existing)))))
      (let ((acquisition
             (appkit-resource--acquisition-create
              :broker broker :identity identity
              :input (appkit-resource-demand-input demand)
              :loader (appkit-resource-demand-loader demand)
              :token (make-symbol "appkit-resource-acquisition-")
              :state 'new :leases (list entry))))
        (puthash identity acquisition
                 (appkit-resource--broker-acquisitions broker))
        (setf (appkit-resource--entry-acquisition entry) acquisition)
        (cond
         ((< (appkit-resource--broker-active-count broker)
             (appkit-resource--broker-max-active broker))
          (appkit-resource--broker-start acquisition))
         ((< (appkit-resource--broker-queue-count broker)
             (appkit-resource--broker-max-queued broker))
          (let ((cell (list acquisition)))
            (if (appkit-resource--broker-queue-tail broker)
                (setcdr (appkit-resource--broker-queue-tail broker) cell)
              (setf (appkit-resource--broker-queue-head broker) cell))
            (setf (appkit-resource--broker-queue-tail broker) cell
                  (appkit-resource--broker-queue-count broker)
                  (1+ (appkit-resource--broker-queue-count broker))
                  (appkit-resource--acquisition-queued-p acquisition) t)))
         (t
          (appkit-resource--broker-retire acquisition nil)
          (appkit-resource--post-coordinator entry 'failed 'capacity)))))))

(defun appkit-resource--release-entry (entry)
  "Release logical ENTRY and its physical acquisition lease."
  (when-let* ((acquisition (appkit-resource--entry-acquisition entry)))
    (setf (appkit-resource--acquisition-leases acquisition)
          (delq entry (appkit-resource--acquisition-leases acquisition))
          (appkit-resource--entry-acquisition entry) nil)
    (unless (appkit-resource--acquisition-leases acquisition)
      (appkit-resource--broker-retire acquisition t))))

(defun appkit-resource--surface-interests (coordinator surface)
  "Return SURFACE's interest table in COORDINATOR, or nil."
  (gethash surface (appkit-resource--coordinator-interests coordinator)))

(defun appkit-resource--interested-surfaces (entry)
  "Return live Surfaces currently interested in ENTRY."
  (let ((coordinator (appkit-resource--entry-coordinator entry)) surfaces)
    (maphash
     (lambda (surface table)
       (when (and (gethash (appkit-resource--entry-key entry) table)
                  (appkit-surface-live-p surface))
         (push surface surfaces)))
     (appkit-resource--coordinator-interests coordinator))
    surfaces))

(defun appkit-resource--notify-surface (coordinator surface keys)
  "Post resource KEYS to interested SURFACE."
  (when (appkit-surface-live-p surface)
    (let* ((loop (appkit-surface-loop surface))
           (incarnation (appkit-loop-incarnation loop))
           (outcome
            (appkit-loop--post-control-addressed
             loop
             (appkit-resource--surface-delivery-create
              :coordinator coordinator :surface surface
              :incarnation incarnation :keys keys)
             incarnation)))
      (when (eq outcome 'full)
        (appkit-loop--enter-fault
         loop '(error "Resource Surface delivery lane is full") nil)))))

(defun appkit-resource-consume-coordinator-delivery (app delivery)
  "Commit App companion DELIVERY and notify interested Surfaces."
  (let* ((coordinator (appkit-resource--coordinator-delivery-coordinator delivery))
         (entry (appkit-resource--coordinator-delivery-entry delivery)))
    (unless (and (eq coordinator (appkit-app-resource-coordinator app))
                 (eq coordinator (appkit-resource--entry-coordinator entry))
                 (eq (appkit-resource--coordinator-delivery-token delivery)
                     (appkit-resource--entry-token entry))
                 (appkit-resource--entry-current-p entry))
      (cl-return-from appkit-resource-consume-coordinator-delivery nil))
    (when (eq (appkit-resource--coordinator-delivery-status delivery) 'fault)
      (let ((condition (appkit-resource--coordinator-delivery-payload delivery)))
        (if (consp condition)
            (signal (car condition) (cdr condition))
          (error "Invalid Resource coordinator fault: %S" condition))))
    (let ((status (appkit-resource--coordinator-delivery-status delivery))
          (payload (appkit-resource--coordinator-delivery-payload delivery)))
      (setf (appkit-resource--entry-state entry)
            (if (eq status 'ready)
                (appkit-resource--state-create
                 :status 'ready :value payload :reason nil)
              (appkit-resource--state-create
               :status 'failed :value nil :reason payload)))
      (dolist (surface (appkit-resource--interested-surfaces entry))
        (appkit-resource--notify-surface
         coordinator surface (list (appkit-resource--entry-key entry))))
      t)))

(defun appkit-resource-consume-surface-delivery (surface delivery)
  "Return resource-only Renderer request for current SURFACE DELIVERY."
  (let* ((coordinator (appkit-resource--surface-delivery-coordinator delivery))
         (keys (appkit-resource--surface-delivery-keys delivery))
         (mapper
          (appkit-generated-renderer-resource-request
           (appkit-surface-renderer surface))))
    (when (and (eq surface (appkit-resource--surface-delivery-surface delivery))
               (= (appkit-resource--surface-delivery-incarnation delivery)
                  (appkit-loop-incarnation (appkit-surface-loop surface)))
               (eq coordinator
                   (appkit-app-resource-coordinator
                    (appkit-surface-app surface)))
               (appkit-resource--surface-interests coordinator surface)
               (functionp mapper))
      (funcall mapper keys))))

(defun appkit-resource-state (surface key)
  "Return presentation resource state for SURFACE and KEY, or nil."
  (when (appkit-surface-live-p surface)
    (let* ((app (appkit-surface-app surface))
           (coordinator (and app (appkit-app-resource-coordinator app)))
           (table (and coordinator
                       (appkit-resource--surface-interests
                        coordinator surface)))
           (entry (and table (gethash key table)
                       (gethash key
                                (appkit-resource--coordinator-entries
                                 coordinator)))))
      (and entry (appkit-resource--entry-state entry)))))

(defun appkit-resource--validate-interest-update (update)
  "Return validated resource interest UPDATE."
  (unless (appkit-resource-interest-update-p update)
    (signal 'wrong-type-argument
            (list 'appkit-resource-interest-update-p update)))
  (unless (memq (appkit-resource-interest-update-mode update)
                '(unchanged replace))
    (error "Invalid resource interest mode: %S"
           (appkit-resource-interest-update-mode update)))
  (when (eq (appkit-resource-interest-update-mode update) 'unchanged)
    (when (appkit-resource-interest-update-entries update)
      (error "Unchanged resource interest cannot carry entries")))
  update)

(defun appkit-resource--prepare-interest-table (coordinator surface entries)
  "Validate SURFACE ENTRIES and return their unique resource-key table."
  (unless (appkit-resource--proper-bounded-list-p
           entries (appkit-resource--coordinator-max-interests coordinator))
    (error "Surface resource interest limit exceeded"))
  (let ((table (make-hash-table :test #'equal)))
    (dolist (interest entries table)
      (unless (and (appkit-resource-interest-p interest)
                   (appkit-resource-interest-key interest)
                   (appkit-resource--proper-bounded-list-p
                    (appkit-resource-interest-row-keys interest)
                    (appkit-resource--coordinator-max-interests coordinator))
                   (appkit-resource-interest-row-keys interest))
        (error "Invalid resource interest: %S" interest))
      (when (gethash (appkit-resource-interest-key interest) table)
        (error "Duplicate resource interest key: %S"
               (appkit-resource-interest-key interest)))
      (puthash (appkit-resource-interest-key interest)
               (copy-sequence (appkit-resource-interest-row-keys interest))
               table))
    (let ((total (hash-table-count table)))
      (maphash
       (lambda (owner owner-table)
         (unless (eq owner surface)
           (setq total (+ total (hash-table-count owner-table)))))
       (appkit-resource--coordinator-interests coordinator))
      (when (> total (appkit-resource--coordinator-max-interests coordinator))
        (error "App resource interest limit exceeded")))
    table))

(defun appkit-resource--replace-interests (coordinator surface table)
  "Replace SURFACE interests in COORDINATOR with validated TABLE."
  (if (= (hash-table-count table) 0)
      (remhash surface (appkit-resource--coordinator-interests coordinator))
    (puthash surface table
             (appkit-resource--coordinator-interests coordinator)))
  table)

(defun appkit-resource--key-interested-p (coordinator key)
  "Return non-nil when any Surface is interested in KEY."
  (let (found)
    (maphash (lambda (_surface table)
               (when (gethash key table) (setq found t)))
             (appkit-resource--coordinator-interests coordinator))
    found))

(defun appkit-resource--release-entries (entries)
  "Release ENTRIES completely and re-signal the first cleanup failure."
  (let (conditions)
    (appkit--run-cleanup-items
     entries #'appkit-resource--release-entry
     (lambda (condition) (push condition conditions)))
    (setq conditions (nreverse conditions))
    (appkit--warn-cleanup-conditions (cdr conditions) 'appkit-resource)
    (when-let* ((condition (car conditions)))
      (signal (car condition) (cdr condition)))))

(defun appkit-resource--release-uninterested (coordinator)
  "Remove and release every while-interested entry with no interest."
  (let ((entries (appkit-resource--coordinator-entries coordinator))
        releases)
    (maphash
     (lambda (key entry)
       (when (and (eq (or (appkit-resource-demand-cache-policy
                           (appkit-resource--entry-demand entry))
                          'retain)
                      'while-interested)
                  (not (appkit-resource--key-interested-p coordinator key)))
         (remhash key entries)
         (push entry releases)))
     (copy-hash-table entries))
    (appkit-resource--release-entries releases)))

(defun appkit-resource--prepare-demands (coordinator demands interest-table)
  "Validate DEMANDS and INTEREST-TABLE without mutating COORDINATOR."
  (unless (appkit-resource--proper-bounded-list-p
           demands appkit-resource-default-per-render-limit)
    (error "Per-render resource demand limit exceeded"))
  (let ((seen (make-hash-table :test #'equal))
        (entries (appkit-resource--coordinator-entries coordinator))
        (new-count 0))
    (dolist (demand demands)
      (setq demand (appkit-resource--validate-demand demand))
      (let* ((key (appkit-resource-demand-key demand))
             (prior (gethash key seen))
             (existing (gethash key entries)))
        (when prior
          (error "Duplicate resource demand key: %S" key))
        (puthash key demand seen)
        (if existing
            (when
                (and
                 (equal (appkit-resource--entry-input existing)
                        (appkit-resource-demand-input demand))
                 (not
                  (and
                   (eq (appkit-resource-demand-loader
                        (appkit-resource--entry-demand existing))
                       (appkit-resource-demand-loader demand))
                   (equal
                    (appkit-resource-demand-acquisition-identity
                     (appkit-resource--entry-demand existing))
                    (appkit-resource-demand-acquisition-identity demand))
                   (eq (or (appkit-resource-demand-sharing-policy
                            (appkit-resource--entry-demand existing))
                           'app-private)
                       (or (appkit-resource-demand-sharing-policy demand)
                           'app-private))
                   (eq (or (appkit-resource-demand-cache-policy
                            (appkit-resource--entry-demand existing))
                           'retain)
                       (or (appkit-resource-demand-cache-policy demand)
                           'retain)))))
              (error "Resource demand contract changed for key %S" key))
          (setq new-count (1+ new-count)))))
    (when (> (+ (hash-table-count entries) new-count)
             (appkit-resource--coordinator-max-entries coordinator))
      (error "App resource entry limit exceeded"))
    (when interest-table
      (maphash
       (lambda (key _row-keys)
         (unless (or (gethash key seen) (gethash key entries))
           (error "Resource interest has no demand or retained entry: %S"
                  key)))
       interest-table))
    demands))

(defun appkit-resource--install-demand (coordinator demand)
  "Install or reuse logical resource DEMAND in COORDINATOR."
  (setq demand (appkit-resource--validate-demand demand))
  (let* ((key (appkit-resource-demand-key demand))
         (entries (appkit-resource--coordinator-entries coordinator))
         (existing (gethash key entries)))
    (when (and existing
               (not (equal (appkit-resource--entry-input existing)
                           (appkit-resource-demand-input demand))))
      (remhash key entries)
      (appkit-resource--release-entry existing)
      (setq existing nil))
    (or existing
        (progn
          (when (>= (hash-table-count entries)
                    (appkit-resource--coordinator-max-entries coordinator))
            (error "App resource entry limit exceeded"))
          (let ((entry
                 (appkit-resource--entry-create
                  :coordinator coordinator :key key
                  :input (appkit-resource-demand-input demand)
                  :demand demand
                  :state (appkit-resource--state-create :status 'pending)
                  :token (make-symbol "appkit-resource-entry-"))))
            (puthash key entry entries)
            (appkit-resource--broker-acquire entry)
            entry)))))

(defun appkit-resource-commit-render-result (surface result)
  "Commit successful Renderer companion RESULT for SURFACE."
  (when result
    (unless (appkit-render-result-p result)
      (error "Renderer returned invalid companion result: %S" result))
    (let* ((app (appkit-surface-app surface))
           (coordinator (and app (appkit-app-resource-coordinator app)))
           (demands (appkit-render-result-resource-demands result))
           (update (appkit-render-result-resource-interest-update result))
           interest-table)
      (unless coordinator
        (error "Resource demands require an App-owned Surface"))
      (when update
        (setq update (appkit-resource--validate-interest-update update))
        (when (eq (appkit-resource-interest-update-mode update) 'replace)
          (setq interest-table
                (appkit-resource--prepare-interest-table
                 coordinator surface
                 (appkit-resource-interest-update-entries update)))))
      (setq demands
            (appkit-resource--prepare-demands
             coordinator demands interest-table))
      (dolist (demand demands)
        (appkit-resource--install-demand coordinator demand))
      (when interest-table
        (appkit-resource--replace-interests
         coordinator surface interest-table))
      (when update
        (appkit-resource--release-uninterested coordinator))
      t)))

(defun appkit-resource-detach-surface (surface)
  "Remove every presentation interest owned by SURFACE."
  (when-let* ((app (appkit-surface-app surface))
              (coordinator (appkit-app-resource-coordinator app)))
    (remhash surface (appkit-resource--coordinator-interests coordinator))
    (appkit-resource--release-uninterested coordinator)))

(defun appkit-resource-coordinator-stop (coordinator)
  "Stop COORDINATOR and release every logical resource lease."
  (when (and (appkit-resource--coordinator-p coordinator)
             (appkit-resource--coordinator-alive-p coordinator))
    (let (entries)
      (setf (appkit-resource--coordinator-alive-p coordinator) nil)
      (maphash (lambda (_key entry) (push entry entries))
               (appkit-resource--coordinator-entries coordinator))
      (clrhash (appkit-resource--coordinator-entries coordinator))
      (clrhash (appkit-resource--coordinator-interests coordinator))
      (appkit-resource--release-entries entries)
      t)))

(provide 'appkit-resource)

;;; appkit-resource.el ends here
