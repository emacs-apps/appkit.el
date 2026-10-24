;;; appkit-operation.el --- Keyed App reads and write lanes -*- lexical-binding: t; -*-

;;; Commentary:

;; An App keeps a table of operation entries keyed by request.  A read is
;; shared: requests join one pending fetch and reuse ready data.  A write owns
;; its key as a lane: another write is refused, or coalesced into one queued
;; follow-up.  A request may run as several steps.  Each fetch is a keyed
;; Effect, so a replacement revokes the old result without tokens.
;;
;; Results commit in the App's update.  The App publishes every changed key on
;; a topic and replies to presenters, which run in their requesting Surface's
;; update and return its transition.
;;
;; An entry is a plist: :pending, :ready, :value, :error, :date, :request,
;; :observation, :completed, :total, :remaining, :waiters and :queued.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-context)
(require 'appkit-core)
(require 'appkit-command)
(require 'appkit-effect)

(declare-function appkit-app-send "appkit-app" (app message))
(declare-function appkit-surface-send "appkit-surface" (surface message))

(defmacro appkit-define-operation (registry name arguments &rest options)
  "Define operation NAME taking ARGUMENTS in hash table REGISTRY.

OPTIONS are forms evaluated with ARGUMENTS bound:
:fetch starts one step with `refresh', `resolve' and `reject' also bound and
  returns a function that cancels it, or nil.
:commit validates and merges `result' in App update, with `observation' bound
  to what :prepare returned when the request was accepted; it returns the
  entry's value.
:key names the entry (default (NAME . ARGUMENTS)); :steps lists the requests
  run in turn (default the request itself).
:write makes the key a lane; :coalesce is a function of the new and queued
  requests that returns non-nil to queue the new one instead of refusing it.
:invalidate and :cancel list key prefixes, with `value' bound, to forget or to
  stop after commit.  :next lists follow-up reads, with `value' and `reason'
  bound, after the request settles."
  (declare (indent 3))
  (cl-flet ((fn (extra form)
              `(lambda (,@extra ,@arguments)
                 (ignore ,@extra ,@arguments)
                 ,form)))
    `(puthash ',name
              (list :write ,(plist-get options :write)
                    :coalesce ,(plist-get options :coalesce)
                    :key ,(fn nil (or (plist-get options :key) `(list ',name ,@arguments)))
                    :steps ,(fn nil (or (plist-get options :steps)
                                        `(list (list ',name ,@arguments))))
                    :fetch ,(fn '(refresh resolve reject) (plist-get options :fetch))
                    :prepare ,(fn nil (plist-get options :prepare))
                    :commit ,(fn '(result observation) (or (plist-get options :commit) 'result))
                    :invalidate ,(fn '(value) (plist-get options :invalidate))
                    :cancel ,(fn '(value) (plist-get options :cancel))
                    :next ,(fn '(value reason) (plist-get options :next)))
              ,registry)))

(defun appkit-operation--spec (registry request)
  "Return REQUEST's spec in REGISTRY, or signal."
  (or (gethash (car request) registry)
      (error "Unknown operation %S" (car request))))

(defun appkit-operation-key (registry request)
  "Return the entry key REGISTRY assigns to REQUEST."
  (apply (plist-get (appkit-operation--spec registry request) :key) (cdr request)))

(defvar appkit-operation--building nil
  "(OWNER . ENTRIES) while an operation update builds OWNER's next ENTRIES.")

(defun appkit-operation-lookup (owner entries key)
  "Return KEY's entry for OWNER from committed ENTRIES.
Inside OWNER's own operation update, read the entries being built instead, so
:next and :commit see the settlement they are part of."
  (gethash key (if (and owner (equal owner (car appkit-operation--building)))
                   (cdr appkit-operation--building)
                 entries)))

(defun appkit-operation-submit (registry entries target request &optional refresh present)
  "Send REQUEST to the App at TARGET, an App or, with PRESENT, a Surface.

ENTRIES are the App's committed entries, used to refuse a busy write lane at
once.  REFRESH t replaces a read; `join' refreshes unless one is pending; nil
reuses ready data.  PRESENT is called in the Surface's update as (PRESENT
CONTEXT MODEL REQUEST VALUE REASON) and returns its transition."
  (let* ((spec (appkit-operation--spec registry request))
         (entry (gethash (appkit-operation-key registry request) entries)))
    (when (and (plist-get spec :write) (not (plist-get spec :coalesce))
               (plist-get entry :pending))
      (user-error "Operation already in progress")))
  (let ((ticket
         (if present
             (appkit-surface-send target (list 'appkit-operation-send request refresh present))
           (appkit-app-send target (list 'appkit-operation request refresh nil)))))
    ;; A send from inside a pass is dropped; say so instead of losing it.
    (when (eq ticket 'reentrant-send)
      (error "Operation %S was requested from inside a pass" (car request)))
    ticket))

(defun appkit-operation-surface-update (context model message)
  "Return the Surface transition for operation MESSAGE, or nil."
  (pcase message
    (`(appkit-operation-send ,request ,refresh ,present)
     (appkit-next
      :model model :render appkit-render-none
      :commands (list (appkit-command-post-message
                       :target (appkit-transition-context-parent-address context)
                       :message (list 'appkit-operation request refresh present)
                       :reply-correlation (make-symbol "appkit-operation")
                       :delivery 'report))))
    (`(appkit-operation-present ,presenter ,request ,value ,reason)
     (funcall presenter context model request value reason))))

(defun appkit-operation--effect (registry call request key refresh step)
  "Return the keyed Effect fetching STEP of REQUEST under KEY."
  (let ((spec (appkit-operation--spec registry request)))
    (appkit-effect-create
     :key (cons 'appkit-operation key)
     :input (list request key step)
     :start
     (lambda (_context _input _observe resolve reject)
       (let ((cancel
              (condition-case condition
                  (funcall call (lambda ()
                                  (apply (plist-get spec :fetch)
                                         refresh resolve reject (cdr step))))
                ((error quit) (funcall reject nil (error-message-string condition)) nil))))
         (appkit-cancellation-create
          :kind 'logical :cancel (lambda () (when (functionp cancel) (funcall cancel))))))
     :success (lambda (input raw) (list 'appkit-operation-result input raw nil))
     :failure (lambda (input _body reason) (list 'appkit-operation-result input nil reason)))))

(defun appkit-operation--replies (waiters value reason)
  "Return commands presenting VALUE or REASON for each WAITER's own request."
  (mapcar (lambda (waiter)
            (appkit-command-post-message
             :target (car waiter)
             :message (list 'appkit-operation-present (cadr waiter) (nth 2 waiter) value reason)
             :delivery 'report))
          (delq nil waiters)))

(defun appkit-operation--forget (entries prefix topic &optional keep)
  "Revoke ENTRIES under key PREFIX; KEEP their values when non-nil.
Return the commands that cancel, publish and reply."
  (let (commands)
    (dolist (key (hash-table-keys entries))
      (when (equal prefix (seq-take key (length prefix)))
        (let ((entry (gethash key entries)))
          (setq commands
                (append (appkit-operation--replies
                         (plist-get entry :waiters) nil "Data invalidated")
                        (list (appkit-command-cancel-effect (cons 'appkit-operation key))
                              (appkit-command-publish topic key))
                        commands))
          (if keep
              (puthash key (plist-put (plist-put (copy-sequence entry) :pending nil)
                                      :waiters nil)
                       entries)
            (remhash key entries)))))
    commands))

(cl-defun appkit-operation-update
    (registry entries context message &key owner (call #'funcall) (topic 'appkit-operation))
  "Return (NEW-ENTRIES . COMMANDS) for operation MESSAGE, or nil for others.

ENTRIES are left untouched.  OWNER identifies whose entries these are for
`appkit-operation-lookup'.  CALL runs spec functions, for example in an
account's context.  Changed keys are published on TOPIC."
  (when (memq (car-safe message)
              '(appkit-operation appkit-operation-result appkit-operation-forget))
    (let* ((entries (copy-hash-table entries))
           (appkit-operation--building (cons owner entries))
           (self (appkit-transition-context-owner-address context))
           commands)
      (pcase message
        (`(appkit-operation-forget ,prefix)
         (setq commands (appkit-operation--forget entries prefix topic)))
        (`(appkit-operation ,request ,refresh ,present)
         (let* ((spec (appkit-operation--spec registry request))
                (key (apply (plist-get spec :key) (cdr request)))
                (entry (gethash key entries))
                (waiter (and present (list (appkit-transition-context-reply-route context)
                                           present request)))
                (write (plist-get spec :write)))
           (cond
            ((and (plist-get entry :pending) (or write (not (eq refresh t))))
             (cond
              ((not write)
               (when waiter
                 (puthash key (plist-put (copy-sequence entry) :waiters
                                         (cons waiter (plist-get entry :waiters)))
                          entries)))
              ((and (plist-get spec :coalesce)
                    (funcall call (lambda ()
                                    (funcall (plist-get spec :coalesce) request
                                             (or (car (plist-get entry :queued))
                                                 (plist-get entry :request))))))
               (puthash key (plist-put (copy-sequence entry) :queued (list request waiter))
                        entries))
              (t
               (setq commands (appkit-operation--replies
                               (list waiter) nil "Operation already in progress")))))
            ((and (not write) (not refresh) (plist-get entry :ready))
             (setq commands (appkit-operation--replies
                             (list waiter) (plist-get entry :value) nil)))
            (t
             (let ((steps (apply (plist-get spec :steps) (cdr request))))
               (unless steps (error "Operation %S has no steps" (car request)))
               (setq commands (appkit-operation--replies
                               (plist-get entry :waiters) nil "Request superseded"))
               (puthash key
                        (list :pending t :request request :error nil
                              :completed 0 :total (length steps) :remaining (cdr steps)
                              :waiters (and waiter (list waiter))
                              :observation (funcall call (lambda ()
                                                           (apply (plist-get spec :prepare)
                                                                  (cdr request))))
                              :ready (plist-get entry :ready) :value (plist-get entry :value)
                              :date (plist-get entry :date))
                        entries)
               (setq commands
                     (append commands
                             (list (appkit-command-start-effect
                                    (appkit-operation--effect
                                     registry call request key refresh (car steps)))
                                   (appkit-command-publish topic key)))))))))
        (`(appkit-operation-result (,request ,key ,step) ,raw ,reason)
         (let ((entry (gethash key entries))
               (spec (appkit-operation--spec registry request))
               value)
           (unless reason
             (condition-case condition
                 (funcall
                  call
                  (lambda ()
                    (setq value (apply (plist-get spec :commit)
                                       raw (plist-get entry :observation) (cdr step)))
                    (dolist (prefix (apply (plist-get spec :cancel) value (cdr request)))
                      (setq commands (append commands
                                             (appkit-operation--forget entries prefix topic t))))
                    (dolist (prefix (apply (plist-get spec :invalidate) value (cdr request)))
                      (setq commands (append commands
                                             (appkit-operation--forget entries prefix topic))))))
               (error (setq reason (error-message-string condition)))))
           (let ((remaining (and (not reason) (plist-get entry :remaining))))
             (puthash key
                      (list :pending (and remaining t) :request request
                            :ready (or (not reason) (plist-get entry :ready))
                            :completed (+ (or (plist-get entry :completed) 0) (if reason 0 1))
                            :total (plist-get entry :total)
                            :queued (and remaining (plist-get entry :queued))
                            :remaining (cdr remaining)
                            :waiters (and remaining (plist-get entry :waiters))
                            :observation (plist-get entry :observation)
                            :error reason
                            :value (if reason (plist-get entry :value) value)
                            :date (if reason (plist-get entry :date)
                                    (format-time-string "%Y-%m-%d")))
                      entries)
             (when (and reason (plist-get spec :write)) (message "%s" reason))
             (setq commands
                   (append
                    commands
                    (list (appkit-command-publish topic key))
                    (if remaining
                        (list (appkit-command-start-effect
                               (appkit-operation--effect
                                registry call request key nil (car remaining))))
                      (append
                       (appkit-operation--replies (plist-get entry :waiters) value reason)
                       (when-let* ((queued (plist-get entry :queued)))
                         (list (appkit-command-post-message
                                :target self :delivery 'report
                                :message (list 'appkit-operation (car queued) t nil))))
                       (mapcar (lambda (next)
                                 (appkit-command-post-message
                                  :target self :delivery 'report
                                  :message (list 'appkit-operation next 'join nil)))
                               (funcall call (lambda ()
                                               (apply (plist-get spec :next)
                                                      value reason (cdr request)))))))))))))
      (cons entries commands))))

(provide 'appkit-operation)

;;; appkit-operation.el ends here
