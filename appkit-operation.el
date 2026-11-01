;;; appkit-operation.el --- Keyed App data operations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Operation arguments become named input values at admission.  Entries are
;; persistent data; Effects retain execution and cancellation authority.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-data)
(require 'appkit-context)
(require 'appkit-core)
(require 'appkit-command)
(require 'appkit-effect)

(declare-function appkit-app-send "appkit-app" (app message))
(declare-function appkit-surface-send "appkit-surface" (surface message))

(cl-defstruct (appkit-operation-request (:constructor appkit-operation--request-create)
                                       (:copier nil))
  (name nil :read-only t) (input nil :read-only t))

(defmacro appkit-define-operation (registry name arguments &rest options)
  "Define NAME's named input fields ARGUMENTS and behavior in REGISTRY.
Spec bodies see ARGUMENTS as locals read from one captured request value.
:fetch also sees refresh/resolve/reject; :commit sees result/observation;
:invalidate and :cancel see value; :next sees value/reason.  :steps returns
requests to run in turn.  :write makes the key a lane and :coalesce decides
whether a new request replaces its queued successor."
  (declare (indent 3))
  (let ((request (make-symbol "request")))
    (cl-flet ((fn (extra form)
                `(lambda (,@extra ,request)
                   (ignore ,request ,@extra)
                   (let ,(mapcar (lambda (arg)
                                   `(,arg (appkit-operation-input ,request
                                             ,(intern (concat ":" (symbol-name arg))))))
                                 arguments)
                     (ignore ,@arguments)
                     ,form))))
      `(puthash ',name
                (list :arguments ',arguments
                      :write ,(plist-get options :write)
                      :coalesce ,(plist-get options :coalesce)
                      :key ,(fn nil (or (plist-get options :key) `(list ',name ,@arguments)))
                      :steps ,(fn nil (or (plist-get options :steps) `(list ,request)))
                      :fetch ,(fn '(refresh resolve reject) (plist-get options :fetch))
                      :prepare ,(fn nil (plist-get options :prepare))
                      :commit ,(fn '(result observation) (or (plist-get options :commit) 'result))
                      :invalidate ,(fn '(value) (plist-get options :invalidate))
                      :cancel ,(fn '(value) (plist-get options :cancel))
                      :next ,(fn '(value reason) (plist-get options :next)))
                ,registry))))

(defun appkit-operation--spec (registry request)
  "Return REQUEST's spec in REGISTRY."
  (let ((name (if (appkit-operation-request-p request)
                  (appkit-operation-request-name request) (car-safe request))))
    (or (gethash name registry) (error "Unknown operation %S" name))))

(defun appkit-operation--check-input (registry name input)
  "Validate named INPUT against NAME's parameter declaration in REGISTRY."
  (let* ((spec (appkit-operation--spec registry (list name)))
         (keys (mapcar (lambda (arg) (intern (concat ":" (symbol-name arg))))
                       (plist-get spec :arguments)))
         (rest input) seen)
    (unless (and (proper-list-p input) (zerop (% (length input) 2)))
      (error "Invalid operation input: %S" input))
    (while rest
      (let ((key (pop rest)))
        (unless (and (memq key keys) (not (memq key seen)))
          (error "Unknown or duplicate operation field %S" key))
        (push key seen) (pop rest)))
    (unless (= (length keys) (length seen)) (error "Missing operation fields for %S" name))))

(defun appkit-operation-request-create (registry name &rest input)
  "Create NAME's request in REGISTRY from named INPUT keyword/value pairs.
All declared fields are required; unknown or duplicate fields signal.
Children are borrowed immutable values, as in other Appkit data records."
  (appkit-operation--check-input registry name input)
  (appkit-operation--request-create :name name :input (copy-sequence input)))

(defun appkit-operation-input (request field)
  "Read named FIELD from REQUEST."
  (appkit-data-get (appkit-operation-request-input request) field))

(defun appkit-operation--request (registry request)
  "Normalize a command list or named REQUEST using REGISTRY's declaration."
  (if (appkit-operation-request-p request)
      (progn (appkit-operation--check-input registry (appkit-operation-request-name request)
                                             (appkit-operation-request-input request))
             request)
    (let ((arguments (plist-get (appkit-operation--spec registry request) :arguments)))
      (unless (and (proper-list-p request) (= (length arguments) (length (cdr request))))
        (error "Wrong operation arguments: %S" request))
      (apply #'appkit-operation-request-create registry (car request)
             (cl-mapcan (lambda (arg value) (list (intern (concat ":" (symbol-name arg))) value))
                        arguments (cdr request))))))

(defun appkit-operation-key (registry request)
  "Return the entry key REGISTRY assigns to REQUEST."
  (funcall (plist-get (appkit-operation--spec registry request) :key)
           (appkit-operation--request registry request)))

(defvar appkit-operation--building nil
  "(OWNER . GETTER) while an update builds OWNER's next persistent entries.")

(defun appkit-operation-lookup (owner entries key)
  "Return KEY's entry for OWNER from ENTRIES, including its current update."
  (appkit-data-map-get
   (if (and owner (equal owner (car appkit-operation--building)))
       (funcall (cdr appkit-operation--building)) entries)
   key))

(defun appkit-operation-submit (registry entries target request &optional refresh reply-token)
  "Send REQUEST to TARGET, an App or, with REPLY-TOKEN, a Surface.
REQUEST is a named request or a command list normalized at App admission.
REFRESH t replaces a read; join refreshes unless pending; nil reuses ready data.
Completion returns (appkit-operation-completed TOKEN REQUEST VALUE REASON)."
  (let* ((spec (appkit-operation--spec registry request))
         (entry (appkit-data-map-get entries (appkit-operation-key registry request))))
    (when (and (plist-get spec :write) (not (plist-get spec :coalesce))
               (plist-get entry :pending))
      (user-error "Operation already in progress")))
  (let ((ticket (if reply-token
                    (appkit-surface-send target (list 'appkit-operation-send request refresh reply-token))
                  (appkit-app-send target (list 'appkit-operation request refresh nil)))))
    (when (eq ticket 'reentrant-send)
      (error "Operation %S was requested from inside a pass" request))
    ticket))

(defun appkit-operation-surface-update (context model message)
  "Forward an operation request to the App, or return nil for other messages."
  (pcase message
    (`(appkit-operation-send ,request ,refresh ,reply-token)
     (appkit-next
      :model model :render appkit-render-none
      :commands (list (appkit-command-post-message
                       :target (appkit-transition-context-parent-address context)
                       :message (list 'appkit-operation request refresh reply-token)
                       :reply-correlation (make-symbol "appkit-operation") :delivery 'report))))))

(defun appkit-operation--effect (registry call request key refresh step)
  "Return the keyed Effect fetching STEP of REQUEST under KEY."
  (let ((spec (appkit-operation--spec registry request)))
    (appkit-effect-create
     :key (cons 'appkit-operation key) :input (list request key step)
     :start
     (lambda (_context _input _observe resolve reject)
       (let ((cancel (condition-case condition
                         (funcall call (lambda ()
                                         (funcall (plist-get spec :fetch) refresh resolve reject step)))
                       ((error quit) (funcall reject nil (error-message-string condition)) nil))))
         (appkit-cancellation-create
          :kind 'logical :cancel (lambda () (when (functionp cancel) (funcall cancel))))))
     :success (lambda (input raw) (list 'appkit-operation-result input raw nil))
     :failure (lambda (input _body reason) (list 'appkit-operation-result input nil reason)))))

(defun appkit-operation--replies (waiters value reason)
  "Return completion commands for each WAITER's request and reply token."
  (mapcar (lambda (waiter)
            (appkit-command-post-message
             :target (car waiter)
             :message (list 'appkit-operation-completed (cadr waiter) (nth 2 waiter) value reason)
             :delivery 'report))
          (delq nil waiters)))

(defun appkit-operation--forget (entries prefix topic &optional keep)
  "Return (ENTRIES . COMMANDS) revoking keys under PREFIX, optionally KEEP values."
  (let (commands changes)
    (appkit-data-map-each
     (lambda (key entry)
       (when (equal prefix (seq-take key (length prefix)))
         (setq commands
               (append (appkit-operation--replies (plist-get entry :waiters) nil "Data invalidated")
                       (list (appkit-command-cancel-effect (cons 'appkit-operation key))
                             (appkit-command-publish topic key)) commands))
         (push (cons key (if keep (appkit-data-with entry (() :pending nil :waiters nil))
                           appkit-data-delete)) changes)))
     entries)
    (cons (appkit-data-map-edit entries changes) commands)))

(cl-defun appkit-operation-update
    (registry entries context message &key owner (call #'funcall) (topic 'appkit-operation))
  "Return (NEW-ENTRIES . COMMANDS) for operation MESSAGE, or nil for others.
ENTRIES remain untouched.  CALL runs spec bodies in their account context."
  (when (memq (car-safe message) '(appkit-operation appkit-operation-result appkit-operation-forget))
    (let* ((entries entries)
           (appkit-operation--building (cons owner (lambda () entries)))
           (self (appkit-transition-context-owner-address context))
           commands)
      (cl-labels
          ((forget (prefix &optional keep)
             (let ((result (appkit-operation--forget entries prefix topic keep)))
               (setq entries (car result) commands (append commands (cdr result))))))
        (pcase message
          (`(appkit-operation-forget ,prefix) (forget prefix))
          (`(appkit-operation ,submitted ,refresh ,reply-token)
           (let* ((request (appkit-operation--request registry submitted))
                  (spec (appkit-operation--spec registry request))
                  (key (funcall (plist-get spec :key) request))
                  (entry (appkit-data-map-get entries key))
                  (waiter (and reply-token (list (appkit-transition-context-reply-route context)
                                                reply-token submitted)))
                  (write (plist-get spec :write)))
             (cond
              ((and (plist-get entry :pending) (or write (not (eq refresh t))))
               (cond
                ((not write)
                 (when waiter
                   (setq entries (appkit-data-map-set
                                  entries key (appkit-data-with entry
                                                (() :waiters (cons waiter (plist-get entry :waiters))))))))
                ((and (plist-get spec :coalesce)
                      (funcall call (lambda ()
                                      (funcall (plist-get spec :coalesce) request
                                               (or (car (plist-get entry :queued))
                                                   (plist-get entry :request))))))
                 (setq entries (appkit-data-map-set entries key
                                 (appkit-data-with entry (() :queued (list request waiter))))))
                (t (setq commands (appkit-operation--replies
                                   (list waiter) nil "Operation already in progress")))))
              ((and (not write) (not refresh) (plist-get entry :ready))
               (setq commands (appkit-operation--replies (list waiter) (plist-get entry :value) nil)))
              (t
               (let ((steps (mapcar (lambda (step) (appkit-operation--request registry step))
                                   (funcall (plist-get spec :steps) request))))
                 (unless steps (error "Operation %S has no steps" request))
                 (setq commands (appkit-operation--replies
                                 (plist-get entry :waiters) nil "Request superseded")
                       entries (appkit-data-map-set
                                entries key
                                (appkit-data-with entry
                                  (() :pending t :request request :error nil :queued nil
                                      :completed 0 :total (length steps) :remaining (cdr steps)
                                      :waiters (and waiter (list waiter))
                                      :observation (funcall call (lambda ()
                                                                  (funcall (plist-get spec :prepare) request)))))))
                 (setq commands
                       (append commands
                               (list (appkit-command-start-effect
                                      (appkit-operation--effect registry call request key refresh (car steps)))
                                     (appkit-command-publish topic key)))))))))
          (`(appkit-operation-result (,request ,key ,step) ,raw ,reason)
           (let ((entry (appkit-data-map-get entries key))
                 (spec (appkit-operation--spec registry request)) value)
             (unless reason
               (condition-case condition
                   (funcall call
                            (lambda ()
                              (setq value (funcall (plist-get spec :commit)
                                                   raw (plist-get entry :observation) step))
                              (dolist (prefix (funcall (plist-get spec :cancel) value request)) (forget prefix t))
                              (dolist (prefix (funcall (plist-get spec :invalidate) value request)) (forget prefix))))
                 (error (setq reason (error-message-string condition)))))
             (let ((remaining (and (not reason) (plist-get entry :remaining))))
               (setq entries
                     (appkit-data-map-set
                      entries key
                      (appkit-data-with entry
                        (() :pending (and remaining t) :ready (or (not reason) (plist-get entry :ready))
                            :completed (+ (or (plist-get entry :completed) 0) (if reason 0 1))
                            :queued (and remaining (plist-get entry :queued)) :remaining (cdr remaining)
                            :waiters (and remaining (plist-get entry :waiters)) :error reason
                            :value (if reason (plist-get entry :value) value)
                            :date (if reason (plist-get entry :date) (format-time-string "%Y-%m-%d"))))))
               (when (and reason (plist-get spec :write)) (message "%s" reason))
               (setq commands
                     (append commands (list (appkit-command-publish topic key))
                             (if remaining
                                 (list (appkit-command-start-effect
                                        (appkit-operation--effect registry call request key nil (car remaining))))
                               (append
                                (appkit-operation--replies (plist-get entry :waiters) value reason)
                                (when-let* ((queued (plist-get entry :queued)))
                                  (list (appkit-command-post-message
                                         :target self :delivery 'report
                                         :message (list 'appkit-operation (car queued) t nil))))
                                (mapcar (lambda (next) (appkit-command-post-message
                                                       :target self :delivery 'report
                                                       :message (list 'appkit-operation next 'join nil)))
                                        (funcall call (lambda ()
                                                        (funcall (plist-get spec :next) value reason request))))))))))))
        (cons entries commands)))))

(provide 'appkit-operation)
;;; appkit-operation.el ends here
