;;; appkit-operation.el --- Keyed App data operations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Operations carry named input values from submission through completion.
;; Entries are persistent data; Effects own execution and cancellation.

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
                (list :fields ',(mapcar (lambda (arg) (intern (concat ":" (symbol-name arg)))) arguments)
                      :write ,(plist-get options :write)
                      :coalesce ,(plist-get options :coalesce)
                      :key ,(fn nil (or (plist-get options :key) `(list ',name ,@arguments)))
                      :steps ,(fn '(model) (or (plist-get options :steps) `(list ,request)))
                      :fetch ,(fn '(model refresh resolve reject) (plist-get options :fetch))
                      :prepare ,(fn '(model) (or (plist-get options :prepare) '(cons model nil)))
                      :commit ,(fn '(model result observation) (or (plist-get options :commit) '(cons model result)))
                      :invalidate ,(fn '(model value) (plist-get options :invalidate))
                      :cancel ,(fn '(model value) (plist-get options :cancel))
                      :next ,(fn '(model value reason) (plist-get options :next)))
                ,registry))))

(defun appkit-operation--spec (registry name)
  "Return the spec for operation NAME in REGISTRY."
  (or (gethash name registry) (error "Unknown operation %S" name)))

(defun appkit-operation--check-input (registry name input)
  "Validate named INPUT against NAME's parameter declaration in REGISTRY."
  (let* ((spec (appkit-operation--spec registry name))
         (keys (plist-get spec :fields))
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

(defun appkit-operation--check-request (registry request)
  "Validate the named REQUEST against REGISTRY and return it."
  (unless (appkit-operation-request-p request)
    (error "Expected a named operation request: %S" request))
  (appkit-operation--check-input registry (appkit-operation-request-name request)
                                (appkit-operation-request-input request))
  request)

(defun appkit-operation-key (registry request)
  "Return the entry key REGISTRY assigns to named REQUEST."
  (appkit-operation--check-request registry request)
  (funcall (plist-get (appkit-operation--spec
                      registry (appkit-operation-request-name request)) :key)
           request))





(defun appkit-operation-submit (registry entries target request &optional refresh reply-token)
  "Send REQUEST to TARGET, an App or, with REPLY-TOKEN, a Surface.
REQUEST is a named input value created by `appkit-operation-request-create'.
REFRESH t replaces a read; join refreshes unless pending; nil reuses ready data.
Completion returns (appkit-operation-completed TOKEN REQUEST VALUE REASON)."
  (appkit-operation--check-request registry request)
  (let* ((spec (appkit-operation--spec registry (appkit-operation-request-name request)))
         (entry (appkit-data-map-get entries (funcall (plist-get spec :key) request))))
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

(defun appkit-operation--effect (registry model request key refresh step)
  "Fetch STEP of REQUEST from the immutable MODEL captured at admission."
  (let ((spec (appkit-operation--spec registry (appkit-operation-request-name request))))
    (appkit-effect-create
     :key (cons 'appkit-operation key) :input (list request key step)
     :start
     (lambda (_context _input _observe resolve reject)
       (let ((cancel
              (condition-case condition
                  (funcall (plist-get spec :fetch) model refresh resolve reject step)
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

(defun appkit-operation--forget (model prefix topic &optional keep)
  "Return (MODEL . COMMANDS) revoking PREFIX, optionally retaining ready values."
  (let (changes commands)
    (appkit-data-map-each
     (lambda (key entry)
       (when (equal prefix (seq-take key (length prefix)))
         (setq commands
               (append commands
                       (appkit-operation--replies (plist-get entry :waiters) nil "Data invalidated")
                       (list (appkit-command-cancel-effect (cons 'appkit-operation key))
                             (appkit-command-publish topic key))))
         (push (cons key (if keep (appkit-data-with entry (() :pending nil :waiters nil))
                           appkit-data-delete))
               changes)))
     (appkit-data-get model :operations))
    (cons (appkit-data-with model
            (() :operations (appkit-data-map-edit (appkit-data-get model :operations) changes)))
          commands)))

(cl-defun appkit-operation-update
    (registry model context message &key (topic 'appkit-operation))
  "Reduce operation MESSAGE against MODEL and return an App transition.
MODEL contains an :operations map.  Prepare and commit functions return
(NEW-MODEL . VALUE); no callbacks mutate a published model."
  (when (memq (car-safe message)
              '(appkit-operation appkit-operation-result appkit-operation-forget))
    (let ((model model) commands)
      (cl-labels
          ((spec (request)
             (appkit-operation--spec registry (appkit-operation-request-name request)))
           (entry (key)
             (appkit-data-map-get (appkit-data-get model :operations) key))
           (put (key value)
             (setq model (appkit-data-with model ((:operations) key value))))
           (reply (waiters value reason)
             (setq commands (append commands (appkit-operation--replies waiters value reason))))
           (publish (key)
             (setq commands (append commands (list (appkit-command-publish topic key)))))
           (fetch (request key refresh step)
             (setq commands
                   (append commands
                           (list (appkit-command-start-effect
                                  (appkit-operation--effect registry model request key refresh step))))))
           (forget (prefix &optional keep)
             (let ((next (appkit-operation--forget model prefix topic keep)))
               (setq model (car next) commands (append commands (cdr next)))))
           (start (request refresh waiter)
             (let* ((definition (spec request))
                    (key (appkit-operation-key registry request))
                    (old (entry key))
                    (prepared (funcall (plist-get definition :prepare) model request))
                    (steps (mapcar (lambda (step) (appkit-operation--check-request registry step))
                                   (funcall (plist-get definition :steps) (car prepared) request))))
               (unless steps (error "Operation %S has no steps" request))
               (setq model (car prepared))
               (reply (plist-get old :waiters) nil "Request superseded")
               (put key (appkit-data-with old
                          (() :pending t :request request :error nil :queued nil
                              :completed 0 :total (length steps) :remaining (cdr steps)
                              :waiters (and waiter (list waiter)) :observation (cdr prepared))))
               (fetch request key refresh (car steps))
               (publish key))))
        (pcase message
          (`(appkit-operation-forget ,prefix) (forget prefix))
          (`(appkit-operation ,request ,refresh ,reply-token)
           (appkit-operation--check-request registry request)
           (let* ((definition (spec request))
                  (key (appkit-operation-key registry request))
                  (old (entry key))
                  (write (plist-get definition :write))
                  (waiter (and reply-token
                               (list (appkit-transition-context-reply-route context)
                                     reply-token request))))
             (cond
              ((and (plist-get old :pending) (or write (not (eq refresh t))))
               (cond
                ((not write)
                 (when waiter
                   (put key (appkit-data-with old
                              (() :waiters (cons waiter (plist-get old :waiters)))))))
                ((and (plist-get definition :coalesce)
                      (funcall (plist-get definition :coalesce) request
                               (or (car (plist-get old :queued)) (plist-get old :request))))
                 (reply (list (cadr (plist-get old :queued))) nil "Request superseded")
                 (put key (appkit-data-with old (() :queued (list request waiter)))))
                (t (reply (list waiter) nil "Operation already in progress"))))
              ((and (not write) (not refresh) (plist-get old :ready))
               (reply (list waiter) (plist-get old :value) nil))
              (t (start request refresh waiter)))))
          (`(appkit-operation-result (,request ,key ,step) ,raw ,reason)
           (let ((old (entry key)) (definition (spec request)) value)
             (unless reason
               (condition-case condition
                   (let ((committed
                          (funcall (plist-get definition :commit)
                                   model raw (plist-get old :observation) step)))
                     (setq model (car committed) value (cdr committed))
                     (dolist (prefix (funcall (plist-get definition :cancel) model value request))
                       (forget prefix t))
                     (dolist (prefix (funcall (plist-get definition :invalidate) model value request))
                       (forget prefix)))
                 (error (setq reason (error-message-string condition)))))
             (let ((remaining (and (not reason) (plist-get old :remaining))))
               (put key (appkit-data-with (entry key)
                          (() :pending (and remaining t)
                              :ready (or (not reason) (plist-get old :ready))
                              :completed (+ (or (plist-get old :completed) 0) (if reason 0 1))
                              :queued (and remaining (plist-get old :queued))
                              :remaining (cdr remaining)
                              :waiters (and remaining (plist-get old :waiters))
                              :error reason :value (if reason (plist-get old :value) value)
                              :date (if reason (plist-get old :date) (format-time-string "%Y-%m-%d")))))
               (when (and reason (plist-get definition :write)) (message "%s" reason))
               (publish key)
               (if remaining
                   (fetch request key nil (car remaining))
                 (reply (plist-get old :waiters) value reason)
                 (when-let* ((queued (plist-get old :queued)))
                   (start (car queued) t (cadr queued)))
                 (dolist (next (funcall (plist-get definition :next) model value reason request))
                   (setq commands
                         (append commands
                                 (list (appkit-command-post-message
                                        :target (appkit-transition-context-owner-address context)
                                        :delivery 'report
                                        :message (list 'appkit-operation next 'join nil)))))))))))
        (appkit-next :model model :render appkit-render-none :commands commands)))))

(provide 'appkit-operation)
;;; appkit-operation.el ends here
