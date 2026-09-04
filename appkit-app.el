;;; appkit-app.el --- UI-free canonical App runtime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; A canonical, UI-free Program facade over the serialized loop and finite
;; Effect runtime.  Apps own Surface identity and provide pass-scoped read
;; views without exposing their loop or registry to client callbacks.

;;; Code:

(require 'cl-lib)
(require 'appkit-cleanup)
(require 'appkit-command)
(require 'appkit-context)

(declare-function appkit-surface-stop "appkit-surface")

(cl-defstruct (appkit-runtime-app-type
               (:constructor appkit-runtime-app-type-create)
               (:copier nil))
  name
  init
  update
  shutdown)

(cl-defstruct (appkit-runtime-app
               (:constructor appkit-runtime-app--create)
               (:copier nil))
  identity
  type
  loop
  effect-runtime
  fault-stop-handle
  command-batch
  command-limit
  surfaces
  surface-limit
  alive-p)

(defun appkit-runtime-app-live-p (app)
  "Return non-nil when APP can accept domain work."
  (and (appkit-runtime-app-p app)
       (appkit-runtime-app-alive-p app)
       (eq (appkit-loop-status (appkit-runtime-app-loop app)) 'running)))

(defun appkit-runtime-app-model (app)
  "Return APP's current committed canonical model."
  (appkit-loop-model (appkit-runtime-app-loop app)))

(defun appkit-runtime-app-status (app)
  "Return APP's loop lifecycle status."
  (appkit-loop-status (appkit-runtime-app-loop app)))

(defun appkit-runtime-app-surface-count (app)
  "Return the number of Surface identities owned by APP."
  (hash-table-count (appkit-runtime-app-surfaces app)))

(defun appkit-runtime-app--read-view (app)
  "Capture APP's current committed read view, or signal if APP is unavailable."
  (unless (appkit-runtime-app-live-p app)
    (error "Cannot capture a read view from an unavailable App"))
  (let ((loop (appkit-runtime-app-loop app)))
    (appkit-app-read-view--create
     :app-identity (appkit-runtime-app-identity app)
     :incarnation (appkit-loop-incarnation loop)
     :revision (appkit-loop-revision loop)
     :model (appkit-loop-model loop))))

(defun appkit-runtime-app--validate-next (app next)
  "Return NEXT after validating APP's transition result."
  (unless (appkit-next-p next)
    (error "App %S returned invalid transition: %S"
           (appkit-runtime-app-type-name (appkit-runtime-app-type app)) next))
  (unless (eq (appkit-next-render next) appkit-render-none)
    (error "UI-free App transitions must use appkit-render-none"))
  next)

(defun appkit-runtime-app--stage-next (app next)
  "Fold NEXT's deferred work and return its model."
  (appkit-command--batch-add
   (appkit-runtime-app-command-batch app)
   (appkit-next-commands next)
   (appkit-runtime-app-command-limit app))
  (appkit-next-model next))

(defun appkit-runtime-app--client-update (app context model message)
  "Run APP's client update in CONTEXT against MODEL and MESSAGE."
  (let ((result
         (funcall (appkit-runtime-app-type-update (appkit-runtime-app-type app))
                  context model message)))
    (cond
     ((appkit-next-p result)
      (appkit-loop-accept (appkit-runtime-app--stage-next app result)))
     ((appkit-loop-rejected-p result) result)
     (t (appkit-runtime-app--validate-next app result)))))

(defun appkit-runtime-app--update (app model message)
  "Dispatch MESSAGE and run APP's client update against MODEL."
  (let ((context (appkit-context--for-loop (appkit-runtime-app-loop app))))
    (appkit-effect-runtime-dispatch
     (appkit-runtime-app-effect-runtime app)
     (lambda (current input)
       (appkit-runtime-app--client-update app context current input))
     model message)))

(defun appkit-runtime-app--commit-pending (app)
  "Execute APP's folded Effect work after its model commit."
  (let ((commands
         (appkit-command--batch-drain-effects
          (appkit-runtime-app-command-batch app))))
    (appkit-command--revoke-effects
     (appkit-runtime-app-effect-runtime app) commands 'appkit-app)
    (appkit-command--start-effects
     (appkit-runtime-app-effect-runtime app) commands)))

(defun appkit-runtime-app--after-pass (app _loop _pass)
  "Commit APP's folded work after one accepted pass."
  (appkit-runtime-app--commit-pending app))

(defun appkit-runtime-app--stop-faulted (app)
  "Finish stopping faulted APP after its active pass unwinds."
  (setf (appkit-runtime-app-fault-stop-handle app) nil)
  (when (eq (appkit-runtime-app-status app) 'faulted)
    (condition-case condition
        (appkit-runtime-app-stop app)
      ((error quit)
       (display-warning
        'appkit-app
        (format "Faulted App cleanup failed: %s"
                (error-message-string condition))
        :warning)))))

(defun appkit-runtime-app--on-fault (app _loop _fault)
  "Revoke APP authority and schedule owned cleanup after its loop faults."
  (setf (appkit-runtime-app-alive-p app) nil)
  (appkit-command--batch-clear (appkit-runtime-app-command-batch app))
  (unwind-protect
      (appkit-effect-runtime-stop (appkit-runtime-app-effect-runtime app))
    (setf (appkit-runtime-app-fault-stop-handle app)
          (run-at-time 0 nil #'appkit-runtime-app--stop-faulted app))))

(defun appkit-runtime-app--register-surface (app identity type surface)
  "Register SURFACE under exact IDENTITY and TYPE in APP."
  (unless (appkit-runtime-app-live-p app)
    (error "Cannot attach a Surface to an unavailable App"))
  (unless identity
    (error "Attached Surface identity must be non-nil"))
  (let* ((registry (appkit-runtime-app-surfaces app))
         (existing (gethash identity registry)))
    (when existing
      (if (eq (car existing) type)
          (error "App Surface identity is already live: %S" identity)
        (error "App Surface identity has a different type: %S" identity)))
    (when (>= (hash-table-count registry) (appkit-runtime-app-surface-limit app))
      (error "App Surface limit exceeded: %S" (appkit-runtime-app-surface-limit app)))
    (puthash identity (cons type surface) registry)))

(defun appkit-runtime-app--unregister-surface (app identity surface)
  "Remove exact SURFACE at IDENTITY from APP."
  (let* ((registry (appkit-runtime-app-surfaces app))
         (entry (gethash identity registry)))
    (when (and entry (eq (cdr entry) surface))
      (remhash identity registry))))

(defun appkit-runtime-app--surface-snapshot (app)
  "Return a snapshot of APP's currently registered Surfaces."
  (let (surfaces)
    (maphash (lambda (_identity entry) (push (cdr entry) surfaces))
             (appkit-runtime-app-surfaces app))
    surfaces))

(defun appkit-runtime-app--cleanup-start (app)
  "Clean a failed APP startup without masking its primary exit."
  (when (appkit-runtime-app-p app)
    (setf (appkit-runtime-app-alive-p app) nil)
    (let (conditions)
      (appkit--run-cleanup-forms conditions
        (when (appkit-runtime-app-effect-runtime app)
          (appkit-effect-runtime-stop (appkit-runtime-app-effect-runtime app)))
        (when (appkit-runtime-app-loop app)
          (appkit-loop-stop (appkit-runtime-app-loop app)))
        (when-let* ((shutdown (appkit-runtime-app-type-shutdown
                              (appkit-runtime-app-type app))))
          (funcall shutdown app)))
      (appkit--warn-cleanup-conditions
       (nreverse conditions) 'appkit-app))))

(cl-defun appkit-start-runtime-app
    (type &key input identity
          (command-limit appkit-command-default-per-next-limit)
          (folded-command-limit appkit-command-default-folded-limit)
          (max-active-effects 32) (surface-limit 32))
  "Start a UI-free App of TYPE with INPUT and return it.

IDENTITY defaults to a fresh opaque symbol.  COMMAND-LIMIT,
FOLDED-COMMAND-LIMIT, MAX-ACTIVE-EFFECTS, and SURFACE-LIMIT are positive hard
bounds owned by this App incarnation."
  (appkit-loop--assert-main-thread)
  (cl-check-type type appkit-runtime-app-type)
  (dolist (callback (list (appkit-runtime-app-type-init type)
                          (appkit-runtime-app-type-update type)))
    (unless (functionp callback)
      (signal 'wrong-type-argument (list 'functionp callback))))
  (unless (or (null (appkit-runtime-app-type-shutdown type))
              (functionp (appkit-runtime-app-type-shutdown type)))
    (signal 'wrong-type-argument
            (list 'functionp (appkit-runtime-app-type-shutdown type))))
  (appkit-command--check-limit "Per-next command limit" command-limit)
  (appkit-command--check-limit "Surface limit" surface-limit)
  (let (app completed-p)
    (unwind-protect
        (let* ((owner (or identity (make-symbol "appkit-app-")))
               (loop
                (appkit-loop-create
                 :owner-identity owner
                 :model nil
                 :update (lambda (model message)
                           (appkit-runtime-app--update app model message))
                 :after-pass (lambda (current pass)
                               (appkit-runtime-app--after-pass app current pass))
                 :on-fault (lambda (current fault)
                             (appkit-runtime-app--on-fault app current fault)))))
          (setq app
                (appkit-runtime-app--create
                 :identity owner
                 :type type
                 :loop loop
                 :effect-runtime
                 (appkit-effect-runtime-create loop max-active-effects)
                 :command-batch
                 (appkit-command--batch-create folded-command-limit)
                 :command-limit command-limit
                 :fault-stop-handle nil
                 :surfaces (make-hash-table :test #'equal)
                 :surface-limit surface-limit
                 :alive-p nil))
          (let* ((context (appkit-context--for-loop loop))
                 (initial
                  (appkit-runtime-app--validate-next
                   app (funcall (appkit-runtime-app-type-init type) context input))))
            (setf (appkit-loop--model loop) (appkit-runtime-app--stage-next app initial)
                  (appkit-runtime-app-alive-p app) t)
            (appkit-runtime-app--commit-pending app))
          (setq completed-p t)
          app)
      (unless completed-p
        (appkit-runtime-app--cleanup-start app)))))

(defun appkit-runtime-app-send (app message)
  "Synchronously send MESSAGE to live APP."
  (unless (appkit-runtime-app-live-p app)
    (error "Cannot send to an unavailable App"))
  (appkit-loop-send (appkit-runtime-app-loop app) message))

(defun appkit-runtime-app-post (app message)
  "Asynchronously post MESSAGE to live APP."
  (unless (appkit-runtime-app-live-p app)
    (error "Cannot post to an unavailable App"))
  (appkit-loop-post (appkit-runtime-app-loop app) message))

(defun appkit-runtime-app-stop (app)
  "Stop APP and every currently attached Surface."
  (appkit-loop--assert-main-thread)
  (when (and (appkit-runtime-app-p app)
             (not (eq (appkit-loop-status (appkit-runtime-app-loop app)) 'stopped)))
    (setf (appkit-runtime-app-alive-p app) nil)
    (when-let* ((timer (appkit-runtime-app-fault-stop-handle app)))
      (setf (appkit-runtime-app-fault-stop-handle app) nil)
      (when (timerp timer)
        (cancel-timer timer)))
    (let (conditions)
      (appkit--run-cleanup-forms conditions
        (appkit--run-cleanup-items
         (appkit-runtime-app--surface-snapshot app)
         #'appkit-surface-stop
         (lambda (condition) (push condition conditions)))
        (appkit-effect-runtime-stop (appkit-runtime-app-effect-runtime app))
        (appkit-loop-stop (appkit-runtime-app-loop app))
        (when-let* ((shutdown (appkit-runtime-app-type-shutdown
                              (appkit-runtime-app-type app))))
          (funcall shutdown app)))
      (clrhash (appkit-runtime-app-surfaces app))
      (setq conditions (nreverse conditions))
      (appkit--warn-cleanup-conditions (cdr conditions) 'appkit-app)
      (when-let* ((condition (car conditions)))
        (signal (car condition) (cdr condition)))
      t)))

(provide 'appkit-app)

;;; appkit-app.el ends here
