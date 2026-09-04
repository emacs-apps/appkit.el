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
(require 'appkit-core)
(require 'appkit-command)
(require 'appkit-context)

(declare-function appkit-surface-stop "appkit-surface")
(declare-function appkit-surface--parent-fault "appkit-surface")

(cl-defstruct (appkit-app-type
               (:constructor appkit-app-type-create)
               (:copier nil))
  name
  init
  update
  shutdown)

(cl-defstruct
    (appkit-app (:constructor appkit-app--create) (:copier nil))
  identity type loop effect-runtime command-batch command-limit
  surfaces handles surface-limit alive-p)

(defun appkit-app-live-p (app)
  "Return non-nil when APP can accept domain work."
  (and (appkit-app-p app)
       (appkit-app-alive-p app)
       (eq (appkit-loop-status (appkit-app-loop app)) 'running)))

(cl-defmethod appkit-owner-live-p ((owner appkit-app))
  (appkit-app-live-p owner))

(cl-defmethod appkit-owner-app ((owner appkit-app)) owner)

(cl-defmethod appkit-owner-handles ((owner appkit-app))
  (appkit-app-handles owner))

(cl-defmethod appkit-owner-set-handles ((owner appkit-app) handles)
  (setf (appkit-app-handles owner) handles))

(defun appkit-app-model (app)
  "Return APP's current committed canonical model."
  (appkit-loop-model (appkit-app-loop app)))

(defun appkit-app-status (app)
  "Return APP's loop lifecycle status."
  (appkit-loop-status (appkit-app-loop app)))

(defun appkit-app-surface-count (app)
  "Return the number of Surface identities owned by APP."
  (hash-table-count (appkit-app-surfaces app)))

(defun appkit-app-surface (app identity)
  "Return APP's registered Surface for IDENTITY, or nil."
  (unless (appkit-app-p app)
    (signal 'wrong-type-argument (list 'appkit-app-p app)))
  (cdr (gethash identity (appkit-app-surfaces app))))

(defun appkit-app--read-view (app)
  "Capture APP's current committed read view, or signal if APP is unavailable."
  (unless (appkit-app-live-p app)
    (error "Cannot capture a read view from an unavailable App"))
  (let ((loop (appkit-app-loop app)))
    (appkit-app-read-view--create
     :app-identity (appkit-app-identity app)
     :incarnation (appkit-loop-incarnation loop)
     :revision (appkit-loop-revision loop)
     :model (appkit-loop-model loop))))

(defun appkit-app--validate-next (app next)
  "Return NEXT after validating APP's transition result."
  (unless (appkit-next-p next)
    (error "App %S returned invalid transition: %S"
           (appkit-app-type-name (appkit-app-type app)) next))
  (unless (eq (appkit-next-render next) appkit-render-none)
    (error "UI-free App transitions must use appkit-render-none"))
  next)

(defun appkit-app--stage-next (app next)
  "Fold NEXT's deferred work and return its model."
  (appkit-command--batch-add
   (appkit-app-command-batch app)
   (appkit-next-commands next)
   (appkit-app-command-limit app))
  (appkit-next-model next))

(defun appkit-app--client-update (app context model message)
  "Run APP's client update in CONTEXT against MODEL and MESSAGE."
  (let
      ((result
        (funcall (appkit-app-type-update (appkit-app-type app))
                 context model message)))
    (if (appkit-next-rejected-p result)
        (appkit-loop-reject (appkit-next-rejected-reason result))
      (appkit-loop-accept
       (appkit-app--stage-next app
                               (appkit-app--validate-next app result))))))

(defun appkit-app--update (app model message)
  "Dispatch MESSAGE and run APP's client update against MODEL."
  (let ((context (appkit-context--for-loop (appkit-app-loop app))))
    (appkit-effect-runtime-dispatch
     (appkit-app-effect-runtime app)
     (lambda (current input)
       (appkit-app--client-update app context current input))
     model message)))

(defun appkit-app--commit-pending (app)
  "Execute APP's folded post-commit work after its model commit."
  (let* ((work
          (appkit-command--batch-drain
           (appkit-app-command-batch app)))
         (posts (car work))
         (effects (cdr work))
         (loop (appkit-app-loop app)))
    (appkit-command--revoke-effects
     (appkit-app-effect-runtime app) effects 'appkit-app)
    (appkit-command--post-messages
     loop (appkit-loop-revision loop) posts)
    (appkit-command--start-effects
     (appkit-app-effect-runtime app) effects)))

(defun appkit-app--after-pass (app _loop _pass)
  "Commit APP's folded work after one accepted pass."
  (appkit-app--commit-pending app))

(defun appkit-app--on-fault (app _loop fault)
  "Revoke APP and every child Surface when its loop faults."
  (setf (appkit-app-alive-p app) nil)
  (appkit-command--batch-clear (appkit-app-command-batch app))
  (let (conditions)
    (appkit--run-cleanup-forms conditions
      (appkit-effect-runtime-stop (appkit-app-effect-runtime app))
      (appkit--run-cleanup-items (appkit-app--surface-snapshot app)
                                 (lambda (surface)
                                   (appkit-surface--parent-fault
                                    surface fault))
                                 (lambda (condition)
                                   (push condition conditions))))
    (appkit--warn-cleanup-conditions (nreverse conditions) 'appkit-app)))

(defun appkit-app--register-surface (app identity type surface)
  "Register SURFACE under exact IDENTITY and TYPE in APP."
  (unless (appkit-app-live-p app)
    (error "Cannot attach a Surface to an unavailable App"))
  (unless identity
    (error "Attached Surface identity must be non-nil"))
  (let* ((registry (appkit-app-surfaces app))
         (existing (gethash identity registry)))
    (when existing
      (if (eq (car existing) type)
          (error "App Surface identity is already live: %S" identity)
        (error "App Surface identity has a different type: %S" identity)))
    (when (>= (hash-table-count registry) (appkit-app-surface-limit app))
      (error "App Surface limit exceeded: %S" (appkit-app-surface-limit app)))
    (puthash identity (cons type surface) registry)))

(defun appkit-app--unregister-surface (app identity surface)
  "Remove exact SURFACE at IDENTITY from APP."
  (let* ((registry (appkit-app-surfaces app))
         (entry (gethash identity registry)))
    (when (and entry (eq (cdr entry) surface))
      (remhash identity registry))))

(defun appkit-app--surface-snapshot (app)
  "Return a snapshot of APP's currently registered Surfaces."
  (let (surfaces)
    (maphash (lambda (_identity entry) (push (cdr entry) surfaces))
             (appkit-app-surfaces app))
    surfaces))

(defun appkit-app--cleanup-start (app)
  "Clean a failed APP startup without masking its primary exit."
  (when (appkit-app-p app)
    (setf (appkit-app-alive-p app) nil)
    (when-let* ((loop (appkit-app-loop app)))
      (when (appkit-loop--begin-stop loop)
        (let (conditions)
          (unwind-protect
              (progn
                (appkit--run-cleanup-forms conditions
                  (when (appkit-app-effect-runtime app)
                    (appkit-effect-runtime-stop
                     (appkit-app-effect-runtime app)))
                  (when-let* ((shutdown
                               (appkit-app-type-shutdown
                                (appkit-app-type app))))
                    (funcall shutdown app)))
                (appkit--warn-cleanup-conditions
                 (nreverse conditions) 'appkit-app))
            (appkit-loop--finish-stop loop)))))))

(cl-defun appkit-app-start
    (type &key input identity
          (command-limit appkit-command-default-per-next-limit)
          (folded-command-limit appkit-command-default-folded-limit)
          (max-active-effects 32) (surface-limit 32))
  "Start a UI-free App of TYPE with INPUT and return it.

IDENTITY defaults to a fresh opaque symbol.  COMMAND-LIMIT,
FOLDED-COMMAND-LIMIT, MAX-ACTIVE-EFFECTS, and SURFACE-LIMIT are positive hard
bounds owned by this App incarnation."
  (appkit-loop--assert-main-thread)
  (cl-check-type type appkit-app-type)
  (dolist (callback (list (appkit-app-type-init type)
                          (appkit-app-type-update type)))
    (unless (functionp callback)
      (signal 'wrong-type-argument (list 'functionp callback))))
  (unless (or (null (appkit-app-type-shutdown type))
              (functionp (appkit-app-type-shutdown type)))
    (signal 'wrong-type-argument
            (list 'functionp (appkit-app-type-shutdown type))))
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
                           (appkit-app--update app model message))
                 :after-pass (lambda (current pass)
                               (appkit-app--after-pass app current pass))
                 :on-fault (lambda (current fault)
                             (appkit-app--on-fault app current fault)))))
          (setq app
                (appkit-app--create
                 :identity owner
                 :type type
                 :loop loop
                 :effect-runtime
                 (appkit-effect-runtime-create loop max-active-effects)
                 :command-batch
                 (appkit-command--batch-create folded-command-limit)
                 :command-limit command-limit
                 :handles nil
                 :surfaces (make-hash-table :test #'equal)
                 :surface-limit surface-limit
                 :alive-p nil))
          (let* ((context (appkit-context--for-loop loop))
                 (initial
                  (appkit-app--validate-next
                   app (funcall (appkit-app-type-init type) context input))))
            (setf (appkit-loop--model loop)
                  (appkit-app--stage-next app initial))
            (appkit-app--commit-pending app)
            (unless (eq (appkit-loop-status loop) 'running)
              (error "App stopped during startup"))
            (setf (appkit-app-alive-p app) t))
          (setq completed-p t)
          app)
      (unless completed-p
        (appkit-app--cleanup-start app)))))

(defun appkit-app-send (app message)
  "Synchronously send MESSAGE to live APP."
  (unless (appkit-app-live-p app)
    (error "Cannot send to an unavailable App"))
  (appkit-loop-send (appkit-app-loop app) message))

(defun appkit-app-post (app message)
  "Asynchronously post MESSAGE to live APP."
  (unless (appkit-app-live-p app)
    (error "Cannot post to an unavailable App"))
  (appkit-loop-post (appkit-app-loop app) message))

(defun appkit-app-close (app)
  "Stop APP and every currently attached Surface."
  (appkit-loop--assert-main-thread)
  (when (appkit-app-p app)
    (let ((loop (appkit-app-loop app)))
      (when (appkit-loop--begin-stop loop)
        (setf (appkit-app-alive-p app) nil)
        (let (conditions)
          (unwind-protect
              (progn
                (appkit--run-cleanup-forms conditions
                  (appkit--run-cleanup-items
                   (appkit-app--surface-snapshot app)
                   #'appkit-surface-stop
                   (lambda (condition) (push condition conditions)))
                  (appkit-cancel-handles app)
                  (appkit-effect-runtime-stop
                   (appkit-app-effect-runtime app))
                  (when-let*
                      ((shutdown
                        (appkit-app-type-shutdown
                         (appkit-app-type app))))
                    (funcall shutdown app))
                  (clrhash (appkit-app-surfaces app)))
                (setq conditions (nreverse conditions))
                (appkit--warn-cleanup-conditions (cdr conditions)
                                                 'appkit-app)
                (when-let* ((condition (car conditions)))
                  (signal (car condition) (cdr condition)))
                t)
            (appkit-loop--finish-stop loop)))))))

(provide 'appkit-app)

;;; appkit-app.el ends here
