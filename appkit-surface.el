;;; appkit-surface.el --- Generated buffer Surface runtime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; A generated-buffer vertical slice over `appkit-loop'.  A Surface owns one
;; persistent buffer, Renderer, and Effect runtime.  Accepted transitions fold
;; closed commands and render requests across a pass, then run cleanup,
;; presentation, and Effect startup in fixed post-commit phases.

;;; Code:

(require 'cl-lib)
(require 'appkit-loop)
(require 'appkit-command)
(require 'appkit-app)
(require 'appkit-context)

(cl-defstruct (appkit-surface-type
               (:constructor appkit-surface-type-create)
               (:copier nil))
  name
  mode
  init
  update
  renderer-factory)

(cl-defstruct (appkit-generated-renderer
               (:constructor appkit-generated-renderer-create)
               (:copier nil))
  mount
  merge
  render
  recover
  unmount)

(cl-defstruct (appkit-surface
               (:constructor appkit-surface--create)
               (:copier nil))
  app
  identity
  type
  buffer
  loop
  effect-runtime
  command-batch
  command-limit
  renderer
  pending-render
  renderer-valid-p
  alive-p)

(defvar-local appkit--current-surface nil
  "Generated Appkit Surface attached to the current buffer.")

(defun appkit-current-surface ()
  "Return the generated Surface owned by the current buffer, or nil."
  (and (appkit-surface-p appkit--current-surface)
       (eq (current-buffer) (appkit-surface-buffer appkit--current-surface))
       (appkit-surface-alive-p appkit--current-surface)
       appkit--current-surface))

(defun appkit-surface-live-p (surface)
  "Return non-nil when SURFACE remains attached to its exact live buffer."
  (and (appkit-surface-p surface)
       (appkit-surface-alive-p surface)
       (buffer-live-p (appkit-surface-buffer surface))
       (with-current-buffer (appkit-surface-buffer surface)
         (eq appkit--current-surface surface))))

(defun appkit-surface-model (surface)
  "Return SURFACE's current committed domain model."
  (appkit-loop-model (appkit-surface-loop surface)))

(defun appkit-surface-status (surface)
  "Return SURFACE's loop lifecycle status."
  (appkit-loop-status (appkit-surface-loop surface)))

(defun appkit-surface--validate-next (surface next)
  "Return NEXT after validating SURFACE's transition result."
  (unless (appkit-next-p next)
    (error "Surface %S returned invalid transition: %S"
           (appkit-surface-type-name (appkit-surface-type surface)) next))
  next)

(defun appkit-surface--merge-render (surface request)
  "Merge explicit REQUEST into SURFACE's pending presentation work."
  (unless (eq request appkit-render-none)
    (let ((pending (appkit-surface-pending-render surface)))
      (setf (appkit-surface-pending-render surface)
            (if (eq pending appkit-render-none)
                request
              (let ((merged
                     (funcall
                      (appkit-generated-renderer-merge
                       (appkit-surface-renderer surface))
                      pending request)))
                (when (eq merged appkit-render-none)
                  (error "Renderer merge discarded pending presentation work"))
                merged))))))

(defun appkit-surface--stage-next (surface next)
  "Fold NEXT's deferred work and return its model."
  (appkit-command--batch-add
   (appkit-surface-command-batch surface)
   (appkit-next-commands next)
   (appkit-surface-command-limit surface))
  (appkit-surface--merge-render surface (appkit-next-render next))
  (appkit-next-model next))

(defun appkit-surface--app-read-view (surface)
  "Return the App read view shared by SURFACE's current pass."
  (when-let* ((app (appkit-surface-app surface)))
    (or appkit-loop--pass-context
        (setq appkit-loop--pass-context (appkit-runtime-app--read-view app)))))

(defun appkit-surface--parent-address (surface)
  "Return SURFACE's parent App address, or nil."
  (when-let* ((app (appkit-surface-app surface)))
    (appkit-routing--address (appkit-runtime-app-loop app))))

(defun appkit-surface--client-update (surface context model message)
  "Run and stage SURFACE's client transition in CONTEXT."
  (let ((result
         (funcall
          (appkit-surface-type-update (appkit-surface-type surface))
          context model message)))
    (cond
     ((appkit-next-p result)
      (appkit-loop-accept (appkit-surface--stage-next surface result)))
     ((appkit-loop-rejected-p result) result)
     (t (appkit-surface--validate-next surface result)))))

(defun appkit-surface--update (surface model message)
  "Dispatch MESSAGE and run SURFACE's client update against MODEL."
  (let ((context
         (appkit-context--for-loop
          (appkit-surface-loop surface)
          (appkit-surface--parent-address surface)
          (appkit-surface--app-read-view surface))))
    (appkit-effect-runtime-dispatch
     (appkit-surface-effect-runtime surface)
     (lambda (current input)
       (appkit-surface--client-update surface context current input))
     model message)))

(defun appkit-surface--render-request (surface app-read-view model request)
  "Render REQUEST from MODEL and APP-READ-VIEW, recovering once on error."
  (let ((renderer (appkit-surface-renderer surface))
        render-condition)
    (condition-case condition
        (progn
          (funcall (appkit-generated-renderer-render renderer)
                   surface app-read-view model request)
          (setf (appkit-surface-renderer-valid-p surface) t))
      ((error quit)
       (setq render-condition condition)))
    (when render-condition
      (setf (appkit-surface-renderer-valid-p surface) nil)
      (condition-case recovery-condition
          (progn
            (funcall (appkit-generated-renderer-recover renderer)
                     surface app-read-view model render-condition)
            (setf (appkit-surface-renderer-valid-p surface) t))
        ((error quit)
         (error "Surface render failed (%s); recovery failed (%s)"
                (error-message-string render-condition)
                (error-message-string recovery-condition)))))))

(defun appkit-surface--commit-work
    (surface app-read-view model request commands)
  "Run SURFACE's closed post-commit phases for MODEL and folded work."
  (appkit-command--revoke-effects
   (appkit-surface-effect-runtime surface) commands 'appkit-surface)
  (unless (eq request appkit-render-none)
    (with-current-buffer (appkit-surface-buffer surface)
      (appkit-surface--render-request
       surface app-read-view model request)))
  (appkit-command--start-effects
   (appkit-surface-effect-runtime surface) commands))

(defun appkit-surface--commit-pending (surface app-read-view model)
  "Detach and execute SURFACE's pending work against MODEL and APP-READ-VIEW."
  (let ((request (appkit-surface-pending-render surface))
        (commands
         (appkit-command--batch-drain-effects
          (appkit-surface-command-batch surface))))
    ;; Detach the completed batch before any cleanup, renderer, or starter can
    ;; fail.  A fault must not retain commands against an unusable model.
    (setf (appkit-surface-pending-render surface) appkit-render-none)
    (appkit-surface--commit-work
     surface app-read-view model request commands)))

(defun appkit-surface--after-pass (surface loop _pass)
  "Commit SURFACE's folded work after LOOP finishes model transitions."
  (appkit-surface--commit-pending
   surface appkit-loop--pass-context (appkit-loop-model loop)))

(defun appkit-surface--on-fault (surface _loop _fault)
  "Revoke SURFACE work immediately after its loop faults."
  (setf (appkit-surface-pending-render surface) appkit-render-none)
  (appkit-command--batch-clear (appkit-surface-command-batch surface))
  (appkit-effect-runtime-stop (appkit-surface-effect-runtime surface)))

(defun appkit-surface--unregister (surface)
  "Remove SURFACE's exact identity from its parent App."
  (when-let* ((app (appkit-surface-app surface))
              (identity (appkit-surface-identity surface)))
    (appkit-runtime-app--unregister-surface app identity surface)))

(defun appkit-surface--cleanup-open (surface buffer created-p renderer-created-p)
  "Clean a failed Surface open without hiding its primary nonlocal exit."
  (when (appkit-surface-p surface)
    (setf (appkit-surface-alive-p surface) nil))
  (let (conditions)
    (appkit--run-cleanup-forms conditions
      (when-let* ((runtime
                   (and (appkit-surface-p surface)
                        (appkit-surface-effect-runtime surface))))
        (appkit-effect-runtime-stop runtime))
      (when-let* ((loop
                   (and (appkit-surface-p surface)
                        (appkit-surface-loop surface))))
        (appkit-loop-stop loop))
      (when (and (appkit-surface-p surface)
                 renderer-created-p
                 (appkit-surface-renderer surface)
                 (buffer-live-p buffer))
        (with-current-buffer buffer
          (funcall
           (appkit-generated-renderer-unmount
            (appkit-surface-renderer surface))
           surface)))
      (when (appkit-surface-p surface)
        (appkit-surface--unregister surface))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (eq appkit--current-surface surface)
            (setq-local appkit--current-surface nil))))
      (when (and created-p (buffer-live-p buffer))
        (kill-buffer buffer)))
    (appkit--warn-cleanup-conditions
     (nreverse conditions) 'appkit-surface)))

(cl-defun appkit-open-generated-surface
    (type &key app identity input buffer buffer-name select
          (command-limit appkit-command-default-per-next-limit)
          (folded-command-limit appkit-command-default-folded-limit)
          (max-active-effects 32))
  "Create and mount one generated Surface of TYPE.

APP and IDENTITY attach the new Surface to one canonical App and must be
supplied together.  INPUT is passed to TYPE's init function with a transition
context.  BUFFER, when non-nil, must be live and unattached; otherwise a fresh
buffer is created using BUFFER-NAME.  Major mode initialization always
precedes Surface attachment.  SELECT displays the buffer only after initial
presentation succeeds.  COMMAND-LIMIT, FOLDED-COMMAND-LIMIT, and
MAX-ACTIVE-EFFECTS bound deferred work."
  (appkit-loop--assert-main-thread)
  (cl-check-type type appkit-surface-type)
  (appkit-command--check-limit "Per-next command limit" command-limit)
  (when (not (eq (not (null app)) (not (null identity))))
    (error "Surface App and identity must be supplied together"))
  (when (and app (not (appkit-runtime-app-live-p app)))
    (error "Cannot open a Surface for an unavailable App"))
  (when (and buffer (not (buffer-live-p buffer)))
    (error "Cannot open a Surface in a dead buffer"))
  (let* ((created-p (null buffer))
         (host (or buffer
                   (generate-new-buffer
                    (or buffer-name
                        (format "*%s*" (appkit-surface-type-name type))))))
         surface
         renderer-created-p
         completed-p)
    (unwind-protect
        (progn
          (with-current-buffer host
            (when (appkit-current-surface)
              (error "Buffer already owns a live generated Surface"))
            (funcall (appkit-surface-type-mode type))
            (setq surface
                  (appkit-surface--create
                   :app app
                   :identity identity
                   :type type
                   :buffer host
                   :loop nil
                   :effect-runtime nil
                   :command-batch
                   (appkit-command--batch-create folded-command-limit)
                   :command-limit command-limit
                   :renderer nil
                   :pending-render appkit-render-none
                   :renderer-valid-p nil
                   :alive-p t))
            (let ((loop
                   (appkit-loop-create
                    :owner-identity
                    (if app
                        (list (appkit-runtime-app-identity app) identity)
                      (make-symbol "appkit-surface-"))
                    :model nil
                    :update
                    (lambda (model message)
                      (appkit-surface--update surface model message))
                    :after-pass
                    (lambda (current pass)
                      (appkit-surface--after-pass surface current pass))
                    :on-fault
                    (lambda (current fault)
                      (appkit-surface--on-fault surface current fault)))))
              (setf (appkit-surface-loop surface) loop
                    (appkit-surface-effect-runtime surface)
                    (appkit-effect-runtime-create loop max-active-effects)))
            (setq-local appkit--current-surface surface)
            (add-hook 'kill-buffer-hook #'appkit-surface--host-detach nil t)
            (add-hook 'change-major-mode-hook
                      #'appkit-surface--host-detach nil t)
            (when app
              (appkit-runtime-app--register-surface app identity type surface))
            (let* ((app-read-view (and app (appkit-runtime-app--read-view app)))
                   (context
                    (appkit-context--for-loop
                     (appkit-surface-loop surface)
                     (and app
                          (appkit-routing--address
                           (appkit-runtime-app-loop app)))
                     app-read-view))
                   (initial
                    (appkit-surface--validate-next
                     surface
                     (funcall (appkit-surface-type-init type)
                              context input)))
                   (model (appkit-surface--stage-next surface initial)))
              (setf (appkit-loop--model (appkit-surface-loop surface)) model
                    (appkit-surface-renderer surface)
                    (funcall
                     (appkit-surface-type-renderer-factory type) surface)
                    renderer-created-p t)
              (funcall
               (appkit-generated-renderer-mount
                (appkit-surface-renderer surface))
               surface app-read-view model)
              (setf (appkit-surface-renderer-valid-p surface) t)
              (appkit-surface--commit-pending
               surface app-read-view model)))
          (when select
            (pop-to-buffer host))
          (setq completed-p t)
          surface)
      (unless completed-p
        (appkit-surface--cleanup-open
         surface host created-p renderer-created-p)))))

(defun appkit-surface-send (surface message)
  "Synchronously send MESSAGE to live SURFACE."
  (unless (appkit-surface-live-p surface)
    (error "Cannot send to a detached Surface"))
  (appkit-loop-send (appkit-surface-loop surface) message))

(defun appkit-surface-post (surface message)
  "Asynchronously post MESSAGE to live SURFACE."
  (unless (appkit-surface-live-p surface)
    (error "Cannot post to a detached Surface"))
  (appkit-loop-post (appkit-surface-loop surface) message))

(defun appkit-surface-stop (surface)
  "Stop and detach SURFACE, preserving its buffer."
  (appkit-loop--assert-main-thread)
  (when (appkit-surface-alive-p surface)
    (let ((buffer (appkit-surface-buffer surface))
          conditions)
      ;; Revoke host identity before client cleanup can reenter.
      (setf (appkit-surface-alive-p surface) nil)
      (appkit-surface--unregister surface)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (eq appkit--current-surface surface)
            (setq-local appkit--current-surface nil))))
      (appkit--run-cleanup-forms conditions
        (appkit-effect-runtime-stop
         (appkit-surface-effect-runtime surface))
        (appkit-loop-stop (appkit-surface-loop surface))
        (when (and (buffer-live-p buffer)
                   (appkit-surface-renderer surface))
          (with-current-buffer buffer
            (funcall
             (appkit-generated-renderer-unmount
              (appkit-surface-renderer surface))
             surface))))
      (setq conditions (nreverse conditions))
      (appkit--warn-cleanup-conditions (cdr conditions) 'appkit-surface)
      (when-let* ((condition (car conditions)))
        (signal (car condition) (cdr condition)))
      t)))

(defun appkit-surface--host-detach ()
  "Synchronously stop the Surface owned by the current buffer."
  (when (appkit-surface-p appkit--current-surface)
    (let ((surface appkit--current-surface))
      (condition-case condition
          (appkit-surface-stop surface)
        ((error quit)
         (display-warning
          'appkit-surface
          (format "Surface host cleanup failed: %s"
                  (error-message-string condition))
          :warning))))))

(provide 'appkit-surface)

;;; appkit-surface.el ends here
