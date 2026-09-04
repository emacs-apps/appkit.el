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
(require 'appkit-routing)
(require 'appkit-command)
(require 'appkit-cleanup)

(defconst appkit-render-none 'appkit-render-none
  "Explicit disposition requesting no generated presentation work.")

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

(defun appkit-surface-address (surface)
  "Return an exact runtime address for live SURFACE."
  (unless (appkit-surface-live-p surface)
    (error "Cannot address a detached Surface"))
  (appkit-loop-address (appkit-surface-loop surface)))

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

(defun appkit-surface--client-update (surface model message)
  "Run and stage SURFACE's client transition for MODEL and MESSAGE."
  (let ((result
         (funcall
          (appkit-surface-type-update (appkit-surface-type surface))
          surface model message)))
    (cond
     ((appkit-next-p result)
      (appkit-loop-accept (appkit-surface--stage-next surface result)))
     ((appkit-loop-rejected-p result) result)
     (t (appkit-surface--validate-next surface result)))))

(defun appkit-surface--update (surface model message)
  "Dispatch MESSAGE and run SURFACE's client update against MODEL."
  (appkit-effect-runtime-dispatch
   (appkit-surface-effect-runtime surface)
   (lambda (current input)
     (appkit-surface--client-update surface current input))
   model message))

(defun appkit-surface--render-request (surface model request)
  "Render REQUEST for SURFACE from committed MODEL, recovering once on error."
  (let ((renderer (appkit-surface-renderer surface))
        render-condition)
    (condition-case condition
        (progn
          (funcall (appkit-generated-renderer-render renderer)
                   surface model request)
          (setf (appkit-surface-renderer-valid-p surface) t))
      ((error quit)
       (setq render-condition condition)))
    (when render-condition
      (setf (appkit-surface-renderer-valid-p surface) nil)
      (condition-case recovery-condition
          (progn
            (funcall (appkit-generated-renderer-recover renderer)
                     surface model render-condition)
            (setf (appkit-surface-renderer-valid-p surface) t))
        ((error quit)
         (error "Surface render failed (%s); recovery failed (%s)"
                (error-message-string render-condition)
                (error-message-string recovery-condition)))))))


(defun appkit-surface--revoke-effect-commands (surface commands)
  "Revoke Effects superseded or cancelled by final COMMANDS for SURFACE."
  (let ((runtime (appkit-surface-effect-runtime surface))
        conditions)
    (appkit--run-cleanup-items
     commands
     (lambda (command)
       (appkit-effect-runtime-cancel
        runtime (appkit-command--effect-key command)))
     (lambda (condition) (push condition conditions)))
    (setq conditions (nreverse conditions))
    (appkit--warn-cleanup-conditions (cdr conditions) 'appkit-surface)
    (when-let* ((condition (car conditions)))
      (signal (car condition) (cdr condition)))))

(defun appkit-surface--start-effect-commands (surface commands)
  "Start final Effect COMMANDS for SURFACE in their explicit order."
  (dolist (command commands)
    (when (appkit-command-start-effect-p command)
      (appkit-effect-runtime-start
       (appkit-surface-effect-runtime surface)
       (appkit-command-start-effect-effect command)))))

(defun appkit-surface--commit-work (surface model request commands)
  "Run SURFACE's closed post-commit phases for MODEL and folded work."
  (appkit-surface--revoke-effect-commands surface commands)
  (unless (eq request appkit-render-none)
    (with-current-buffer (appkit-surface-buffer surface)
      (appkit-surface--render-request surface model request)))
  (appkit-surface--start-effect-commands surface commands))

(defun appkit-surface--commit-pending (surface model)
  "Detach and execute SURFACE's current post-commit work against MODEL."
  (let ((request (appkit-surface-pending-render surface))
        (commands
         (appkit-command--batch-drain-effects
          (appkit-surface-command-batch surface))))
    ;; Detach the completed batch before any cleanup, renderer, or starter can
    ;; fail.  A fault must not retain commands against an unusable model.
    (setf (appkit-surface-pending-render surface) appkit-render-none)
    (appkit-surface--commit-work surface model request commands)))

(defun appkit-surface--after-pass (surface loop _pass)
  "Commit SURFACE's folded work after LOOP finishes model transitions."
  (appkit-surface--commit-pending surface (appkit-loop-model loop)))

(defun appkit-surface--on-fault (surface _loop _fault)
  "Revoke SURFACE work immediately after its loop faults."
  (setf (appkit-surface-pending-render surface) appkit-render-none)
  (appkit-command--batch-clear (appkit-surface-command-batch surface))
  (appkit-effect-runtime-stop (appkit-surface-effect-runtime surface)))

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
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (eq appkit--current-surface surface)
            (setq-local appkit--current-surface nil))))
      (when (and created-p (buffer-live-p buffer))
        (kill-buffer buffer)))
    (appkit--warn-cleanup-conditions
     (nreverse conditions) 'appkit-surface)))

(cl-defun appkit-open-generated-surface
    (type &key input buffer buffer-name select
          (command-limit appkit-command-default-per-next-limit)
          (folded-command-limit appkit-command-default-folded-limit)
          (max-active-effects 32))
  "Create and mount one generated Surface of TYPE.

INPUT is passed to TYPE's init function.  BUFFER, when non-nil, must be a live
unattached buffer; otherwise a fresh buffer is created using BUFFER-NAME.
Major mode initialization always precedes Surface attachment.  SELECT displays
the buffer only after initial presentation succeeds.  COMMAND-LIMIT,
FOLDED-COMMAND-LIMIT, and MAX-ACTIVE-EFFECTS bound deferred work."
  (appkit-loop--assert-main-thread)
  (cl-check-type type appkit-surface-type)
  (appkit-command--check-limit "Per-next command limit" command-limit)
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
            (let ((initial
                   (appkit-surface--validate-next
                    surface
                    (funcall (appkit-surface-type-init type) surface input))))
              (appkit-surface--stage-next surface initial)
              (setf (appkit-surface-renderer surface)
                    (funcall
                     (appkit-surface-type-renderer-factory type) surface)
                    renderer-created-p t)
              (let ((loop
                     (appkit-loop-create
                      :model (appkit-next-model initial)
                      :update
                      (lambda (model message)
                        (appkit-surface--update surface model message))
                      :after-pass
                      (lambda (current pass)
                        (appkit-surface--after-pass surface current pass))
                      :on-fault
                      (lambda (current fault)
                        (appkit-surface--on-fault
                         surface current fault)))))
                (setf (appkit-surface-loop surface) loop
                      (appkit-surface-effect-runtime surface)
                      (appkit-effect-runtime-create
                       loop max-active-effects)))
              (setq-local appkit--current-surface surface)
              (add-hook 'kill-buffer-hook
                        #'appkit-surface--host-detach nil t)
              (add-hook 'change-major-mode-hook
                        #'appkit-surface--host-detach nil t)
              (funcall
               (appkit-generated-renderer-mount
                (appkit-surface-renderer surface))
               surface (appkit-next-model initial))
              (setf (appkit-surface-renderer-valid-p surface) t)
              (appkit-surface--commit-pending
               surface (appkit-next-model initial))))
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
