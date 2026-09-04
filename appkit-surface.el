;;; appkit-surface.el --- Generated buffer Surface runtime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; A first complete generated-buffer vertical slice over `appkit-loop'.  A
;; Surface owns one persistent buffer and one renderer.  Client updates return
;; model and render disposition together; the runtime merges render requests
;; across a pass and commits presentation once from the final model.

;;; Code:

(require 'cl-lib)
(require 'appkit-loop)

(defconst appkit-render-none 'appkit-render-none
  "Explicit disposition requesting no generated presentation work.")

(cl-defstruct (appkit-surface-next
               (:constructor appkit-surface-next (model render))
               (:copier nil))
  "One accepted Surface transition with MODEL and explicit RENDER request."
  model
  render)

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
  (unless (appkit-surface-p surface)
    (signal 'wrong-type-argument (list 'appkit-surface-p surface)))
  (appkit-loop-model (appkit-surface-loop surface)))

(defun appkit-surface-status (surface)
  "Return SURFACE's loop lifecycle status."
  (unless (appkit-surface-p surface)
    (signal 'wrong-type-argument (list 'appkit-surface-p surface)))
  (appkit-loop-status (appkit-surface-loop surface)))

(defun appkit-surface--validate-type (type)
  "Return TYPE after validating its required program functions."
  (unless (appkit-surface-type-p type)
    (signal 'wrong-type-argument (list 'appkit-surface-type-p type)))
  (unless (symbolp (appkit-surface-type-name type))
    (error "Surface type name must be a symbol: %S"
           (appkit-surface-type-name type)))
  (dolist (entry
           `((,(appkit-surface-type-mode type) . mode)
             (,(appkit-surface-type-init type) . init)
             (,(appkit-surface-type-update type) . update)
             (,(appkit-surface-type-renderer-factory type)
              . renderer-factory)))
    (unless (functionp (car entry))
      (error "Surface type %S %s must be callable"
             (appkit-surface-type-name type) (cdr entry))))
  type)

(defun appkit-surface--validate-renderer (renderer)
  "Return RENDERER after validating the generated protocol."
  (unless (appkit-generated-renderer-p renderer)
    (signal 'wrong-type-argument
            (list 'appkit-generated-renderer-p renderer)))
  (dolist (entry
           `((,(appkit-generated-renderer-mount renderer) . mount)
             (,(appkit-generated-renderer-merge renderer) . merge)
             (,(appkit-generated-renderer-render renderer) . render)
             (,(appkit-generated-renderer-recover renderer) . recover)
             (,(appkit-generated-renderer-unmount renderer) . unmount)))
    (unless (functionp (car entry))
      (error "Generated renderer %s must be callable" (cdr entry))))
  renderer)

(defun appkit-surface--validate-next (surface next)
  "Return NEXT after validating SURFACE's transition result."
  (unless (appkit-surface-next-p next)
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

(defun appkit-surface--update (surface model message)
  "Run SURFACE's client update for MODEL and MESSAGE."
  (let ((result
         (funcall
          (appkit-surface-type-update (appkit-surface-type surface))
          surface model message)))
    (cond
     ((appkit-surface-next-p result)
      (appkit-surface--merge-render
       surface (appkit-surface-next-render result))
      (appkit-loop-accept (appkit-surface-next-model result)))
     ((appkit-loop-rejected-p result) result)
     (t (appkit-surface--validate-next surface result)))))

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

(defun appkit-surface--after-pass (surface loop _pass)
  "Commit SURFACE presentation after LOOP finishes its model transitions."
  (let ((request (appkit-surface-pending-render surface)))
    (unless (eq request appkit-render-none)
      (setf (appkit-surface-pending-render surface) appkit-render-none)
      (with-current-buffer (appkit-surface-buffer surface)
        (appkit-surface--render-request
         surface (appkit-loop-model loop) request)))))

(defun appkit-surface--cleanup-open (surface buffer created-p renderer-created-p)
  "Clean a failed Surface open without hiding its primary nonlocal exit."
  (when (appkit-surface-p surface)
    (setf (appkit-surface-alive-p surface) nil)
    (when-let* ((loop (appkit-surface-loop surface)))
      (condition-case nil
          (appkit-loop-stop loop)
        ((error quit) nil)))
    (when (and renderer-created-p (appkit-surface-renderer surface))
      (condition-case nil
          (with-current-buffer buffer
            (funcall
             (appkit-generated-renderer-unmount
              (appkit-surface-renderer surface))
             surface))
        ((error quit) nil))))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq appkit--current-surface surface)
        (setq-local appkit--current-surface nil)))
    (when created-p
      (kill-buffer buffer))))

(cl-defun appkit-open-generated-surface
    (type &key input buffer buffer-name select)
  "Create and mount one generated Surface of TYPE.

INPUT is passed to TYPE's init function.  BUFFER, when non-nil, must be a live
unattached buffer; otherwise a fresh buffer is created using BUFFER-NAME.
Major mode initialization always precedes Surface attachment.  SELECT displays
the buffer only after initial presentation succeeds."
  (appkit-loop--assert-main-thread)
  (appkit-surface--validate-type type)
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
                   :renderer nil
                   :pending-render appkit-render-none
                   :renderer-valid-p nil
                   :alive-p t))
            (let ((initial
                   (appkit-surface--validate-next
                    surface
                    (funcall (appkit-surface-type-init type) surface input))))
              (setf (appkit-surface-renderer surface)
                    (appkit-surface--validate-renderer
                     (funcall
                      (appkit-surface-type-renderer-factory type) surface))
                    renderer-created-p t)
              (let ((loop
                     (appkit-loop-create
                      :model (appkit-surface-next-model initial)
                      :update
                      (lambda (model message)
                        (appkit-surface--update surface model message))
                      :after-pass
                      (lambda (current pass)
                        (appkit-surface--after-pass surface current pass)))))
                (setf (appkit-surface-loop surface) loop))
              (setq-local appkit--current-surface surface)
              (add-hook 'kill-buffer-hook
                        #'appkit-surface--host-detach nil t)
              (add-hook 'change-major-mode-hook
                        #'appkit-surface--host-detach nil t)
              (funcall
               (appkit-generated-renderer-mount
                (appkit-surface-renderer surface))
               surface (appkit-surface-next-model initial))
              (setf (appkit-surface-renderer-valid-p surface) t)
              (unless (eq (appkit-surface-next-render initial)
                          appkit-render-none)
                (appkit-surface--render-request
                 surface
                 (appkit-surface-next-model initial)
                 (appkit-surface-next-render initial)))))
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
  (unless (appkit-surface-p surface)
    (signal 'wrong-type-argument (list 'appkit-surface-p surface)))
  (when (appkit-surface-alive-p surface)
    (let ((buffer (appkit-surface-buffer surface))
          first-condition)
      ;; Revoke host identity before client cleanup can reenter.
      (setf (appkit-surface-alive-p surface) nil)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (eq appkit--current-surface surface)
            (setq-local appkit--current-surface nil))))
      (condition-case condition
          (appkit-loop-stop (appkit-surface-loop surface))
        ((error quit) (setq first-condition condition)))
      (condition-case condition
          (when (and (buffer-live-p buffer)
                     (appkit-surface-renderer surface))
            (with-current-buffer buffer
              (funcall
               (appkit-generated-renderer-unmount
                (appkit-surface-renderer surface))
               surface)))
        ((error quit)
         (unless first-condition
           (setq first-condition condition))))
      (when first-condition
        (signal (car first-condition) (cdr first-condition)))
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
