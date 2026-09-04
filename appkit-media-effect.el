;;; appkit-media-effect.el --- Managed media Effect adapters  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;;; Commentary:

;; Closed Effect starters for media acquisition and video presentation.
;; Transport callbacks only settle Effect gates; presentation lifetime remains
;; owned by the Effect instance that opened it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-effect)
(require 'appkit-media-resource)

(cl-defstruct (appkit-media-acquisition
               (:constructor appkit-media--acquisition-create)
               (:copier nil))
  "Owned input for one atomic media acquisition."
  resource
  target
  headers)

(cl-defun appkit-media-acquisition-create (resource target &key headers)
  "Create owned media acquisition input for RESOURCE at TARGET.

HEADERS are copied and validated by the transport when the Effect starts."
  (appkit-media--acquisition-create
   :resource (copy-tree (appkit-media-resource-normalize resource))
   :target (expand-file-name target)
   :headers (copy-tree headers)))

(defun appkit-media-acquisition-start
    (_context input _observe resolve reject)
  "Start media acquisition INPUT and settle RESOLVE or REJECT.

Return a transport cancellation capability, or nil after synchronous
settlement."
  (let ((transfer
         (appkit-media-copy-or-download-resource-async
          (appkit-media-acquisition-resource input)
          (appkit-media-acquisition-target input)
          resolve reject
          :headers (appkit-media-acquisition-headers input))))
    (when (appkit-media-transfer-p transfer)
      (appkit-cancellation-create
       :kind 'transport
       :cancel (lambda () (appkit-media-cancel-transfer transfer))))))

(defun appkit-media-acquisition-resource-load
    (context input resolve reject)
  "Load media acquisition INPUT for the Resource companion protocol."
  (appkit-media-acquisition-start
   context input #'ignore resolve reject))

(cl-defstruct (appkit-media-image-acquisition
               (:constructor appkit-media--image-acquisition-create)
               (:copier nil))
  "Owned input for one cached image acquisition."
  resource
  cache-base
  headers)

(cl-defun appkit-media-image-acquisition-create
    (resource cache-base &key headers)
  "Create owned cached image acquisition input."
  (appkit-media--image-acquisition-create
   :resource (copy-tree (appkit-media-resource-normalize resource))
   :cache-base (expand-file-name cache-base)
   :headers (copy-tree headers)))

(defun appkit-media-image-acquisition-start
    (_context input _observe resolve reject)
  "Start cached image acquisition INPUT and settle RESOLVE or REJECT."
  (let ((cache-base (appkit-media-image-acquisition-cache-base input)))
    (if-let* ((cached (appkit-media-image-cache-existing-file cache-base)))
        (progn
          (funcall resolve cached)
          nil)
      (let ((transfer
             (appkit-media-cache-image-resource-async
              (appkit-media-image-acquisition-resource input)
              cache-base resolve reject
              :headers (appkit-media-image-acquisition-headers input))))
        (when (appkit-media-transfer-p transfer)
          (appkit-cancellation-create
           :kind 'transport
           :cancel
           (lambda () (appkit-media-cancel-transfer transfer))))))))

(defun appkit-media-image-resource-load
    (context input resolve reject)
  "Load cached image INPUT for the Resource companion protocol."
  (appkit-media-image-acquisition-start
   context input #'ignore resolve reject))

(defun appkit-media-file-presentation-start
    (_context file _observe resolve reject)
  "Open local FILE after commit and settle the presentation Effect."
  (condition-case condition
      (progn
        (funcall resolve (appkit-media-open-file file))
        nil)
    ((error quit)
     (funcall reject (error-message-string condition))
     nil)))

(cl-defstruct (appkit-media-video-presentation
               (:constructor appkit-media--video-presentation-create)
               (:copier nil))
  "Owned input for one managed video presentation."
  resource
  label
  cache-key
  cache-directory
  cache-policy
  muted
  live
  request-headers
  buffer
  display-function
  autoplay-p)

(cl-defun appkit-media-video-presentation-create
    (resource &key label cache-key cache-directory
              (cache-policy appkit-media-video-cache-policy)
              muted live request-headers buffer display-function (start t))
  "Create owned input for a managed video RESOURCE presentation."
  (appkit-media--video-presentation-create
   :resource (copy-tree (appkit-media-resource-normalize resource))
   :label (or label "media")
   :cache-key cache-key
   :cache-directory cache-directory
   :cache-policy cache-policy
   :muted muted
   :live live
   :request-headers (copy-tree request-headers)
   :buffer buffer
   :display-function display-function
   :autoplay-p start))

(defun appkit-media-video-presentation-start
    (_context input _observe resolve reject)
  "Present video INPUT until its viewer closes or the Effect is cancelled."
  (let (session viewer finished-p)
    (cl-labels
        ((close-presentation
          ()
          (unless finished-p
            (setq finished-p t)
            (when (buffer-live-p viewer)
              (kill-buffer viewer))
            (when (appkit-media-video-session-live-p session)
              (appkit-media-video-session-close session))))
         (viewer-closed
          ()
          (unless finished-p
            (setq finished-p t)
            (funcall resolve 'closed))))
      (condition-case condition
          (progn
            (setq session
                  (appkit-media-video-session-create
                   (appkit-media-video-presentation-resource input)
                   (appkit-media-video-presentation-label input)
                   :cache-key
                   (appkit-media-video-presentation-cache-key input)
                   :cache-directory
                   (appkit-media-video-presentation-cache-directory input)
                   :cache-policy
                   (appkit-media-video-presentation-cache-policy input)
                   :muted (appkit-media-video-presentation-muted input)
                   :live (appkit-media-video-presentation-live input)
                   :request-headers
                   (appkit-media-video-presentation-request-headers input))
                  viewer
                  (appkit-media-present-video-session
                   session
                   (appkit-media-video-presentation-label input)
                   :buffer (appkit-media-video-presentation-buffer input)
                   :display-function
                   (appkit-media-video-presentation-display-function input)
                   :start (appkit-media-video-presentation-autoplay-p input)))
            (with-current-buffer viewer
              (add-hook 'kill-buffer-hook #'viewer-closed nil t))
            (appkit-cancellation-create
             :kind 'logical :cancel #'close-presentation))
        ((error quit)
         (close-presentation)
         (funcall reject (error-message-string condition))
         nil)))))

(provide 'appkit-media-effect)

;;; appkit-media-effect.el ends here
