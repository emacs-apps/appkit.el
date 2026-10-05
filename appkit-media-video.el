;;; appkit-media-video.el --- Shared video preview jobs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-apps/appkit.el

;;; Commentary:

;; Protocol-neutral video preview extraction and static preview decoration.
;; Callers provide playable sources and optional poster inputs; this module
;; extracts JPG posters only and never inspects application protocol payloads.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'svg nil t)
(require 'appkit-media-image)
(require 'appkit-media-resource)

(cl-defstruct (appkit-media--video-preview-job
               (:constructor appkit-media--video-preview-job-create))
  process
  target-file
  done-p)

(defvar appkit-media--video-preview-processes
  (make-hash-table :test #'equal)
  "Active video preview jobs keyed by an application-owned opaque key.")

(defvar appkit-media--video-decoration-cache
  (make-hash-table :test #'equal)
  "Static video preview images keyed by namespace and source identity.")

(defconst appkit-media--video-input-protocols "file,https,tls,tcp"
  "FFmpeg protocols allowed while extracting media previews.")

(defun appkit-media-video-preview-policy-key ()
  "Return a cache-key fragment for the current video preview policy."
  (format "video-v4:static:%s:%s"
          appkit-media-preview-max-width
          appkit-media-preview-max-height))

(defun appkit-media-cancel-video-preview (key)
  "Cancel the active video preview owned by opaque KEY, if any."
  (when-let* ((job (gethash key appkit-media--video-preview-processes)))
    (remhash key appkit-media--video-preview-processes)
    (setf (appkit-media--video-preview-job-done-p job) t)
    (let ((process (appkit-media--video-preview-job-process job))
          (target-file (appkit-media--video-preview-job-target-file job)))
      (when (and (processp process) (process-live-p process))
        (delete-process process))
      (when (and (processp process)
                 (buffer-live-p (process-buffer process)))
        (kill-buffer (process-buffer process)))
      (when (and (stringp target-file) (file-exists-p target-file))
        (ignore-errors (delete-file target-file))))
    t))

(defun appkit-media--delete-video-preview-files (cache-base)
  "Delete generated video preview files rooted at CACHE-BASE."
  (let ((file (concat cache-base ".jpg")))
    (when (file-exists-p file)
      (ignore-errors (delete-file file)))))

(cl-defun appkit-media-start-video-preview
    (&key key source preview-source cache-base callback)
  "Create a static JPG video poster asynchronously.

KEY is an application-namespaced opaque job identity.  SOURCE is the playable
file or URL.  PREVIEW-SOURCE optionally supplies a cheaper poster input.
CACHE-BASE is the output path without an extension.  CALLBACK receives IMAGE
and its TARGET-FILE, or two nil values when extraction fails."
  (unless (functionp callback)
    (error "Video preview callback must be callable"))
  (unless (and key (stringp cache-base) (not (string-empty-p cache-base)))
    (error "Video preview requires key and cache-base"))
  (appkit-media-cancel-video-preview key)
  (let* ((ffmpeg (executable-find "ffmpeg"))
         (static-source
          (if (appkit-media--local-or-https-source-p preview-source)
              preview-source
            source)))
    (if (or (not ffmpeg)
            (not (appkit-media--local-or-https-source-p static-source)))
        (funcall callback nil nil)
      (let* ((target-file (concat cache-base ".jpg"))
             (command
              (list ffmpeg
                    "-nostdin" "-y" "-loglevel" "error"
                    "-protocol_whitelist"
                    appkit-media--video-input-protocols
                    "-i" static-source
                    "-vf"
                    (concat "thumbnail=24,scale=960:-2:"
                            "force_original_aspect_ratio=decrease")
                    "-frames:v" "1" target-file))
             (buffer (generate-new-buffer " *appkit-media-video-preview*"))
             (job (appkit-media--video-preview-job-create
                   :target-file target-file)))
        (appkit-media--delete-video-preview-files cache-base)
        (make-directory (or (file-name-directory target-file)
                            default-directory)
                        t)
        ;; Register the job before process creation.  A very short process may
        ;; invoke its sentinel before MAKE-PROCESS returns.
        (puthash key job appkit-media--video-preview-processes)
        (cl-labels
            ((finish (process)
               (unless (appkit-media--video-preview-job-done-p job)
                 (setf (appkit-media--video-preview-job-done-p job) t)
                 (let ((current-p
                        (eq job
                            (gethash key
                                     appkit-media--video-preview-processes))))
                   (when current-p
                     (remhash key appkit-media--video-preview-processes))
                   (unwind-protect
                       (when current-p
                         (if (and (= (process-exit-status process) 0)
                                  (file-exists-p target-file))
                             (funcall
                              callback
                              (appkit-media-preview-image-from-file target-file)
                              target-file)
                           (when (file-exists-p target-file)
                             (ignore-errors (delete-file target-file)))
                           (funcall callback nil nil)))
                     (when (buffer-live-p (process-buffer process))
                       (kill-buffer (process-buffer process))))))))
          (condition-case error-data
              (let ((process
                     (make-process
                      :name (format "appkit-media-video-preview-%s"
                                    (substring
                                     (md5 (format "%s" key)) 0 8))
                      :buffer buffer
                      :command command
                      :noquery t
                      :sentinel
                      (lambda (process _event)
                        (when (memq (process-status process) '(exit signal))
                          (finish process))))))
                (setf (appkit-media--video-preview-job-process job) process)
                ;; Cover process implementations that finish during creation
                ;; without delivering the sentinel until a later event cycle.
                (when (memq (process-status process) '(exit signal))
                  (finish process))
                process)
            (error
             (when (eq job
                       (gethash key appkit-media--video-preview-processes))
               (remhash key appkit-media--video-preview-processes))
             (setf (appkit-media--video-preview-job-done-p job) t)
             (when (buffer-live-p buffer)
               (kill-buffer buffer))
             (when (file-exists-p target-file)
               (ignore-errors (delete-file target-file)))
             (signal (car error-data) (cdr error-data)))))))))

(defun appkit-media--video-preview-image-source (image)
  "Return an SVG-embeddable source plist for IMAGE."
  (let* ((properties (cdr-safe image))
         (file (plist-get properties :file))
         (data (plist-get properties :data))
         (type (plist-get properties :type))
         (mime (cond
                ((and (stringp file) (file-exists-p file))
                 (appkit-media-image-mime-type file))
                ((eq type 'svg) "image/svg+xml")
                ((eq type 'png) "image/png")
                ((memq type '(jpeg jpg)) "image/jpeg")
                ((eq type 'gif) "image/gif")
                ((eq type 'webp) "image/webp"))))
    (cond
     ((and (stringp file) mime)
      (list :source file :data-p nil :mime mime))
     ((and (stringp data) mime)
      (list :source data :data-p t :mime mime)))))

(defun appkit-media--video-preview-display-properties (image)
  "Return safe display properties copied from IMAGE."
  (let* ((properties (cdr-safe image))
         (slices (or (plist-get properties :appkit-media-nslices)
                     (appkit-media-image-slice-count image)))
         result)
    (dolist (property '(:height :width :scale :ascent :mask
                       :appkit-media-playback))
      (when-let* ((value (plist-get properties property)))
        (setq result (plist-put result property value))))
    (plist-put result :appkit-media-nslices slices)))

(defun appkit-media--svg-to-image (svg properties)
  "Create an image object from SVG using PROPERTIES."
  (let ((data (with-temp-buffer
                (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
                (svg-print svg)
                (buffer-string))))
    (apply #'create-image data 'svg t properties)))

(defun appkit-media-clear-video-decoration-cache (&optional namespace)
  "Clear decorated video previews owned by NAMESPACE.

When NAMESPACE is nil, clear every decorated preview."
  (if (null namespace)
      (clrhash appkit-media--video-decoration-cache)
    (let (keys)
      (maphash
       (lambda (key _image)
         (when (equal namespace (car-safe key))
           (push key keys)))
       appkit-media--video-decoration-cache)
      (dolist (key keys)
        (remhash key appkit-media--video-decoration-cache)))))

(defun appkit-media--video-preview-source-identity (properties image)
  "Return cache identity for image PROPERTIES and IMAGE."
  (let ((file (plist-get properties :file)))
    (if (and (stringp file) (file-exists-p file))
        (let ((attributes (file-attributes file)))
          (list file
                (file-attribute-modification-time attributes)
                (file-attribute-size attributes)))
      (or (plist-get properties :data) image))))

(defun appkit-media-video-preview-display-image (image &optional namespace)
  "Return static IMAGE decorated with a play marker.

NAMESPACE owns the cached decoration and supports targeted eviction.
Cache lookup precedes image decoding and SVG source embedding.
Original-resource playback metadata survives this poster-only decoration."
    (when (and image
               (image-type-available-p 'svg)
               (fboundp 'svg-create)
               (fboundp 'svg-embed)
               (fboundp 'svg-print))
      (let* ((properties (cdr-safe image))
             (cache-key
              (list namespace
                    (appkit-media--video-preview-source-identity
                     properties image)
                    (appkit-media--char-pixel-width)
                    (appkit-media--char-pixel-height)
                    properties
                    appkit-media-video-play-icon-radius-divisor
                    appkit-media-video-play-icon-circle-opacity
                    appkit-media-video-play-icon-triangle-opacity)))
        (or (gethash cache-key appkit-media--video-decoration-cache)
            (when-let* ((display-size
                         (ignore-errors
                           (image-size image t (selected-frame))))
                        (source
                         (appkit-media--video-preview-image-source image)))
              (let* ((width (max 1 (round (car display-size))))
                     (height (max 1 (round (cdr display-size))))
                     (svg (svg-create width height)))
                (svg-embed svg
                           (plist-get source :source)
                           (plist-get source :mime)
                           (plist-get source :data-p)
                           :x 0 :y 0 :width width :height height)
                (appkit-media-append-video-play-icon svg width height)
                (let ((decorated
                       (appkit-media--svg-to-image
                        svg
                        (appkit-media--video-preview-display-properties
                         image))))
                  (when (appkit-media-image-object-valid-p decorated)
                    (puthash (copy-tree cache-key) decorated
                             appkit-media--video-decoration-cache)
                    decorated))))))))

(provide 'appkit-media-video)

;;; appkit-media-video.el ends here
