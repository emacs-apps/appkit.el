;;; appkit-media-video-test.el --- Tests for video previews -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'appkit-media-video)

(defun appkit-media-video-test--buffer-generator (preview-buffer)
  "Return a generator reserving only the preview buffer as PREVIEW-BUFFER."
  (let ((original (symbol-function 'generate-new-buffer)))
    (lambda (name &optional inhibit-buffer-hooks)
      (if (equal name " *appkit-media-video-preview*")
          preview-buffer
        (funcall original name inhibit-buffer-hooks)))))

(ert-deftest appkit-media-video-poster-cache-follows-size-not-autoplay ()
  (let ((appkit-media-inline-animation-enabled t)
        (appkit-media-preview-max-width 460)
        (appkit-media-preview-max-height 360))
    (let ((poster-key (appkit-media-video-preview-policy-key)))
      (let ((appkit-media-inline-animation-enabled nil))
        (should (equal poster-key (appkit-media-video-preview-policy-key))))
      (let ((appkit-media-preview-max-width 320))
        (should-not
         (equal poster-key (appkit-media-video-preview-policy-key)))))))

(ert-deftest appkit-media-video-preview-uses-static-poster-source ()
  (let ((appkit-media--video-preview-processes
         (make-hash-table :test #'equal))
        (buffer (generate-new-buffer " *appkit-video-static-test*"))
        command
        executable-queries)
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (program)
                     (push program executable-queries)
                     (and (equal program "ffmpeg") "/usr/bin/ffmpeg")))
                  ((symbol-function 'generate-new-buffer)
                   (appkit-media-video-test--buffer-generator buffer))
                  ((symbol-function 'make-process)
                   (lambda (&rest properties)
                     (setq command (plist-get properties :command))
                     :static-process))
                  ((symbol-function 'process-status)
                   (lambda (_process) 'run)))
          (appkit-media-start-video-preview
           :key '(disco . "attachment-1")
           :source "https://cdn.example.invalid/long.mp4"
           :preview-source "https://media.example.invalid/poster.jpg"
           :cache-base "/tmp/appkit-static-preview"
           :callback #'ignore)
          (should (equal "/usr/bin/ffmpeg" (car command)))
          (should (member "https://media.example.invalid/poster.jpg" command))
          (should-not
           (member "https://cdn.example.invalid/long.mp4" command))
          (should
           (member
            "thumbnail=24,scale=960:-2:force_original_aspect_ratio=decrease"
            command))
          (should (member "-frames:v" command))
          (should (string-suffix-p ".jpg" (car (last command))))
          (should (equal '("ffmpeg") executable-queries)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest appkit-media-cancel-video-preview-detaches-before-cleanup ()
  (let* ((appkit-media--video-preview-processes
          (make-hash-table :test #'equal))
         (buffer (generate-new-buffer " *appkit-video-cancel-test*"))
         deleted)
    (let ((target (make-temp-file "appkit-video-partial")))
      (puthash '(qq . "preview")
               (appkit-media--video-preview-job-create
                :process :process
                :target-file target)
               appkit-media--video-preview-processes)
      (cl-letf (((symbol-function 'processp)
                 (lambda (process) (eq process :process)))
                ((symbol-function 'process-live-p)
                 (lambda (_process) t))
                ((symbol-function 'delete-process)
                 (lambda (process)
                   (setq deleted process)
                   (should-not
                    (gethash '(qq . "preview")
                             appkit-media--video-preview-processes))))
                ((symbol-function 'process-buffer)
                 (lambda (_process) buffer)))
        (should
         (appkit-media-cancel-video-preview '(qq . "preview")))
        (should (eq :process deleted))
        (should-not (buffer-live-p buffer))
        (should-not (file-exists-p target))
        (should-not
         (gethash '(qq . "preview")
                  appkit-media--video-preview-processes))))))

(ert-deftest appkit-media-video-preview-extracts-one-frame-without-probe ()
  (let ((appkit-media--video-preview-processes
         (make-hash-table :test #'equal))
        (buffer (generate-new-buffer " *appkit-video-poster-test*"))
        executable-queries
        command)
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (program)
                     (push program executable-queries)
                     (and (equal program "ffmpeg") "/usr/bin/ffmpeg")))
                  ((symbol-function 'generate-new-buffer)
                   (appkit-media-video-test--buffer-generator buffer))
                  ((symbol-function 'make-process)
                   (lambda (&rest properties)
                     (setq command (plist-get properties :command))
                     :static-process))
                  ((symbol-function 'process-status)
                   (lambda (_process) 'run)))
          (appkit-media-start-video-preview
           :key '(qq . "poster")
           :source "https://example.invalid/clip.mp4"
           :cache-base "/tmp/appkit-poster-preview"
           :callback #'ignore)
          (should (equal '("ffmpeg") executable-queries))
          (should (member "-frames:v" command))
          (should (string-suffix-p ".jpg" (car (last command))))
          (should-not (member "-filter_complex" command)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest appkit-media-video-preview-immediate-exit-completes-once ()
  (let* ((appkit-media--video-preview-processes
          (make-hash-table :test #'equal))
         (directory (make-temp-file "appkit-video-race" t))
         (cache-base (expand-file-name "preview" directory))
         (buffer (generate-new-buffer " *appkit-video-race-test*"))
         (callback-count 0)
         callback-image)
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (program)
                     (and (equal program "ffmpeg") "/usr/bin/ffmpeg")))
                  ((symbol-function 'generate-new-buffer)
                   (appkit-media-video-test--buffer-generator buffer))
                  ((symbol-function 'make-process)
                   (lambda (&rest properties)
                     (let* ((command (plist-get properties :command))
                            (target (car (last command)))
                            (sentinel (plist-get properties :sentinel)))
                       (with-temp-file target (insert "preview"))
                       (funcall sentinel :instant-process "finished\n")
                       :instant-process)))
                  ((symbol-function 'process-status)
                   (lambda (_process) 'exit))
                  ((symbol-function 'process-exit-status)
                   (lambda (_process) 0))
                  ((symbol-function 'process-buffer)
                   (lambda (_process) buffer))
                  ((symbol-function 'appkit-media-preview-image-from-file)
                   (lambda (_file) :preview-image)))
          (appkit-media-start-video-preview
           :key '(disco . "instant")
           :source "https://example.invalid/poster.jpg"
           :cache-base cache-base
           :callback
           (lambda (image _file)
             (setq callback-count (1+ callback-count)
                   callback-image image)))
          (should (= 1 callback-count))
          (should (eq :preview-image callback-image))
          (should-not
           (gethash '(disco . "instant")
                    appkit-media--video-preview-processes)))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (file-directory-p directory) (delete-directory directory t)))))

(ert-deftest appkit-media-video-decoration-cache-is-namespaced ()
  (let ((appkit-media--video-decoration-cache
         (make-hash-table :test #'equal)))
    (puthash '(qq source) :qq appkit-media--video-decoration-cache)
    (puthash '(disco source) :disco appkit-media--video-decoration-cache)
    (appkit-media-clear-video-decoration-cache 'qq)
    (should-not (gethash '(qq source)
                         appkit-media--video-decoration-cache))
    (should (eq :disco
                (gethash '(disco source)
                         appkit-media--video-decoration-cache)))))

(ert-deftest appkit-media-video-preview-rejects-unsafe-input-protocols ()
  (let ((appkit-media--video-preview-processes
         (make-hash-table :test #'equal))
        spawned
        callback-arguments)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program) "/usr/bin/ffmpeg"))
              ((symbol-function 'make-process)
               (lambda (&rest _)
                 (setq spawned t)
                 :process)))
      (dolist
          (source
           (list
            "file:///etc/passwd"
            "https://user@example.invalid/video.mp4"
            (concat "https://example.invalid/video.mp4\""
                    "\n--output /tmp/injected")))
        (setq callback-arguments nil)
        (appkit-media-start-video-preview
         :key '(test . unsafe)
         :source source
         :cache-base "/tmp/appkit-unsafe-preview"
         :callback (lambda (&rest arguments)
                     (setq callback-arguments arguments)))
        (should (equal '(nil nil) callback-arguments))
        (should-not spawned)
        (should-not
         (gethash '(test . unsafe)
                  appkit-media--video-preview-processes))))))

(provide 'appkit-media-video-test)

;;; appkit-media-video-test.el ends here
