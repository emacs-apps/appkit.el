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

(ert-deftest appkit-media-video-preview-policy-covers-rendering-limits ()
  (let ((appkit-media-inline-animation-enabled t)
        (appkit-media-inline-animation-max-duration 10)
        (appkit-media-inline-animation-max-file-size 4096)
        (appkit-media-inline-animation-frame-rate 8)
        (appkit-media-preview-max-width 460)
        (appkit-media-preview-max-height 360))
    (let ((animated-key (appkit-media-video-preview-policy-key)))
      (should (string-prefix-p "video-v3:animated:" animated-key))
      (let ((appkit-media-inline-animation-enabled nil))
        (should-not
         (equal animated-key (appkit-media-video-preview-policy-key))))
      (let ((appkit-media-preview-max-width 320))
        (should-not
         (equal animated-key (appkit-media-video-preview-policy-key)))))))

(ert-deftest appkit-media-video-animation-policy-requires-bounded-metadata ()
  (let ((appkit-media-inline-animation-enabled t)
        (appkit-media-inline-animation-max-duration 10)
        (appkit-media-inline-animation-max-file-size 4096))
    (should
     (appkit-media--video-animation-eligible-p
      "https://example.invalid/clip.mp4" "2048" "2.5"))
    (should
     (appkit-media--video-animation-eligible-p
      "https://example.invalid/clip.mp4" 4096 10))
    (should-not
     (appkit-media--video-animation-eligible-p
      "https://example.invalid/clip.mp4" nil 2.5))
    (should-not
     (appkit-media--video-animation-eligible-p
      "https://example.invalid/clip.mp4" 4097 2.5))
    (should-not
     (appkit-media--video-animation-eligible-p
      "https://example.invalid/clip.mp4" 2048 10.1))
    (let ((appkit-media-inline-animation-enabled nil))
      (should-not
       (appkit-media--video-animation-eligible-p
        "https://example.invalid/clip.mp4" 2048 2.5)))))

(ert-deftest appkit-media-video-preview-explicit-metadata-selects-animated-command ()
  (let ((appkit-media-inline-animation-enabled t)
        (appkit-media-inline-animation-max-duration 10)
        (appkit-media-inline-animation-max-file-size 4096)
        (appkit-media-inline-animation-frame-rate 8)
        (appkit-media--video-preview-processes
         (make-hash-table :test #'equal))
        (buffer (generate-new-buffer " *appkit-video-animated-test*"))
        command
        sentinel
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
                     (setq command (plist-get properties :command)
                           sentinel (plist-get properties :sentinel))
                     :animated-process))
                  ((symbol-function 'process-status)
                   (lambda (_process) 'run)))
          (appkit-media-start-video-preview
           :key '(qq . "message-1")
           :source "https://example.invalid/clip.mp4"
           :source-size "2048"
           :duration "2.5"
           :cache-base "/tmp/appkit-animated-preview"
           :callback #'ignore)
          (should (functionp sentinel))
          (should (equal "/usr/bin/ffmpeg" (car command)))
          (should (member "https://example.invalid/clip.mp4" command))
          (should
           (equal appkit-media--video-input-protocols
                  (cadr (member "-protocol_whitelist" command))))
          (should (member "-filter_complex" command))
          (should (member "-loop" command))
          (should (member "2.500" command))
          (should (string-suffix-p ".gif" (car (last command))))
          (should-not
           (seq-some
            (lambda (argument)
              (and (stringp argument)
                   (string-prefix-p "thumbnail=" argument)))
            command))
          (should
           (eq :animated-process
               (appkit-media--video-preview-job-process
                (gethash '(qq . "message-1")
                         appkit-media--video-preview-processes))))
          (should (equal '("ffmpeg") executable-queries)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest appkit-media-video-preview-explicit-metadata-selects-static-poster-command ()
  (let ((appkit-media-inline-animation-enabled t)
        (appkit-media-inline-animation-max-duration 10)
        (appkit-media-inline-animation-max-file-size 4096)
        (appkit-media--video-preview-processes
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
           :source-size 2048
           :duration 30
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

(ert-deftest appkit-media-video-preview-missing-duration-is-static-without-probe ()
  (let ((appkit-media--video-preview-processes
         (make-hash-table :test #'equal))
        (buffer (generate-new-buffer " *appkit-video-no-duration-test*"))
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
           :key '(qq . "no-duration")
           :source "https://example.invalid/clip.mp4"
           :source-size 2048
           :duration nil
           :cache-base "/tmp/appkit-no-duration-preview"
           :callback #'ignore)
          (should (equal '("ffmpeg") executable-queries))
          (should (member "-frames:v" command))
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
           :source-size nil
           :duration nil
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

(ert-deftest appkit-media-animated-video-preview-display-is-passthrough ()
  (let ((image '(image :type gif
                 :appkit-media-inline-animation t
                 :appkit-media-inline-animation-duration 2.5)))
    (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
               (lambda (&rest _)
                 (ert-fail
                  "animated previews must bypass static decoration")))
              ((symbol-function 'image-type-available-p)
               (lambda (&rest _)
                 (ert-fail
                  "animated previews must not probe SVG support"))))
      (should (eq image
                  (appkit-media-video-preview-display-image image))))))

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
         :source-size 100
         :duration 1
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
