;;; appkit-media-player-test.el --- Tests for owned media playback -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-media-player)

(require 'appkit-test-helper)

(ert-deftest appkit-media-player-normalizes-finite-player-lifecycle ()
  (should
   (equal
    '("mpv" "--no-video" "--keep-open=yes"
      "--force-window=no" "--keep-open=no" "--idle=no" "--start=3.50")
    (appkit-media-player-command-arguments
     '("mpv" "--no-video" "--keep-open=yes") 'audio 3.5)))
  (should
   (equal '("ffplay" "-nodisp" "-hide_banner" "-autoexit"
            "-ss" "3.50")
          (appkit-media-player-command-arguments
           '("ffplay" "-nodisp") 'audio 3.5))))

(ert-deftest appkit-media-player-parses-telega-style-ffplay-progress ()
  (with-temp-buffer
    (insert "\r    12.75 M-A:  0.000 fd=0")
    (should (= 12.75 (appkit-media-player--parse-progress
                      (current-buffer)))))
  (with-temp-buffer
    (insert "frame=3 time=00:01:02.50 bitrate=0")
    (should (= 62.5 (appkit-media-player--parse-progress
                     (current-buffer))))))

(ert-deftest appkit-media-player-pauses-by-restart-and-stops-with-owner ()
  (let* ((app (appkit-app-start appkit-test--app-type :identity 'toggle))
         (file (make-temp-file "appkit-player-"))
         (next-process 0)
         (live (make-hash-table :test #'eq))
         (buffers (make-hash-table :test #'eq))
         process-properties
         filters
         (delete-count 0)
         changes
         finalized
         session)
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-media-command-runnable-p)
                   (lambda (_command) t))
                  ((symbol-function 'make-process)
                   (lambda (&rest properties)
                     (let ((process (intern (format ":player-%d"
                                                    (cl-incf next-process)))))
                       (puthash process t live)
                       (puthash process (plist-get properties :buffer) buffers)
                       (push properties process-properties)
                       (push (cons process (plist-get properties :filter)) filters)
                       process)))
                  ((symbol-function 'processp)
                   (lambda (object) (gethash object live)))
                  ((symbol-function 'process-live-p)
                   (lambda (object) (gethash object live)))
                  ((symbol-function 'process-buffer)
                   (lambda (process) (gethash process buffers)))
                  ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
                  ((symbol-function 'set-process-filter) #'ignore)
                  ((symbol-function 'set-process-sentinel) #'ignore)
                  ((symbol-function 'set-process-plist) #'ignore)
                  ((symbol-function 'delete-process)
                   (lambda (process)
                     (puthash process nil live)
                     (cl-incf delete-count)))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest _arguments) :timer))
                  ((symbol-function 'timerp)
                   (lambda (object) (eq object :timer)))
                  ((symbol-function 'cancel-timer) #'ignore))
          (setq session
                (appkit-media-player-start-file
                 file
                 :kind 'audio
                 :command '("ffplay" "-nodisp")
                 :owner app
                 :duration-seconds 10
                 :on-change
                 (lambda (current)
                   (push (appkit-media-player-session-status current)
                         changes))
                 :on-finalize
                 (lambda (current)
                   (push (appkit-media-player-session-status current)
                         finalized))))
          (let* ((first (appkit-media-player-session-process session))
                 (filter (alist-get first filters)))
            (funcall filter first "\r    3.25 M-A: 0.000"))
          (should (= 3.25 (appkit-media-player-played-seconds session)))
          (appkit-media-player-pause session)
          (should (eq 'paused
                      (appkit-media-player-session-status session)))
          (should-not (appkit-media-player-session-process session))
          (should (= 1 delete-count))
          (appkit-media-player-resume session)
          (should (eq 'playing
                      (appkit-media-player-session-status session)))
          (should (= 2 next-process))
          (should
           (equal
            (append '("ffplay" "-nodisp" "-hide_banner" "-autoexit"
                      "-ss" "3.25")
                    (list file))
            (plist-get (car process-properties) :command)))
          (appkit-media-player-stop session)
          (should (= 2 delete-count))
          (should (eq 'stopped
                      (appkit-media-player-session-status session)))
          (should (equal '(stopped) finalized))
          (appkit-media-player-stop session)
          (should (equal '(stopped) finalized)))
      (when (appkit-app-live-p app)
        (appkit-app-close app))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest appkit-media-player-natural-exit-finalizes-once ()
  (let* ((app (appkit-app-start appkit-test--app-type :identity 'finish))
         (file (make-temp-file "appkit-player-finish-"))
         (live-p t)
         sentinel
         process-buffer
         finalized
         session)
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-media-command-runnable-p)
                   (lambda (_command) t))
                  ((symbol-function 'make-process)
                   (lambda (&rest properties)
                     (setq sentinel (plist-get properties :sentinel)
                           process-buffer (plist-get properties :buffer))
                     :player))
                  ((symbol-function 'processp)
                   (lambda (object) (eq object :player)))
                  ((symbol-function 'process-live-p)
                   (lambda (object) (and (eq object :player) live-p)))
                  ((symbol-function 'process-exit-status)
                   (lambda (_process) 0))
                  ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
                  ((symbol-function 'set-process-plist) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (&rest _arguments) :timer))
                  ((symbol-function 'timerp)
                   (lambda (object) (eq object :timer)))
                  ((symbol-function 'cancel-timer) #'ignore))
          (setq session
                (appkit-media-player-start-file
                 file :command '("ffplay" "-autoexit") :owner app
                 :duration-seconds 2
                 :on-finalize
                 (lambda (current)
                   (push current finalized))))
          (setq live-p nil)
          (should (eq 'finished
                      (appkit-media-player-status session)))
          (should (= 2.0 (appkit-media-player-played-seconds session)))
          (should-not (buffer-live-p process-buffer))
          (should (= 1 (length finalized)))
          (funcall sentinel :player "finished\n")
          (should (= 1 (length finalized))))
      (when (appkit-app-live-p app)
        (appkit-app-close app))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest appkit-media-player-owner-stop-cancels-process-and-finalizes ()
  (let* ((app (appkit-app-start appkit-test--app-type :identity 'owner-stop))
         (file (make-temp-file "appkit-player-owner-"))
         (live-p t)
         deleted
         final-status)
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-media-command-runnable-p)
                   (lambda (_command) t))
                  ((symbol-function 'make-process)
                   (lambda (&rest _properties) :player))
                  ((symbol-function 'processp)
                   (lambda (object) (eq object :player)))
                  ((symbol-function 'process-live-p)
                   (lambda (object) (and (eq object :player) live-p)))
                  ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
                  ((symbol-function 'set-process-filter) #'ignore)
                  ((symbol-function 'set-process-sentinel) #'ignore)
                  ((symbol-function 'delete-process)
                   (lambda (_process)
                     (setq live-p nil deleted t)))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest _arguments) :timer))
                  ((symbol-function 'timerp)
                   (lambda (object) (eq object :timer)))
                  ((symbol-function 'cancel-timer) #'ignore))
          (appkit-media-player-start-file
           file :command '("ffplay" "-autoexit") :owner app
           :on-finalize
           (lambda (session)
             (setq final-status
                   (appkit-media-player-session-status session))))
          (appkit-app-close app)
          (should deleted)
          (should (eq 'stopped final-status)))
      (when (appkit-app-live-p app)
        (appkit-app-close app))
      (when (file-exists-p file)
        (delete-file file)))))

(provide 'appkit-media-player-test)

;;; appkit-media-player-test.el ends here
