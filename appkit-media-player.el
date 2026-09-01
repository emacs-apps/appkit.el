;;; appkit-media-player.el --- Owned local media playback sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Protocol-neutral, owner-bound playback sessions for finite local media.
;; The audio lifecycle follows Telega's proven ffplay design: parse player
;; progress, pause by terminating the player, and resume by starting a new
;; process at the retained media position.  Clients retain acquisition,
;; resource identity, and invalidation policy.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-media-resource)

(defcustom appkit-media-audio-player-command
  (cond
   ((executable-find "ffplay")
    '("ffplay" "-hide_banner" "-autoexit" "-nodisp"))
   ((executable-find "mpv")
    '("mpv" "--no-video" "--force-window=no"))
   ((executable-find "vlc")
    '("vlc" "--intf" "dummy" "--play-and-exit"))
   (t nil))
  "Command used for finite local audio playback sessions."
  :type '(choice
          (const :tag "No audio player" nil)
          string
          (repeat string))
  :group 'appkit-media)

(cl-defstruct
    (appkit-media-player-session
     (:constructor appkit-media-player-session--create))
  "One protocol-neutral, lifecycle-owned local playback session."
  kind
  file
  command
  owner
  process
  buffer
  handle
  timer
  status
  duration-seconds
  played-seconds
  started-at
  progress-observed-p
  last-notified-second
  on-change
  on-finalize
  finalized-p
  cancelled-p)

(defun appkit-media-player--append-option (arguments option)
  "Return ARGUMENTS with OPTION appended unless it is already present."
  (if (member option arguments)
      arguments
    (append arguments (list option))))

(defun appkit-media-player-command-arguments
    (command kind &optional start-seconds)
  "Return finite-playback arguments for COMMAND and media KIND.

START-SECONDS, when positive, is translated to the selected player's native
seek argument.  Persistent MPV `idle' and `keep-open' configuration is always
overridden because an Appkit owner must eventually retire its process."
  (let ((arguments (appkit-media-command-arguments command)))
    (when arguments
      (let ((program (file-name-nondirectory (car arguments)))
            (start (and (numberp start-seconds)
                        (> start-seconds 0)
                        (float start-seconds))))
        (pcase program
          ("ffplay"
           (setq arguments
                 (appkit-media-player--append-option
                  arguments "-hide_banner"))
           (setq arguments
                 (appkit-media-player--append-option arguments "-autoexit"))
           (when (eq kind 'audio)
             (setq arguments
                   (appkit-media-player--append-option arguments "-nodisp")))
           (when start
             (setq arguments
                   (append arguments (list "-ss" (format "%.2f" start))))))
          ("mpv"
           (when (eq kind 'audio)
             (setq arguments
                   (appkit-media-player--append-option arguments "--no-video"))
             (setq arguments
                   (appkit-media-player--append-option
                    arguments "--force-window=no")))
           (setq arguments
                 (appkit-media-player--append-option
                  arguments "--keep-open=no"))
           (setq arguments
                 (appkit-media-player--append-option arguments "--idle=no"))
           (when start
             (setq arguments
                   (append arguments (list (format "--start=%.2f" start))))))
          ("vlc"
           (setq arguments
                 (appkit-media-player--append-option
                  arguments "--play-and-exit"))
           (when start
             (setq arguments
                   (append arguments
                           (list (format "--start-time=%.2f" start)))))))))
    arguments))

(defun appkit-media-player-available-p (&optional command kind)
  "Return non-nil when COMMAND for media KIND is runnable.

COMMAND defaults to `appkit-media-audio-player-command' for audio."
  (let ((effective (or command
                       (and (or (null kind) (eq kind 'audio))
                            appkit-media-audio-player-command))))
    (and (appkit-media-player-command-arguments effective (or kind 'audio))
         (appkit-media-command-runnable-p effective))))

(defun appkit-media-player-status (session)
  "Return SESSION status after synchronously settling a dead player process."
  (unless (appkit-media-player-session-p session)
    (error "Appkit media player session is invalid"))
  (when-let* ((process (appkit-media-player-session-process session))
              ((processp process))
              ((not (process-live-p process))))
    (appkit-media-player--process-finished session process "state poll\n"))
  (appkit-media-player-session-status session))

(defun appkit-media-player-played-seconds (session)
  "Return current bounded playback position for SESSION."
  (unless (appkit-media-player-session-p session)
    (error "Appkit media player session is invalid"))
  (let* ((played (max 0.0
                      (float
                       (or (appkit-media-player-session-played-seconds session)
                           0.0))))
         (started-at (appkit-media-player-session-started-at session))
         (position
          (if (and (eq (appkit-media-player-session-status session) 'playing)
                   (not (appkit-media-player-session-progress-observed-p session))
                   (numberp started-at))
              (+ played (max 0.0 (- (float-time) started-at)))
            played))
         (duration (appkit-media-player-session-duration-seconds session)))
    (if (and (numberp duration) (>= duration 0))
        (min (float duration) position)
      position)))

(defun appkit-media-player--call (function session)
  "Call FUNCTION with SESSION without letting client code break cleanup."
  (when (functionp function)
    (condition-case nil
        (funcall function session)
      (error
       (message "Appkit media player callback failed")))))

(defun appkit-media-player--notify (session)
  "Publish SESSION's current state to its client callback."
  (appkit-media-player--call
   (appkit-media-player-session-on-change session) session))

(defun appkit-media-player--cancel-timer (session)
  "Cancel SESSION's progress timer."
  (when-let* ((timer (appkit-media-player-session-timer session)))
    (when (timerp timer)
      (cancel-timer timer)))
  (setf (appkit-media-player-session-timer session) nil))

(defun appkit-media-player--tick (session)
  "Publish one progress tick for live playing SESSION."
  (let ((process (appkit-media-player-session-process session)))
    (cond
     ((and (eq (appkit-media-player-session-status session) 'playing)
           (process-live-p process))
      (appkit-media-player--notify session))
     ((and (processp process) (not (process-live-p process)))
      (appkit-media-player--process-finished session process "finished\n"))
     (t
      (appkit-media-player--cancel-timer session)))))

(defun appkit-media-player--ensure-timer (session)
  "Start SESSION's bounded progress notification timer."
  (unless (timerp (appkit-media-player-session-timer session))
    (setf (appkit-media-player-session-timer session)
          (run-at-time 1 1 #'appkit-media-player--tick session))))

(defun appkit-media-player--close-buffer (session)
  "Dispose SESSION's exact player output buffer."
  (when-let* ((buffer (appkit-media-player-session-buffer session)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
  (setf (appkit-media-player-session-buffer session) nil))

(defun appkit-media-player--retire-handle (session)
  "Retire SESSION's owner handle without cancellation."
  (when-let* ((handle (appkit-media-player-session-handle session)))
    (when (appkit-handle-alive-p handle)
      (appkit-retire-handle handle)))
  (setf (appkit-media-player-session-handle session) nil))

(defun appkit-media-player--finalize (session status)
  "Finalize SESSION exactly once with terminal STATUS."
  (unless (appkit-media-player-session-finalized-p session)
    (appkit-media-player--cancel-timer session)
    (appkit-media-player--close-buffer session)
    (setf (appkit-media-player-session-process session) nil
          (appkit-media-player-session-status session) status
          (appkit-media-player-session-started-at session) nil
          (appkit-media-player-session-finalized-p session) t)
    (appkit-media-player--retire-handle session)
    (appkit-media-player--notify session)
    (appkit-media-player--call
     (appkit-media-player-session-on-finalize session) session))
  session)

(defun appkit-media-player--stop-process (session)
  "Stop and detach SESSION's current player process."
  (when-let* ((process (appkit-media-player-session-process session)))
    (set-process-filter process nil)
    (set-process-sentinel process nil)
    (when (process-live-p process)
      (delete-process process)))
  (setf (appkit-media-player-session-process session) nil)
  (appkit-media-player--close-buffer session))

(defun appkit-media-player--cancel (session)
  "Cancel SESSION from its Appkit owner handle."
  (unless (appkit-media-player-session-finalized-p session)
    (setf (appkit-media-player-session-played-seconds session)
          (appkit-media-player-played-seconds session)
          (appkit-media-player-session-cancelled-p session) t)
    (appkit-media-player--stop-process session)
    (appkit-media-player--finalize session 'stopped)))

(defun appkit-media-player--parse-progress (buffer)
  "Return latest ffplay/ffmpeg progress found in BUFFER, or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (cond
         ((re-search-backward "\r\\s-*\\([0-9.]+\\)" nil t)
          (string-to-number (match-string 1)))
         ((re-search-backward
           " time=\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9.]+\\) "
           nil t)
          (+ (* 3600 (string-to-number (match-string 1)))
             (* 60 (string-to-number (match-string 2)))
             (string-to-number (match-string 3)))))))))

(defun appkit-media-player--process-filter (session process output)
  "Track Telega-style ffplay progress from PROCESS OUTPUT for SESSION."
  (when (and (eq process (appkit-media-player-session-process session))
             (eq 'playing (appkit-media-player-session-status session)))
    (let ((buffer (process-buffer process)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert output)
            (when (> (buffer-size) 20000)
              (delete-region (point-min)
                             (max (point-min) (- (point-max) 10000)))))))
      (when-let* ((progress (appkit-media-player--parse-progress buffer))
                  ((>= progress
                       (or (appkit-media-player-session-played-seconds session)
                           0.0))))
        (let ((second (floor progress)))
          (setf (appkit-media-player-session-played-seconds session) progress
                (appkit-media-player-session-progress-observed-p session) t
                (appkit-media-player-session-started-at session) nil)
          (unless (equal second
                         (appkit-media-player-session-last-notified-second
                          session))
            (setf (appkit-media-player-session-last-notified-second session)
                  second)
            (appkit-media-player--notify session)))))))

(defun appkit-media-player--process-finished (session process _event)
  "Finalize exact SESSION PROCESS after it exits."
  (when (and (appkit-media-player-session-p session)
             (eq process (appkit-media-player-session-process session))
             (not (process-live-p process))
             (not (appkit-media-player-session-finalized-p session)))
    (let ((successful (zerop (process-exit-status process))))
      (setf (appkit-media-player-session-played-seconds session)
            (if (and successful
                     (numberp
                      (appkit-media-player-session-duration-seconds session)))
                (float
                 (appkit-media-player-session-duration-seconds session))
              (appkit-media-player-played-seconds session))
            (appkit-media-player-session-process session) nil)
      (set-process-plist process nil)
      (appkit-media-player--finalize
       session (if successful 'finished 'failed)))))

(defun appkit-media-player--spawn (session)
  "Start SESSION's player at its retained playback position."
  (let* ((start (or (appkit-media-player-session-played-seconds session) 0.0))
         (arguments
          (appkit-media-player-command-arguments
           (appkit-media-player-session-command session)
           (appkit-media-player-session-kind session)
           start))
         (file (appkit-media-player-session-file session))
         (buffer (generate-new-buffer " *appkit-media-player*"))
         process)
    (with-current-buffer buffer
      (buffer-disable-undo))
    (setf (appkit-media-player-session-buffer session) buffer
          (appkit-media-player-session-status session) 'starting
          (appkit-media-player-session-progress-observed-p session) nil
          (appkit-media-player-session-last-notified-second session) nil)
    (condition-case error-data
        (progn
          (setq process
                (make-process
                 :name (format "appkit-media-%s-player"
                               (appkit-media-player-session-kind session))
                 :buffer buffer
                 :command (append arguments (list file))
                 :noquery t
                 :filter
                 (apply-partially
                  #'appkit-media-player--process-filter session)
                 :sentinel
                 (apply-partially
                  #'appkit-media-player--process-finished session)))
          (set-process-query-on-exit-flag process nil)
          (if (or (appkit-media-player-session-cancelled-p session)
                  (appkit-media-player-session-finalized-p session))
              (progn
                (set-process-filter process nil)
                (set-process-sentinel process nil)
                (when (process-live-p process)
                  (delete-process process))
                (when (buffer-live-p buffer)
                  (kill-buffer buffer))
                session)
            (setf (appkit-media-player-session-process session) process
                  (appkit-media-player-session-status session) 'playing
                  (appkit-media-player-session-started-at session) (float-time))
            (appkit-media-player--ensure-timer session)
            (appkit-media-player--notify session)
            (unless (process-live-p process)
              (appkit-media-player--process-finished
               session process "finished\n"))
            session))
      (error
       (when (buffer-live-p buffer)
         (kill-buffer buffer))
       (setf (appkit-media-player-session-buffer session) nil)
       (appkit-media-player--finalize session 'failed)
       (signal (car error-data) (cdr error-data))))))

(cl-defun appkit-media-player-start-file
    (file &key (kind 'audio) command owner duration-seconds played-seconds
          on-change on-finalize)
  "Start one finite local FILE playback session.

KIND is `audio' or `video'.  COMMAND defaults to
`appkit-media-audio-player-command' for audio.  OWNER, when non-nil, owns the
whole pause/resume session through one Appkit handle.  ON-CHANGE receives every
state/progress transition.  ON-FINALIZE runs exactly once after terminal
cleanup."
  (unless (memq kind '(audio video))
    (error "Appkit media player kind is invalid: %S" kind))
  (unless (and (stringp file) (file-regular-p file))
    (user-error "Appkit media player local file is unavailable"))
  (let* ((effective-command
          (or command
              (and (eq kind 'audio) appkit-media-audio-player-command)))
         (arguments
          (appkit-media-player-command-arguments effective-command kind)))
    (unless (and arguments
                 (appkit-media-command-runnable-p effective-command))
      (user-error "Appkit media player command is unavailable"))
    (let* ((session
            (appkit-media-player-session--create
             :kind kind
             :file file
             :command effective-command
             :owner owner
             :status 'starting
             :duration-seconds
             (and (numberp duration-seconds)
                  (max 0.0 (float duration-seconds)))
             :played-seconds
             (and (numberp played-seconds)
                  (max 0.0 (float played-seconds)))
             :on-change on-change
             :on-finalize on-finalize))
           handle)
      (when owner
        (setq handle
              (appkit-register-handle
               owner 'process session #'appkit-media-player--cancel))
        (setf (appkit-media-player-session-handle session) handle))
      (condition-case error-data
          (appkit-media-player--spawn session)
        (error
         (when (and handle (appkit-handle-alive-p handle))
           (appkit-retire-handle handle))
         (setf (appkit-media-player-session-handle session) nil)
         (unless (appkit-media-player-session-finalized-p session)
           (appkit-media-player--finalize session 'failed))
         (signal (car error-data) (cdr error-data)))))))

(defun appkit-media-player-pause (session)
  "Pause SESSION by retaining progress and stopping its current process."
  (unless (and (appkit-media-player-session-p session)
               (eq (appkit-media-player-session-status session) 'playing)
               (process-live-p
                (appkit-media-player-session-process session)))
    (user-error "Appkit media player session is not playing"))
  (setf (appkit-media-player-session-played-seconds session)
        (appkit-media-player-played-seconds session)
        (appkit-media-player-session-started-at session) nil)
  (appkit-media-player--cancel-timer session)
  (appkit-media-player--stop-process session)
  (setf (appkit-media-player-session-status session) 'paused)
  (appkit-media-player--notify session)
  session)

(defun appkit-media-player-resume (session)
  "Resume paused SESSION with a fresh process at retained progress."
  (unless (and (appkit-media-player-session-p session)
               (eq (appkit-media-player-session-status session) 'paused)
               (not (appkit-media-player-session-finalized-p session)))
    (user-error "Appkit media player session is not paused"))
  (appkit-media-player--spawn session))

(defun appkit-media-player-toggle (session)
  "Pause or resume SESSION."
  (pcase (and (appkit-media-player-session-p session)
              (appkit-media-player-session-status session))
    ('playing (appkit-media-player-pause session))
    ('paused (appkit-media-player-resume session))
    (_ (user-error "Appkit media player session cannot be toggled"))))

(defun appkit-media-player-stop (session)
  "Stop SESSION and finalize it exactly once."
  (unless (appkit-media-player-session-p session)
    (error "Appkit media player session is invalid"))
  (if-let* ((handle (appkit-media-player-session-handle session))
            ((appkit-handle-alive-p handle)))
      (appkit-cancel-handle handle)
    (appkit-media-player--cancel session))
  session)

(provide 'appkit-media-player)

;;; appkit-media-player.el ends here
