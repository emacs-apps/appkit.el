;;; appkit-media-inline.el --- Buffer-owned inline Canvas media -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;;; Commentary:
;; Prepared text contains immutable posters and resource plans.  Overlays own
;; the displayed Canvas and interaction; video.el owns playback and leases.

;;; Code:

(require 'cl-lib)
(require 'appkit-media-image)
(require 'appkit-media-resource)
(require 'appkit-surface)
(require 'video-inline)

(cl-defstruct (appkit-media-inline-host
               (:constructor appkit-media--inline-host-create))
  buffer overlays owner occurrence plan resource animated
  pending cancel inline map owner-handle autoplay-disabled-p)

(defvar-local appkit-media--inline-hosts nil
  "Live media occurrences in this buffer.")

(defvar-local appkit-media--inline-change nil
  "Character modification tick and hosts intersecting the pending change.")

(defvar-local appkit-media--inline-start-timer nil
  "Coalesced post-render autoplay timer for this buffer.")

(defvar-local appkit-media--inline-prune-tick nil
  "Last character tick accounted for by normal modification hooks.")

(defun appkit-media-inline-host-live-p (host)
  "Return non-nil while HOST owns all its original prepared image spans."
  (and (appkit-media-inline-host-p host)
       (buffer-live-p (appkit-media-inline-host-buffer host))
       (appkit-media-inline-host-overlays host)
       (let ((buffer (appkit-media-inline-host-buffer host))
             (owner (appkit-media-inline-host-owner host))
             (occurrence (appkit-media-inline-host-occurrence host)))
         (with-current-buffer buffer
           (and (or (null owner)
                    (and (appkit-surface-live-p owner)
                         (eq owner (appkit-current-surface))))
                (cl-every
                 (lambda (overlay)
                   (and (eq (overlay-buffer overlay) buffer)
                        (eq (overlay-get overlay 'appkit-media-inline-host) host)
                        (< (overlay-start overlay) (overlay-end overlay))
                        (not (text-property-not-all
                              (overlay-start overlay) (overlay-end overlay)
                              'appkit-media-occurrence occurrence))))
                 (appkit-media-inline-host-overlays host)))))))

(defun appkit-media-inline-host-at-point (&optional position)
  "Return the live media occurrence containing POSITION in this buffer."
  (cl-loop for overlay in (overlays-at (or position (point)))
           for host = (overlay-get overlay 'appkit-media-inline-host)
           when (and (appkit-media-inline-host-live-p host)
                     (eq (current-buffer) (appkit-media-inline-host-buffer host))
                     (memq overlay (appkit-media-inline-host-overlays host)))
           return host))

(defun appkit-media--inline-visible-p (host)
  "Return a visible graphical window displaying one of HOST's spans."
  (and (appkit-media-inline-host-live-p host)
       (cl-find-if
        (lambda (window)
          (and (display-graphic-p (window-frame window))
               (eq (frame-visible-p (window-frame window)) t)
               (let ((start (window-start window))
                     (end (window-end window)))
                 (cl-some
                  (lambda (overlay)
                    (and (> (overlay-end overlay) start)
                         (or (null end) (< (overlay-start overlay) end))
                         (pos-visible-in-window-p
                          (max start (overlay-start overlay)) window t)))
                  (appkit-media-inline-host-overlays host)))))
        (get-buffer-window-list (appkit-media-inline-host-buffer host) nil t))))

(defun appkit-media--inline-close (host)
  "Cancel HOST work, remove its overlays, and release its exact lease."
  (let ((overlays (appkit-media-inline-host-overlays host))
        (handle (appkit-media-inline-host-owner-handle host))
        (cancel (appkit-media-inline-host-cancel host))
        (inline (appkit-media-inline-host-inline host)))
    ;; Detach before callbacks: retiring the owner handle calls us again.
    (setf (appkit-media-inline-host-overlays host) nil
          (appkit-media-inline-host-owner-handle host) nil
          (appkit-media-inline-host-pending host) nil
          (appkit-media-inline-host-cancel host) nil
          (appkit-media-inline-host-inline host) nil)
    (mapc #'delete-overlay overlays)
    (unwind-protect
        (unwind-protect
            (when (and handle (appkit-handle-alive-p handle))
              (appkit-retire-handle handle))
          (unwind-protect
              (when cancel (funcall cancel))
            (when inline (appkit-media-video-inline-close inline))))
      (when (buffer-live-p (appkit-media-inline-host-buffer host))
        (with-current-buffer (appkit-media-inline-host-buffer host)
          (setq appkit-media--inline-hosts
                (delq host appkit-media--inline-hosts)))))))

(defun appkit-media--inline-hosts-in-region (start end)
  "Return distinct owned hosts whose overlays intersect START..END."
  (let (hosts)
    (dolist (overlay (overlays-in start end))
      (when-let* ((host (overlay-get overlay 'appkit-media-inline-host))
                  ((appkit-media-inline-host-p host))
                  ((memq overlay (appkit-media-inline-host-overlays host)))
                  ((< (overlay-start overlay) end))
                  ((> (overlay-end overlay) start)))
        (unless (memq host hosts) (push host hosts))))
    hosts))

(defun appkit-media--inline-prune (&optional force)
  "Retire invalid spans after suppressed modification hooks.
FORCE also checks property-only changes and externally removed overlays."
  (when (or force
            (not (eql appkit-media--inline-prune-tick
                      (buffer-chars-modified-tick))))
    (setq appkit-media--inline-prune-tick (buffer-chars-modified-tick))
    (dolist (host appkit-media--inline-hosts)
      (unless (appkit-media-inline-host-live-p host)
        (appkit-media--inline-close host)))))

(defun appkit-media--inline-before-change (start end)
  "Remember owned spans intersecting the pending buffer change."
  (appkit-media--inline-prune)
  (setq appkit-media--inline-change
        (cons
         (buffer-chars-modified-tick)
         (if (< start end)
             (appkit-media--inline-hosts-in-region start end)
           (when-let* ((host (appkit-media-inline-host-at-point start)))
             (when (cl-some
                    (lambda (overlay)
                      (and (< (overlay-start overlay) start)
                           (< start (overlay-end overlay))))
                    (appkit-media-inline-host-overlays host))
               (list host)))))))

(defun appkit-media--inline-reap (&rest _ignored)
  "Retire character replacements, preserving hosts across property styling."
  (let ((change appkit-media--inline-change))
    (setq appkit-media--inline-change nil)
    (dolist (host (cdr change))
      (when (or (/= (car change) (buffer-chars-modified-tick))
                (not (appkit-media-inline-host-live-p host)))
        (appkit-media--inline-close host))))
  (setq appkit-media--inline-prune-tick (buffer-chars-modified-tick)))

(defun appkit-media--inline-release-all ()
  "Retire all hosts before this buffer changes mode or dies."
  (when (timerp appkit-media--inline-start-timer)
    (cancel-timer appkit-media--inline-start-timer))
  (setq appkit-media--inline-start-timer nil)
  (dolist (host (copy-sequence appkit-media--inline-hosts))
    (appkit-media--inline-close host)))

(defun appkit-media--inline-canvas-p (host)
  "Return non-nil if HOST's buffer has a Canvas-capable graphical window."
  (and (image-type-available-p 'canvas)
       (cl-some (lambda (window) (display-graphic-p (window-frame window)))
                (get-buffer-window-list
                 (appkit-media-inline-host-buffer host) nil t))))

(defun appkit-media--inline-show-canvas (host _inline canvas)
  "Display CANVAS through HOST's overlays without changing source text."
  (when (appkit-media-inline-host-live-p host)
    (with-current-buffer (appkit-media-inline-host-buffer host)
      (dolist (overlay (appkit-media-inline-host-overlays host))
        (let ((display (get-text-property (overlay-start overlay) 'display)))
          (overlay-put
           overlay 'display
           (list (if (eq (car-safe (car-safe display)) 'slice)
                     (car display)
                   '(slice 0 0 1.0 1.0))
                 canvas)))))))

(defun appkit-media--inline-ensure (host)
  "Create HOST's video.el lease lazily, or return its existing lease."
  (or (and (appkit-media-inline-host-inline host)
           (not (appkit-media-video-inline-closed-p
                 (appkit-media-inline-host-inline host)))
           (appkit-media-inline-host-inline host))
      (with-current-buffer (appkit-media-inline-host-buffer host)
        (let* ((plan (appkit-media-inline-host-plan host))
               (kind (appkit-media--playback-kind plan))
               (poster (appkit-media--occurrence-image
                        (appkit-media-inline-host-occurrence host)))
               (start (overlay-start (car (appkit-media-inline-host-overlays host))))
               (size (ignore-errors
                       (image-size
                        (appkit-media--display-image-spec
                         (get-text-property start 'display)) t)))
               (session
                (appkit-media-video-session-create
                 (appkit-media-inline-host-resource host)
                 (or (appkit-media--playback-label plan) "media")
                 :kind kind :owner (appkit-media-inline-host-owner host)
                 :cache-key (appkit-media--playback-cache-key plan)
                 :cache-directory (appkit-media--playback-cache-directory plan)
                 :cache-policy (if (eq kind 'image) 'none 'automatic)
                 :muted (eq kind 'image)
                 :animation-loop-policy
                 (or (appkit-media--playback-animation-loop-policy plan)
                     video-animation-loop-policy)))
               inline opened)
          (unwind-protect
              (progn
                (setq inline
                      (appkit-media-video-inline-create
                       session (max 1 (round (or (car-safe size) 320)))
                       (max 1 (round
                               (or (cdr-safe size)
                                   (* (length (appkit-media-inline-host-overlays host))
                                      (appkit-media--char-pixel-height)))))
                       :poster poster :fit (appkit-media--playback-fit plan)
                       :controls-p (appkit-media--inline-toggle-p host)
                       :visible-function (lambda (_) (appkit-media--inline-visible-p host))
                       :alive-function (lambda (_) (appkit-media-inline-host-live-p host))
                       :anchor (car (appkit-media-inline-host-overlays host))
                       :activate-function
                       (lambda (surface canvas)
                         (appkit-media--inline-show-canvas host surface canvas))
                       :close-function
                       (lambda (surface)
                         (when (eq surface (appkit-media-inline-host-inline host))
                           (setf (appkit-media-inline-host-inline host) nil)
                           (appkit-media--inline-show-canvas host surface poster)))))
                (setf (appkit-media-inline-host-inline host) inline)
                (when (appkit-media--inline-toggle-p host)
                  (appkit-media-video-inline-bind-controls
                   inline (appkit-media-inline-host-map host)))
                (setq opened t)
                inline)
            (unless opened
              (when inline (appkit-media-video-inline-close inline))
              (appkit-media-video-session-close session)))))))

(defun appkit-media--inline-toggle-p (host)
  "Whether HOST's normal action controls inline playback."
  (let ((plan (appkit-media-inline-host-plan host)))
    (and (eq (appkit-media--playback-interaction plan) 'media)
         (or (eq (appkit-media--playback-kind plan) 'video)
             (appkit-media-inline-host-animated host)))))

(defun appkit-media--inline-use (host presentation)
  "Activate HOST with resolved resource using PRESENTATION."
  (let ((plan (appkit-media-inline-host-plan host)))
    (if (not (appkit-media-inline-host-resource host))
        (unless (appkit-media-inline-host-pending host)
          (let ((resolver (appkit-media--playback-resolve-function plan))
                (request (cons nil nil)))
            (unless resolver
              (user-error "Inline media resource is not available"))
            (setf (appkit-media-inline-host-pending host) request)
            (condition-case err
                (let ((cancel
                       (funcall
                        resolver
                        (lambda (resource)
                          (when (and (eq request (appkit-media-inline-host-pending host))
                                     (appkit-media-inline-host-live-p host))
                            (setf (appkit-media-inline-host-pending host) nil
                                  (appkit-media-inline-host-cancel host) nil
                                  (appkit-media-inline-host-resource host)
                                  (appkit-media-resource-normalize resource))
                            (when (eq (appkit-media--playback-animation plan) 'auto)
                              (when-let* ((file (alist-get
                                                'file
                                                (appkit-media-inline-host-resource host))))
                                (setf (appkit-media-inline-host-animated host)
                                      (appkit-media--file-animated-p file))))
                            (appkit-media--inline-use host presentation)))
                        (lambda (reason)
                          (when (and (eq request (appkit-media-inline-host-pending host))
                                     (appkit-media-inline-host-live-p host))
                            (setf (appkit-media-inline-host-pending host) nil
                                  (appkit-media-inline-host-cancel host) nil)
                            (message "%s: %s"
                                     (or (appkit-media--playback-label plan) "media")
                                     reason))))))
                  ;; Resolution may settle or retire its host synchronously.
                  (if (eq request (appkit-media-inline-host-pending host))
                      (setf (appkit-media-inline-host-cancel host) cancel)
                    (when (and cancel (not (appkit-media-inline-host-live-p host)))
                      (funcall cancel))))
              ((error quit)
               (setf (appkit-media-inline-host-pending host) nil)
               (signal (car err) (cdr err))))))
      (let ((inline (appkit-media--inline-ensure host)))
        (pcase (or presentation
                   (unless (appkit-media--inline-toggle-p host) 'dedicated))
          ('dedicated
           (appkit-media-present-video-inline
            inline (appkit-media--playback-label plan)
            :owner (appkit-media-inline-host-owner host)))
          ('frame
           (appkit-media-present-video-inline
            inline (appkit-media--playback-label plan)
            :owner (appkit-media-inline-host-owner host)
            :display-function #'video-display-buffer-other-frame))
          (_
           (if (and (appkit-media--inline-toggle-p host)
                    (video-inline-target (appkit-media-video-inline-inline inline)))
               (appkit-media-video-inline-toggle inline)
             (appkit-media-video-inline-play inline))))))))

(defun appkit-media-inline-host-activate (host &optional presentation)
  "Activate HOST using its interaction policy, or promote to PRESENTATION.
PRESENTATION is nil, `dedicated', or `frame'.  A media interaction toggles
animated images and videos; still images and open interactions use a viewer."
  (unless (appkit-media-inline-host-live-p host)
    (user-error "Inline media belongs to a retired buffer span"))
  (unless (memq presentation '(nil dedicated frame))
    (error "Unknown inline media presentation: %S" presentation))
  (unless (appkit-media--inline-canvas-p host)
    (user-error "Canvas inline media is unavailable"))
  (appkit-media--inline-use host presentation))

(defun appkit-media--inline-start-visible (&rest _ignored)
  "Start eligible visible images after committed buffer changes."
  (appkit-media--inline-prune)
  ;; Inspect visible overlays, not every image retained in chat history.
  (let (hosts)
    (when (image-type-available-p 'canvas)
      (dolist (window (get-buffer-window-list (current-buffer) nil t))
        (when (display-graphic-p (window-frame window))
          (dolist (host (appkit-media--inline-hosts-in-region
                         (window-start window) (window-end window t)))
            (unless (memq host hosts) (push host hosts))))))
    (dolist (host hosts)
      (let ((plan (appkit-media-inline-host-plan host)))
        (cond
         ((not (appkit-media-inline-host-live-p host))
          (appkit-media--inline-close host))
         ((and (eq (appkit-media--playback-kind plan) 'image)
               appkit-media-inline-animation-enabled
               (appkit-media--playback-autoplay plan)
               (appkit-media-inline-host-animated host)
               (not (appkit-media-inline-host-autoplay-disabled-p host))
               (appkit-media-inline-host-resource host)
               (not (appkit-media-inline-host-inline host))
               (appkit-media--inline-visible-p host))
          (condition-case err
              (appkit-media--inline-use host 'inline)
            (error
             (setf (appkit-media-inline-host-autoplay-disabled-p host) t)
             (message "Inline media unavailable: %s" (error-message-string err))))))))))

(defun appkit-media--inline-schedule-start ()
  "Start visible images after the renderer yields, coalescing insertions."
  (unless (timerp appkit-media--inline-start-timer)
    (let ((buffer (current-buffer)))
      (setq appkit-media--inline-start-timer
            (run-at-time
             0 nil
             (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq appkit-media--inline-start-timer nil)
                   (appkit-media--inline-start-visible)))))))))

(defun appkit-media--inline-attach (ranges occurrence owner)
  "Create overlay-owned playback for exact RANGES of OCCURRENCE under OWNER."
  (let* ((poster (appkit-media--occurrence-image occurrence))
         (plan (plist-get (cdr poster) :appkit-media-playback))
         (interaction (appkit-media--playback-interaction plan))
         (resource (appkit-media--playback-resource plan))
         (host (appkit-media--inline-host-create
                :buffer (current-buffer) :owner owner :occurrence occurrence
                :plan plan
                :resource (and resource (appkit-media-resource-normalize resource))
                :animated (appkit-media--playback-animated-p plan)))
         (map (and interaction (make-sparse-keymap)))
         completed)
    (unwind-protect
        (progn
          (setf (appkit-media-inline-host-map host) map)
          (when map
            (dolist (key '("RET" "<return>" "<mouse-1>"))
              (define-key map (kbd key)
                          (lambda () (interactive)
                            (appkit-media-inline-host-activate host))))
            (dolist (key '("C-RET" "C-<return>"))
              (define-key map (kbd key)
                          (lambda () (interactive)
                            (appkit-media-inline-host-activate host 'dedicated))))
            (define-key map (kbd "F")
                        (lambda () (interactive)
                          (appkit-media-inline-host-activate host 'frame)))
            (when (eq interaction 'media)
              (define-key map (kbd "m")
                          (lambda () (interactive)
                            (unless (and (appkit-media-inline-host-live-p host)
                                         (appkit-media-inline-host-resource host))
                              (user-error "Activate this media before using controls"))
                            (appkit-media-video-inline-toggle-muted
                             (appkit-media--inline-ensure host))))
              (define-key map (kbd "L")
                          (lambda () (interactive)
                            (unless (and (appkit-media-inline-host-live-p host)
                                         (appkit-media-inline-host-resource host))
                              (user-error "Activate this media before using controls"))
                            (appkit-media-video-inline-toggle-loop
                             (appkit-media--inline-ensure host))))))
          (dolist (range ranges)
            (let* ((overlay (make-overlay (car range) (cdr range) nil t nil))
                   (display (get-text-property (car range) 'display)))
              (push overlay (appkit-media-inline-host-overlays host))
              (overlay-put overlay 'evaporate t)
              (overlay-put overlay 'appkit-media-inline-host host)
              ;; Distinct overlay boundaries preserve adjacent copies of the
              ;; same prepared string, even before a player has been created.
              (overlay-put overlay 'display
                           (list (if (eq (car-safe (car-safe display)) 'slice)
                                     (car display)
                                   '(slice 0 0 1.0 1.0))
                                 poster))
              (when map
                (overlay-put overlay 'keymap map)
                (overlay-put overlay 'mouse-face 'highlight)
                (overlay-put overlay 'follow-link t)
                (overlay-put overlay 'help-echo
                             (if (appkit-media--inline-toggle-p host)
                                 "RET: play/pause; C-RET: open viewer"
                               "RET: open viewer")))))
          (setf (appkit-media-inline-host-overlays host)
                (nreverse (appkit-media-inline-host-overlays host)))
          (push host appkit-media--inline-hosts)
          (add-hook 'before-change-functions #'appkit-media--inline-before-change nil t)
          (add-hook 'after-change-functions #'appkit-media--inline-reap nil t)
          (add-hook 'post-command-hook #'appkit-media--inline-start-visible nil t)
          (add-hook 'window-scroll-functions #'appkit-media--inline-start-visible nil t)
          (add-hook 'window-configuration-change-hook
                    #'appkit-media--inline-start-visible nil t)
          (add-hook 'kill-buffer-hook #'appkit-media--inline-release-all nil t)
          (add-hook 'change-major-mode-hook #'appkit-media--inline-release-all nil t)
          (when owner
            (setf (appkit-media-inline-host-owner-handle host)
                  (appkit-register-handle
                   owner 'function (lambda () (appkit-media--inline-close host)))))
          (when (and (appkit-media--playback-autoplay plan)
                     (appkit-media-inline-host-animated host))
            (appkit-media--inline-schedule-start))
          (setq completed t)
          host)
      (unless completed (appkit-media--inline-close host)))))

(cl-defun appkit-media-inline-mount (start end &optional owner)
  "Reconcile prepared media in START..END with buffer-owned overlays.
OWNER defaults to the exact current Surface; otherwise the buffer owns media.
During a Surface render, defer to its final commit boundary.  Mounting does not
modify source text, display properties, undo history, or enclosing action maps.
Each repeated insertion has independent runtime state; slices share one host.
Return newly created hosts in order."
  (setq owner (or owner (appkit-current-surface)))
  (when (and owner (eq owner appkit-surface--rendering))
    (cl-return-from appkit-media-inline-mount nil))
  (when (and owner
             (not (and (eq owner (appkit-current-surface))
                       (appkit-surface-live-p owner))))
    (error "Inline media owner must be the live Surface owning this buffer"))
  (appkit-media--inline-prune (and (= start (point-min)) (= end (point-max))))
  ;; Property-only replacement can make a previously interactive span inert.
  ;; Retire it before deciding whether its replacement needs an overlay.
  (dolist (host (appkit-media--inline-hosts-in-region start end))
    (unless (appkit-media-inline-host-live-p host)
      (appkit-media--inline-close host)))
  (unless (text-property-not-all start end 'appkit-media-occurrence nil)
    (cl-return-from appkit-media-inline-mount nil))
  (let ((position start)
        (groups (make-hash-table :test #'eq))
        order hosts)
    (while (< position end)
      (let* ((occurrence (get-text-property position 'appkit-media-occurrence))
             (next (next-single-property-change
                    position 'appkit-media-occurrence nil end))
             (plan (and occurrence
                        (plist-get (cdr (appkit-media--occurrence-image occurrence))
                                   :appkit-media-playback))))
        (when (and plan
                   (or (appkit-media--playback-interaction plan)
                       (and appkit-media-inline-animation-enabled
                            (eq (appkit-media--playback-kind plan) 'image)
                            (appkit-media--playback-autoplay plan)
                            (appkit-media--playback-animated-p plan))))
          (let ((span position))
            (while (< span next)
              (let ((limit (min (next-single-property-change span 'display nil next)
                                (+ span (appkit-media--occurrence-span-length occurrence)))))
                (when (and (not (appkit-media-inline-host-at-point span))
                           (appkit-media--display-image-spec
                            (get-text-property span 'display)))
                  (let ((group (gethash occurrence groups)))
                    (when (or (null group)
                              (= (length (cdr group))
                                 (appkit-media--occurrence-slices occurrence)))
                      (setq group (list occurrence))
                      (puthash occurrence group groups)
                      (push group order))
                    (push (cons span limit) (cdr group))))
                (setq span limit)))))
        (setq position next)))
    (dolist (group (nreverse order))
      (push (appkit-media--inline-attach
             (nreverse (cdr group)) (car group) owner)
            hosts))
    (nreverse hosts)))

(provide 'appkit-media-inline)
;;; appkit-media-inline.el ends here
