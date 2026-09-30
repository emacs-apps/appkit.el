;;; appkit-media-inline.el --- Buffer-owned inline Canvas media -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;;; Commentary:
;; Own the exact image display spans inserted by an application, while video.el
;; owns playback, Canvas targets, and the shared presentation leases.

;;; Code:

(require 'cl-lib)
(require 'appkit-media-image)
(require 'appkit-media-resource)
(require 'appkit-surface)
(require 'video-inline)

(cl-defstruct (appkit-media-inline-host
               (:constructor appkit-media--inline-host-create))
  buffer ranges token owner kind resource poster label cache-key cache-directory
  autoplay toggle-p resolve-function pending cancel inline map owner-handle
  animation-loop-policy)

(defvar-local appkit-media--inline-hosts nil
  "Live media occurrences in this buffer.")

(defvar-local appkit-media--inline-change nil
  "Character modification tick and hosts intersecting the pending change.")

(defvar-local appkit-media--inline-start-timer nil
  "Coalesced post-render autoplay timer for this buffer.")

(defun appkit-media-inline-static-poster (image)
  "Return IMAGE's static poster without changing its descriptor or pixels.
Already static Canvas posters retain their backing-store identity; live
Canvases are snapshotted.  Other images are copied without the GIF-animation
flag and remain normal static images on non-Canvas displays."
  (if (eq (plist-get (cdr-safe image) :type) 'canvas)
      (if (plist-get (cdr image) :video-static-poster)
          image
        (video-canvas-copy image))
    (if (eq (car-safe image) 'image)
        (let ((copy (cons 'image (copy-sequence (cdr image)))))
          (plist-put (cdr copy) :appkit-media-inline-animation nil)
          copy)
      image)))

(defun appkit-media-inline-host-live-p (host)
  "Return non-nil while HOST owns every original image display span."
  (and (appkit-media-inline-host-p host)
       (buffer-live-p (appkit-media-inline-host-buffer host))
       (appkit-media-inline-host-ranges host)
       (let ((buffer (appkit-media-inline-host-buffer host))
             (owner (appkit-media-inline-host-owner host))
             (token (appkit-media-inline-host-token host)))
         (with-current-buffer buffer
           (and (or (null owner)
                    (and (appkit-surface-live-p owner)
                         (eq owner (appkit-current-surface))))
                (cl-every
                 (lambda (range)
                   (let ((start (marker-position (car range)))
                         (end (marker-position (cdr range))))
                     (and (eq (marker-buffer (car range)) buffer)
                          (eq (marker-buffer (cdr range)) buffer)
                          start end (< start end)
                          (eq (get-text-property start 'appkit-media-inline-token)
                              token)
                          (eq (get-text-property (1- end)
                                                 'appkit-media-inline-token)
                              token)
                          (let ((change
                                 (next-single-property-change
                                  start 'appkit-media-inline-token nil end)))
                            (or (null change) (>= change end))))))
                 (appkit-media-inline-host-ranges host)))))))

(defun appkit-media-inline-host-at-point (&optional position)
  "Return the live inline media host whose exact image span contains POSITION.
POSITION defaults to point in the current buffer."
  (let* ((position (or position (point)))
         (host (get-text-property position 'appkit-media-inline-host)))
    (when (and (appkit-media-inline-host-live-p host)
               (cl-some (lambda (range)
                          (and (<= (car range) position)
                               (< position (cdr range))))
                        (appkit-media-inline-host-ranges host)))
      host)))

(defun appkit-media--inline-visible-p (host)
  "Return a window displaying a HOST image span, or nil.
Reject offscreen slices by buffer positions before asking Emacs for pixel
visibility.  The returned window also owns this occurrence's background."
  (and (appkit-media-inline-host-live-p host)
       (let ((buffer (appkit-media-inline-host-buffer host)))
         (cl-find-if
          (lambda (window)
            (let ((start (window-start window))
                  (end (window-end window)))
              (cl-some
               (lambda (range)
                 (and (> (cdr range) start)
                      (or (null end) (< (car range) end))
                      (pos-visible-in-window-p
                       (max start (car range)) window t)))
               (appkit-media-inline-host-ranges host))))
          (get-buffer-window-list buffer nil t)))))

(defun appkit-media--inline-close (host)
  "Cancel HOST work and release its exact inline lease."
  (when-let* ((handle (appkit-media-inline-host-owner-handle host)))
    (setf (appkit-media-inline-host-owner-handle host) nil)
    (when (appkit-handle-alive-p handle)
      (appkit-retire-handle handle)))
  (let ((cancel (appkit-media-inline-host-cancel host))
        (ranges (appkit-media-inline-host-ranges host)))
    (setf (appkit-media-inline-host-pending host) nil
          (appkit-media-inline-host-cancel host) nil
          (appkit-media-inline-host-ranges host) nil)
    (dolist (range ranges)
      (set-marker (car range) nil)
      (set-marker (cdr range) nil))
    (when cancel (funcall cancel)))
  (when-let* ((inline (appkit-media-inline-host-inline host)))
    (setf (appkit-media-inline-host-inline host) nil)
    (appkit-media-video-inline-close inline))
  (when (buffer-live-p (appkit-media-inline-host-buffer host))
    (with-current-buffer (appkit-media-inline-host-buffer host)
      (setq appkit-media--inline-hosts
            (delq host appkit-media--inline-hosts)))))

(defun appkit-media--inline-hosts-in-region (start end)
  "Return distinct hosts attached to text in START..END."
  (let (hosts)
    (while (< start end)
      (when-let* ((host (get-text-property start 'appkit-media-inline-host)))
        (unless (memq host hosts)
          (push host hosts)))
      (setq start
            (next-single-property-change start 'appkit-media-inline-host nil end)))
    hosts))

(defun appkit-media--inline-before-change (start end)
  "Remember hosts whose exact spans overlap a change at START..END.
Character ticks distinguish row replacement from harmless property styling."
  (setq appkit-media--inline-change
        (cons
         (buffer-chars-modified-tick)
         (if (< start end)
             (appkit-media--inline-hosts-in-region start end)
           (when-let* ((host (get-text-property start 'appkit-media-inline-host)))
             (when (cl-some
                    (lambda (range)
                      (and (< (car range) start) (< start (cdr range))))
                    (appkit-media-inline-host-ranges host))
               (list host)))))))

(defun appkit-media--inline-reap (&rest _ignored)
  "Retire replaced text spans, but preserve hosts across property styling."
  (let ((change appkit-media--inline-change))
    (setq appkit-media--inline-change nil)
    (dolist (host (cdr change))
      (when (or (/= (car change) (buffer-chars-modified-tick))
                (not (appkit-media-inline-host-live-p host)))
        (appkit-media--inline-close host)))))

(defun appkit-media--inline-release-all ()
  "Retire all hosts before this buffer changes mode or dies."
  (when (timerp appkit-media--inline-start-timer)
    (cancel-timer appkit-media--inline-start-timer))
  (setq appkit-media--inline-start-timer nil)
  (dolist (host (copy-sequence appkit-media--inline-hosts))
    (appkit-media--inline-close host)))

(defun appkit-media--inline-canvas-p (host)
  "Return non-nil if HOST can display a Canvas in a graphical window."
  (and (image-type-available-p 'canvas)
       (cl-some (lambda (window) (display-graphic-p (window-frame window)))
                (get-buffer-window-list
                 (appkit-media-inline-host-buffer host) nil t))))

(defun appkit-media--inline-show-canvas (host _inline canvas)
  "Replace HOST's image spans with CANVAS, preserving unsliced image height."
  (when (appkit-media-inline-host-live-p host)
    (with-current-buffer (appkit-media-inline-host-buffer host)
      (let* ((ranges (appkit-media-inline-host-ranges host))
             (count (length ranges))
             (inhibit-read-only t))
        (with-silent-modifications
          (if (= count 1)
              (put-text-property (caar ranges) (cdar ranges) 'display canvas)
            (plist-put (cdr canvas) :appkit-media-nslices count)
            (let ((rows (appkit-media-image-slice-rows canvas)))
              (unless (= count (length rows))
                (error "Inline Canvas changed its image slice geometry"))
              (cl-mapc (lambda (range row)
                         (put-text-property
                          (car range) (cdr range) 'display
                          (get-text-property 0 'display row)))
                       ranges rows))))))))

(defun appkit-media--inline-ensure (host)
  "Create HOST's video.el lease lazily, or return the existing one."
  (or (and (appkit-media-inline-host-inline host)
           (not (appkit-media-video-inline-closed-p
                 (appkit-media-inline-host-inline host)))
           (appkit-media-inline-host-inline host))
      (with-current-buffer (appkit-media-inline-host-buffer host)
        (let* ((kind (appkit-media-inline-host-kind host))
               (poster (appkit-media-inline-host-poster host))
               ;; Slice insertion has already resolved cached Nch geometry.
               ;; Measure that displayed image, not the unscaled source poster.
               (size (ignore-errors
                       (image-size
                        (appkit-media--display-image-spec
                         (get-text-property
                          (caar (appkit-media-inline-host-ranges host))
                          'display))
                        t)))
               (session
                (appkit-media-video-session-create
                 (appkit-media-inline-host-resource host)
                 (appkit-media-inline-host-label host)
                 :kind kind
                 :owner (appkit-media-inline-host-owner host)
                 :cache-key (appkit-media-inline-host-cache-key host)
                 :cache-directory (appkit-media-inline-host-cache-directory host)
                 :cache-policy (if (eq kind 'image) 'none 'automatic)
                 :muted (eq kind 'image)
                 :animation-loop-policy
                 (appkit-media-inline-host-animation-loop-policy host)))
               inline opened)
          (unwind-protect
              (progn
                (setq inline
                      (appkit-media-video-inline-create
                       session (max 1 (round (or (car-safe size) 320)))
                       (max 1 (round
                               (or (cdr-safe size)
                                   (* (length (appkit-media-inline-host-ranges host))
                                      (appkit-media--char-pixel-height)))))
                       :poster poster
                       :visible-function
                       (lambda (_inline)
                         (appkit-media--inline-visible-p host))
                       :alive-function
                       (lambda (_inline)
                         (appkit-media-inline-host-live-p host))
                       :anchor (copy-marker
                                (caar (appkit-media-inline-host-ranges host)) t)
                       :activate-function
                       (lambda (surface canvas)
                         (appkit-media--inline-show-canvas host surface canvas))
                       :close-function
                       (lambda (surface)
                         (when (eq surface
                                   (appkit-media-inline-host-inline host))
                           (setf (appkit-media-inline-host-inline host) nil)
                           (appkit-media--inline-show-canvas
                            host surface
                            (appkit-media-inline-host-poster host))))))
                (setf (appkit-media-inline-host-inline host) inline)
                (appkit-media-video-inline-bind-controls
                 inline (appkit-media-inline-host-map host))
                (setq opened t)
                inline)
            (unless opened
              (when inline (appkit-media-video-inline-close inline))
              (appkit-media-video-session-close session)))))))

(defun appkit-media--inline-use (host presentation)
  "Activate HOST with resolved resource using PRESENTATION."
  (if (not (appkit-media-inline-host-resource host))
      (unless (appkit-media-inline-host-pending host)
        (let ((resolver (appkit-media-inline-host-resolve-function host))
              (request (cons nil nil)))
          (unless resolver
            (user-error "Inline media resource is not available"))
          (setf (appkit-media-inline-host-pending host) request)
          (condition-case err
              (let ((cancel
                     (funcall
                      resolver
                      (lambda (resource)
                        (when (and (eq request
                                       (appkit-media-inline-host-pending host))
                                   (appkit-media-inline-host-live-p host))
                          (setf (appkit-media-inline-host-pending host) nil
                                (appkit-media-inline-host-cancel host) nil
                                (appkit-media-inline-host-resource host)
                                (appkit-media-resource-normalize resource))
                          (appkit-media-inline-host-activate host presentation)))
                      (lambda (reason)
                        (when (and (eq request
                                       (appkit-media-inline-host-pending host))
                                   (appkit-media-inline-host-live-p host))
                          (setf (appkit-media-inline-host-pending host) nil
                                (appkit-media-inline-host-cancel host) nil)
                          (message "%s: %s"
                                   (appkit-media-inline-host-label host)
                                   reason))))))
                ;; Synchronous resolution may already have completed.
                (if (eq request (appkit-media-inline-host-pending host))
                    (setf (appkit-media-inline-host-cancel host) cancel)
                  (when (and cancel
                             (not (appkit-media-inline-host-live-p host)))
                    (funcall cancel))))
            ((error quit)
             (setf (appkit-media-inline-host-pending host) nil)
             (signal (car err) (cdr err))))))
    (let ((inline (appkit-media--inline-ensure host)))
      (pcase presentation
        ('dedicated
         (appkit-media-present-video-inline
          inline (appkit-media-inline-host-label host)
          :owner (appkit-media-inline-host-owner host)))
        ('frame
         (appkit-media-present-video-inline
          inline (appkit-media-inline-host-label host)
          :owner (appkit-media-inline-host-owner host)
          :display-function #'video-display-buffer-other-frame))
        (_
         (if (and (appkit-media-inline-host-toggle-p host)
                  (video-inline-target
                   (appkit-media-video-inline-inline inline)))
             (appkit-media-video-inline-toggle inline)
           (appkit-media-video-inline-play inline)))))))

(defun appkit-media-inline-host-activate (host &optional presentation)
  "Activate HOST inline, or promote its exact session to PRESENTATION.
PRESENTATION is nil, `dedicated', or `frame'.  Static images whose
TOGGLE-P is nil open a dedicated viewer on ordinary activation."
  (unless (appkit-media-inline-host-live-p host)
    (user-error "Inline media belongs to a retired buffer span"))
  (unless (memq presentation '(nil dedicated frame))
    (error "Unknown inline media presentation: %S" presentation))
  (unless (appkit-media--inline-canvas-p host)
    (user-error "Canvas inline media is unavailable"))
  (appkit-media--inline-use
   host (or presentation
            (unless (appkit-media-inline-host-toggle-p host) 'dedicated))))

(defun appkit-media--inline-start-visible (&rest _ignored)
  "Start Canvas-capable, visible images after committed buffer changes."
  ;; Inspect displayed text, not every media occurrence in retained history.
  ;; In particular, typing in the composer must not validate all image slices.
  (let (hosts)
    (when (image-type-available-p 'canvas)
      (dolist (window (get-buffer-window-list (current-buffer) nil t))
        (when (display-graphic-p (window-frame window))
          (dolist (host (appkit-media--inline-hosts-in-region
                         (window-start window)
                         (window-end window t)))
            (unless (memq host hosts)
              (push host hosts))))))
    (dolist (host hosts)
      (cond
       ((not (appkit-media-inline-host-live-p host))
        (appkit-media--inline-close host))
       ((and (eq (appkit-media-inline-host-kind host) 'image)
             (appkit-media-inline-host-autoplay host)
             (appkit-media-inline-host-resource host)
             (not (appkit-media-inline-host-inline host))
             (appkit-media--inline-visible-p host))
        (condition-case err
            (appkit-media--inline-use host nil)
          (error
           (setf (appkit-media-inline-host-autoplay host) nil)
           (message "Inline media unavailable: %s"
                    (error-message-string err)))))))))

(defun appkit-media--inline-schedule-start ()
  "Discover visible images after this render, even without another command."
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

(cl-defun appkit-media-inline-host-attach
    (start end poster resource &key (kind 'image) owner label cache-key
           cache-directory autoplay toggle-p resolve-function
           (animation-loop-policy video-animation-loop-policy))
  "Own all image display spans in START..END; return a host or nil.
Call after inserting POSTER, before or after line-prefix properties change.
POSTER must be an immutable static image spec; use
`appkit-media-inline-static-poster' to copy an animated source first.
RESOURCE is a canonical Appkit media alist or nil; KIND is `image' or
`video'.  OWNER, if non-nil, must be the exact live Appkit Surface.
ANIMATION-LOOP-POLICY is captured on first activation; ordinary images use
their file policy.  A host such as a repeating face may select `forever'.
AUTOPLAY starts only visible Canvas-capable images after rendering or
scrolling, never videos.  TOGGLE-P selects inline RET/click playback rather than a
viewer.  RESOLVE-FUNCTION, if RESOURCE is nil, is invoked only on activation
with (SUCCESS FAILURE).  SUCCESS receives a canonical resource alist;
FAILURE receives a human-readable reason string.  It returns a cancellation
function or nil; this function is called on retirement while pending.
Callbacks after retirement or replacement are ignored."
  (unless (memq kind '(image video))
    (error "Inline media kind must be image or video"))
  (unless (eq (car-safe poster) 'image)
    (error "Inline media poster must be an image spec"))
  (when (and owner
             (or (not (appkit-surface-live-p owner))
                 (not (eq owner (appkit-current-surface)))))
    (error "Inline media owner must own this buffer"))
  (when (and resolve-function (not (functionp resolve-function)))
    (error "Inline media resolver must be callable"))
  (let ((position start) ranges)
    (while (< position end)
      (let* ((display (get-text-property position 'display))
             (next (or (next-single-property-change position 'display nil end)
                       end)))
        (when (appkit-media--display-image-spec display)
          (push (cons (copy-marker position t) (copy-marker next)) ranges))
        (setq position next)))
    (when ranges
      (setq ranges (nreverse ranges))
      (let* ((host (appkit-media--inline-host-create
                    :buffer (current-buffer) :ranges ranges
                    :token (cons 'appkit-inline nil) :owner owner :kind kind
                    :resource (and resource
                                   (appkit-media-resource-normalize resource))
                    :poster (appkit-media-inline-static-poster poster)
                    :label (or label "media") :cache-key cache-key
                    :cache-directory cache-directory
                    :autoplay (and autoplay (eq kind 'image))
                    :toggle-p (or (eq kind 'video) toggle-p)
                    :animation-loop-policy animation-loop-policy
                    :resolve-function resolve-function))
             (map (make-sparse-keymap)))
        (setf (appkit-media-inline-host-map host) map)
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
                       (appkit-media--inline-ensure host))))
        (dolist (range ranges)
          (add-text-properties
           (car range) (cdr range)
           (list 'appkit-media-inline-token (appkit-media-inline-host-token host)
                 'appkit-media-inline-host host 'keymap map
                 'help-echo (if (eq kind 'video)
                                "RET: play/pause; C-RET: view video"
                              "RET: open or replay; C-RET: view image")
                 'mouse-face 'highlight 'follow-link t)))
        (push host appkit-media--inline-hosts)
        (add-hook 'before-change-functions
                  #'appkit-media--inline-before-change nil t)
        (add-hook 'after-change-functions #'appkit-media--inline-reap nil t)
        (add-hook 'post-command-hook #'appkit-media--inline-start-visible nil t)
        (add-hook 'window-scroll-functions
                  #'appkit-media--inline-start-visible nil t)
        (add-hook 'window-configuration-change-hook
                  #'appkit-media--inline-start-visible nil t)
        (add-hook 'kill-buffer-hook #'appkit-media--inline-release-all nil t)
        (add-hook 'change-major-mode-hook
                  #'appkit-media--inline-release-all nil t)
        (when owner
          (setf (appkit-media-inline-host-owner-handle host)
                (appkit-register-handle
                 owner 'function
                 (lambda () (appkit-media--inline-close host)))))
        (when (appkit-media-inline-host-autoplay host)
          (appkit-media--inline-schedule-start))
        host))))

(provide 'appkit-media-inline)
;;; appkit-media-inline.el ends here
