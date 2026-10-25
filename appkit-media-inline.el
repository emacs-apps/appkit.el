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

(autoload 'appkit-media--track-mount "appkit-media-track")
(declare-function appkit-media--track-descriptor-plan "appkit-media-track" (descriptor))
(declare-function appkit-media-track-poster "appkit-media-track" (track))
(declare-function appkit-media-track-index "appkit-media-track" (track))
(declare-function appkit-media-track-inline-index "appkit-media-track" (track))
(declare-function appkit-media--track-layout "appkit-media-track" (track))
(declare-function appkit-media--track-scene "appkit-media-track" (track))
(autoload 'appkit-media-track-activate "appkit-media-track")

(cl-defstruct (appkit-media-inline-host
               (:constructor appkit-media--inline-host-create))
  buffer overlays owner occurrence plan resource animated
  pending cancel inline map owner-handle autoplay-disabled-p track
  thumbnail identity parked resume layout)

(defvar-local appkit-media--inline-hosts nil
  "Live media occurrences in this buffer.")

(defvar-local appkit-media--inline-change nil
  "Character modification tick and hosts intersecting the pending change.")

(defvar-local appkit-media--inline-start-timer nil
  "Coalesced post-render autoplay timer for this buffer.")

(defvar-local appkit-media--inline-prune-tick nil
  "Last character tick accounted for by normal modification hooks.")

(defvar-local appkit-media--inline-parked nil
  "Hosts detached by their owner's render, awaiting another occurrence.
Their owner handle, pending resolver and playback lease remain owned.")

(defvar appkit-media--inline-image-hosts nil
  "Hosts retaining inline image playback across buffers.")

(defvar appkit-media--inline-hidden-timer nil
  "Coalesced release of image playback after window state changes.")

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
                         (or (null end) (< (overlay-start overlay) end))))
                  (appkit-media-inline-host-overlays host)))))
        (get-buffer-window-list (appkit-media-inline-host-buffer host) nil t))))

(defun appkit-media--inline-close (host)
  "Fence HOST callbacks, remove its spans and release its work exactly once."
  (setq appkit-media--inline-image-hosts
        (delq host appkit-media--inline-image-hosts))
  (let ((overlays (appkit-media-inline-host-overlays host))
        (handle (appkit-media-inline-host-owner-handle host))
        (cancel (appkit-media-inline-host-cancel host))
        (inline (appkit-media-inline-host-inline host)))
    (setf (appkit-media-inline-host-overlays host) nil
          (appkit-media-inline-host-owner-handle host) nil
          (appkit-media-inline-host-pending host) nil
          (appkit-media-inline-host-cancel host) nil
          (appkit-media-inline-host-inline host) nil
          (appkit-media-inline-host-thumbnail host) nil
          (appkit-media-inline-host-parked host) nil
          (appkit-media-inline-host-resume host) nil)
    (when (buffer-live-p (appkit-media-inline-host-buffer host))
      (with-current-buffer (appkit-media-inline-host-buffer host)
        (setq appkit-media--inline-hosts (delq host appkit-media--inline-hosts)
              appkit-media--inline-parked (delq host appkit-media--inline-parked))))
    (mapc #'delete-overlay overlays)
    (unwind-protect
        (when (and handle (appkit-handle-alive-p handle))
          (appkit-retire-handle handle))
      (unwind-protect
          (when cancel (funcall cancel))
        (when inline (appkit-media-video-inline-close inline))))))

(defun appkit-media--inline-retainable-p (host)
  "Whether HOST may survive its exact owner's current render."
  (and appkit-surface--rendering
       (eq appkit-surface--rendering (appkit-media-inline-host-owner host))
       (appkit-media-inline-host-identity host)
       (appkit-surface-live-p appkit-surface--rendering)))

(defun appkit-media--inline-release-parked ()
  "Release every presentation not claimed by its completed render.
Continue releasing other hosts even if a resolver cancellation fails."
  (let ((parked appkit-media--inline-parked))
    (setq appkit-media--inline-parked nil)
    (appkit-media--inline-close-hosts parked)))

(defun appkit-media--inline-retire (host)
  "Detach retainable HOST's spans, or close it when truly removed."
  (if (not (appkit-media--inline-retainable-p host))
      (appkit-media--inline-close host)
    (unless (memq host appkit-media--inline-parked)
      (let ((overlays (appkit-media-inline-host-overlays host))
            (buffer (current-buffer)))
        (setf (appkit-media-inline-host-parked host) t
              (appkit-media-inline-host-overlays host) nil)
        (setq appkit-media--inline-hosts (delq host appkit-media--inline-hosts))
        (mapc #'delete-overlay overlays)
        (unless appkit-media--inline-parked
          ;; A renderer that never reaches its commit boundary still releases
          ;; leases.  The owner handle also remains valid while parked.
          (run-at-time 0 nil
                       (lambda ()
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (appkit-media--inline-release-parked))))))
        (setq appkit-media--inline-parked
              (append appkit-media--inline-parked (list host)))))))

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
      (if (appkit-media-inline-host-live-p host)
          (setf (appkit-media-inline-host-parked host) nil)
        (appkit-media--inline-retire host)))))

(defun appkit-media--inline-before-change (start end)
  "Remember owned spans intersecting the pending buffer change.
Mark retainable hosts parked first: video.el's own change hook must not
close their inline before the reap below decides between park and close."
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
               (list host))))))
  (dolist (host (cdr appkit-media--inline-change))
    (when (appkit-media--inline-retainable-p host)
      (setf (appkit-media-inline-host-parked host) t))))

(defun appkit-media--inline-reap (&rest _ignored)
  "Retire character replacements, preserving hosts across property styling."
  (let ((change appkit-media--inline-change))
    (setq appkit-media--inline-change nil)
    (dolist (host (cdr change))
      (if (or (/= (car change) (buffer-chars-modified-tick))
              (not (appkit-media-inline-host-live-p host)))
          (appkit-media--inline-retire host)
        (setf (appkit-media-inline-host-parked host) nil))))
  (setq appkit-media--inline-prune-tick (buffer-chars-modified-tick)))

(defun appkit-media--inline-release-all ()
  "Retire all hosts before this buffer changes mode or dies."
  (when (timerp appkit-media--inline-start-timer)
    (cancel-timer appkit-media--inline-start-timer))
  (setq appkit-media--inline-start-timer nil)
  (appkit-media--inline-close-hosts
   (delete-dups (append appkit-media--inline-hosts appkit-media--inline-parked))))

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
      (when-let* ((thumbnail (appkit-media--inline-thumbnail host))
                  ((eq (plist-get (cdr canvas) :type) 'canvas)))
        ;; Preserve authored aspect and line-relative sizing for live canvases
        ;; and refreshed frozen posters alike.
        (plist-put (cdr canvas) :width
                   (cons (/ (float (appkit-media--thumbnail-width thumbnail))
                            (appkit-media--thumbnail-height thumbnail)) 'ch))
        (plist-put (cdr canvas) :height '(1 . ch))
        (plist-put (cdr canvas) :scale 1.0))
      (dolist (overlay (appkit-media-inline-host-overlays host))
        (let ((display (get-text-property (overlay-start overlay) 'display)))
          (overlay-put
           overlay 'display
           (list (if (eq (car-safe (car-safe display)) 'slice)
                     (car display)
                   '(slice 0 0 1.0 1.0))
                 canvas)))))))

(defun appkit-media--inline-thumbnail (host)
  "Return HOST's immutable thumbnail descriptor, if any."
  (or (appkit-media-inline-host-thumbnail host)
      (plist-get
       (cdr (appkit-media--occurrence-image
             (appkit-media-inline-host-occurrence host)))
       :appkit-media-thumbnail)))

(defun appkit-media--inline-refresh-thumbnail (host)
  "Reselect HOST's frozen poster when its effective background changes.
This only queues a bounded first-frame job; it never decodes synchronously."
  (when (and (not (appkit-media-inline-host-inline host))
             (appkit-media--inline-visible-p host))
    (when-let* ((thumbnail (appkit-media--inline-thumbnail host)))
      (let ((background
             (video-background-color
              (appkit-media-inline-host-buffer host)
              (overlay-start (car (appkit-media-inline-host-overlays host))))))
        (unless (equal background (appkit-media--thumbnail-background thumbnail))
          (setq thumbnail
                (appkit-media--thumbnail-get
                 (appkit-media--thumbnail-source thumbnail)
                 (appkit-media--thumbnail-width thumbnail)
                 (appkit-media--thumbnail-height thumbnail)
                 (appkit-media--thumbnail-fit thumbnail) background))
          (setf (appkit-media-inline-host-thumbnail host) thumbnail)
          (appkit-media--inline-show-canvas
           host nil (appkit-media--thumbnail-poster thumbnail)))))))

(defun appkit-media--inline-alive-p (host)
  "Whether HOST still owns a live or temporarily parked presentation."
  (and (appkit-media-inline-host-p host)
       (buffer-live-p (appkit-media-inline-host-buffer host))
       (with-current-buffer (appkit-media-inline-host-buffer host)
         (let ((owner (appkit-media-inline-host-owner host)))
           (and (or (null owner)
                    (and (appkit-surface-live-p owner)
                         (eq owner (appkit-current-surface))))
                (or (appkit-media-inline-host-parked host)
                    (appkit-media-inline-host-live-p host)))))))

(defun appkit-media--inline-identity (image)
  "Return IMAGE's stable slot and content identity, excluding its geometry.
A resolver-only plan needs a cache key: a new resolver closure is neither
proof of new content nor proof of the same content."
  (when-let* ((slot (plist-get (cdr image) :appkit-media-slot-key)))
    (cond
     ((plist-get (cdr image) :appkit-media-track)
      (let ((descriptor (plist-get (cdr image) :appkit-media-track)))
        (list slot 'track
              (mapcar (lambda (cell)
                        (let ((item (plist-get cell :item)))
                          (list (plist-get item :file)
                                (plist-get item :playable-p)
                                (plist-get item :fit))))
                      (plist-get (appkit-media--track-descriptor-plan descriptor) :items)))))
     ((plist-get (cdr image) :appkit-media-playback)
      (let* ((plan (plist-get (cdr image) :appkit-media-playback))
             (resource (appkit-media-playback-resource plan))
             (content (or (and (appkit-media-playback-cache-key plan)
                               (list 'cache (appkit-media-playback-cache-key plan)))
                          (and (alist-get 'url resource)
                               (list 'url (alist-get 'url resource)))
                          (and (alist-get 'file resource)
                               (list 'file (alist-get 'file resource))))))
        (when content
          (list slot (appkit-media-playback-kind plan) content
                (appkit-media-playback-fit plan)
                (appkit-media-playback-live plan)
                (appkit-media-playback-animation plan)
                (appkit-media-playback-animation-loop-policy plan))))))))

(defun appkit-media--inline-adopt (occurrence owner)
  "Claim a parked host of OWNER for OCCURRENCE's stable slot and content."
  (let* ((image (appkit-media--occurrence-image occurrence))
         (identity (appkit-media--inline-identity image)))
    (when-let* ((host (and identity
                           (cl-find-if
                            (lambda (candidate)
                              (and (eq owner (appkit-media-inline-host-owner candidate))
                                   (appkit-media--inline-alive-p candidate)
                                   (equal identity (appkit-media-inline-host-identity candidate))))
                            appkit-media--inline-parked))))
      (setq appkit-media--inline-parked (delq host appkit-media--inline-parked))
      host)))

(defun appkit-media--inline-poster (host)
  "Return the current poster for HOST, including its current track selection."
  (if-let* ((track (appkit-media-inline-host-track host)))
      (appkit-media-track-poster track)
    (if-let* ((thumbnail (appkit-media-inline-host-thumbnail host)))
        (appkit-media--thumbnail-poster thumbnail)
      (appkit-media--image-poster
       (appkit-media--occurrence-image (appkit-media-inline-host-occurrence host))))))

(defun appkit-media--inline-display-size (host poster &optional intrinsic)
  "Measure HOST's POSTER in current displayed pixels.
INTRINSIC supplies a composite scene's authored width and height.  Its
aspect follows the same display height used by ordinary image occurrences."
  (let* ((thumbnail (appkit-media--inline-thumbnail host))
         (line (appkit-media--char-pixel-height))
         (height-spec (plist-get (cdr poster) :height))
         (display-height
          (cond ((numberp height-spec) height-spec)
                ((and (consp height-spec) (eq (cdr height-spec) 'ch))
                 (* (car height-spec) line))))
         (frame (when-let* ((window (appkit-geometry-display-window)))
                  (window-frame window)))
         (size
          (cond
           (thumbnail
            (cons (* line (/ (float (appkit-media--thumbnail-width thumbnail))
                            (appkit-media--thumbnail-height thumbnail))) line))
           (intrinsic
            (let ((height (or display-height (cdr intrinsic))))
              (cons (* height (/ (float (car intrinsic)) (cdr intrinsic))) height)))
           (t (or (ignore-errors (image-size poster t frame))
                  (cons (let ((width (plist-get (cdr poster) :width)))
                          (and (numberp width) width))
                        display-height))))))
    (cons (max 1 (round (or (car-safe size) 320)))
          (max 1 (round (or (cdr-safe size)
                            (* (length (appkit-media-inline-host-overlays host)) line)))))))

(defun appkit-media--inline-layout (host)
  "Return HOST's effective display layout, suitable for equality comparison.
Content identity is separate.  The layout includes composite cell geometry
so a reprint with unchanged dimensions and selection keeps its native scene."
  (if-let* ((track (appkit-media-inline-host-track host)))
      (let* ((layout (appkit-media--track-layout track))
             (index (appkit-media-track-index track))
             (cell (nth index (plist-get layout :cells))))
        (list :width (plist-get cell :width) :height (plist-get layout :height)
              :canvas-width (plist-get layout :width)
              :canvas-height (plist-get layout :height)
              :destination-x (plist-get cell :x)
              :cells (plist-get layout :cells) :index index))
    (let ((size (appkit-media--inline-display-size host (appkit-media--inline-poster host))))
      (list :width (car size) :height (cdr size)))))

(defun appkit-media--inline-placed (host)
  "Relocate HOST's retained lease, rebuilding only changed display geometry."
  (let* ((track (appkit-media-inline-host-track host))
         (surface (appkit-media-inline-host-inline host))
         (inline (and surface (not (appkit-media-video-inline-closed-p surface))
                      (appkit-media-video-inline-inline surface)))
         (poster (appkit-media--inline-poster host)))
    (when inline
      (video-inline-relocate inline (car (appkit-media-inline-host-overlays host)))
      (let ((layout (appkit-media--inline-layout host)))
        (if (equal layout (appkit-media-inline-host-layout host))
            (setf (video-inline-poster inline) poster)
          (if track
              (video-inline-set-geometry
               inline (plist-get layout :width) (plist-get layout :height)
               :poster poster :canvas (appkit-media--track-scene track)
               :canvas-width (plist-get layout :canvas-width)
               :canvas-height (plist-get layout :canvas-height)
               :destination-x (plist-get layout :destination-x))
            (video-inline-set-geometry
             inline (plist-get layout :width) (plist-get layout :height) :poster poster))
          (setf (appkit-media-inline-host-layout host) layout)))
      (when (appkit-media-inline-host-map host)
        (appkit-media-video-inline-bind-controls surface (appkit-media-inline-host-map host))))
    (appkit-media--inline-show-canvas
     host inline
     (if (and inline (video-inline-active inline) (video-inline-target inline))
         (video-target-canvas (video-inline-target inline))
       poster))
    (when-let* ((resume (appkit-media-inline-host-resume host)))
      (setf (appkit-media-inline-host-resume host) nil)
      (appkit-media--inline-use host (car resume)))))

(defun appkit-media--inline-close-hosts (hosts)
  "Close all HOSTS, reporting failures only after every lease is released."
  (let (conditions)
    (appkit--run-cleanup-items
     hosts #'appkit-media--inline-close (lambda (condition) (push condition conditions)))
    (setq conditions (nreverse conditions))
    (appkit--warn-cleanup-conditions (cdr conditions) 'appkit-media)
    (when-let* ((condition (car conditions)))
      (signal (car condition) (cdr condition)))))

(defun appkit-media--inline-ensure (host)
  "Create HOST's playback lease lazily, or return its existing lease."
  (let ((existing (appkit-media-inline-host-inline host)))
    (or (and existing (not (appkit-media-video-inline-closed-p existing)) existing)
        (with-current-buffer (appkit-media-inline-host-buffer host)
          (let* ((plan (appkit-media-inline-host-plan host))
                 (kind (appkit-media-playback-kind plan))
                 (poster (appkit-media--inline-poster host))
                 (thumbnail (appkit-media--inline-thumbnail host))
                 (size (appkit-media--inline-display-size host poster))
                 (width (car size))
                 (height (cdr size))
                 (session
                  (appkit-media-video-session-create
                   (appkit-media-inline-host-resource host)
                   (or (appkit-media-playback-label plan) "media")
                   :kind kind :owner (appkit-media-inline-host-owner host)
                   :cache-key (appkit-media-playback-cache-key plan)
                   :cache-directory (appkit-media-playback-cache-directory plan)
                   :cache-policy
                   (or (appkit-media-playback-cache-policy plan)
                       (if (eq kind 'image) 'none 'automatic))
                   :muted (or (appkit-media-playback-muted plan)
                              (eq kind 'image))
                   :loop (and (eq kind 'video)
                              (appkit-media-playback-loop plan))
                   :live (appkit-media-playback-live plan)
                   :request-headers
                   (appkit-media-playback-request-headers plan)
                   :animation-loop-policy
                   (or (appkit-media-playback-animation-loop-policy plan)
                       video-animation-loop-policy)))
                 inline opened)
            (unwind-protect
                (progn
                  (setq inline
                        (appkit-media-video-inline-create
                         session width height
                         :poster poster :fit (appkit-media-playback-fit plan)
                         :corner-radius (if thumbnail (/ (min width height) 5.0) 0)
                         :controls-p (appkit-media--inline-toggle-p host)
                         :visible-function (lambda (_) (appkit-media--inline-visible-p host))
                         :alive-function (lambda (_) (appkit-media--inline-alive-p host))
                         :anchor (car (appkit-media-inline-host-overlays host))
                         :activate-function
                         (lambda (surface canvas)
                           (appkit-media--inline-show-canvas host surface canvas))
                         :close-function
                         (lambda (surface)
                           (when (eq surface (appkit-media-inline-host-inline host))
                             (setf (appkit-media-inline-host-inline host) nil)
                             (setq appkit-media--inline-image-hosts
                                   (delq host appkit-media--inline-image-hosts))
                             (appkit-media--inline-show-canvas
                              host surface (appkit-media--inline-poster host))))))
                  (setf (appkit-media-inline-host-inline host) inline
                        (appkit-media-inline-host-layout host)
                        (appkit-media--inline-layout host))
                  (when (eq kind 'image)
                    (cl-pushnew host appkit-media--inline-image-hosts))
                  (when (appkit-media--inline-toggle-p host)
                    (appkit-media-video-inline-bind-controls
                     inline (appkit-media-inline-host-map host)))
                  (setq opened t)
                  inline)
              (unless opened
                (when inline (appkit-media-video-inline-close inline))
                (appkit-media-video-session-close session))))))))

(defun appkit-media--inline-toggle-p (host)
  "Whether HOST's normal action controls inline playback."
  (let ((plan (appkit-media-inline-host-plan host)))
    (and (eq (appkit-media-playback-interaction plan) 'media)
         (or (eq (appkit-media-playback-kind plan) 'video)
             (appkit-media-inline-host-animated host)))))

(defun appkit-media--playback-inline-autoplay-p (plan)
  "Return non-nil when PLAN is eligible for automatic inline playback.
Images preserve the existing autoplay/animation policy; video requires the
explicit INLINE-AUTOPLAY plan field."
  (and plan
       appkit-media-inline-animation-enabled
       (pcase (appkit-media-playback-kind plan)
         ('image
          (and (appkit-media-playback-autoplay plan)
               (appkit-media-playback-animated-p plan)))
         ('video (appkit-media-playback-inline-autoplay plan))
         (_ nil))))

(defun appkit-media--inline-autoplay-p (host)
  "Return non-nil when HOST may start inline playback without activation."
  (let ((plan (appkit-media-inline-host-plan host)))
    (and (appkit-media--playback-inline-autoplay-p plan)
         (appkit-media-inline-host-live-p host)
         (not (appkit-media-inline-host-autoplay-disabled-p host))
         (appkit-media-inline-host-resource host)
         (not (appkit-media-inline-host-inline host))
         (appkit-media--inline-visible-p host)
         (or (not (eq (appkit-media-playback-kind plan) 'image))
             (appkit-media-inline-host-animated host)))))

(defun appkit-media--inline-use (host presentation)
  "Activate HOST with its resolved resource using PRESENTATION.
While parked, a completed resolver records intent without using absent spans."
  (let ((plan (appkit-media-inline-host-plan host)))
    (cond
     ((appkit-media-inline-host-parked host)
      (setf (appkit-media-inline-host-resume host) (list presentation)))
     ((not (appkit-media-inline-host-resource host))
      (unless (appkit-media-inline-host-pending host)
        (let ((resolver (appkit-media-playback-resolve-function plan))
              (request (cons nil nil)))
          (unless resolver (user-error "Inline media resource is not available"))
          (setf (appkit-media-inline-host-pending host) request)
          (condition-case err
              (let ((cancel
                     (funcall
                      resolver
                      (lambda (resource)
                        (when (and (eq request (appkit-media-inline-host-pending host))
                                   (appkit-media--inline-alive-p host))
                          (setf (appkit-media-inline-host-pending host) nil
                                (appkit-media-inline-host-cancel host) nil
                                (appkit-media-inline-host-resource host)
                                (appkit-media-resource-normalize resource))
                          (when (eq (appkit-media-playback-animation
                                     (appkit-media-inline-host-plan host)) 'auto)
                            (when-let* ((file (alist-get 'file (appkit-media-inline-host-resource host))))
                              (setf (appkit-media-inline-host-animated host)
                                    (appkit-media--file-animated-p file))))
                          (appkit-media--inline-use host presentation)))
                      (lambda (reason)
                        (when (and (eq request (appkit-media-inline-host-pending host))
                                   (appkit-media--inline-alive-p host))
                          (setf (appkit-media-inline-host-pending host) nil
                                (appkit-media-inline-host-cancel host) nil)
                          (message "%s: %s"
                                   (or (appkit-media-playback-label
                                        (appkit-media-inline-host-plan host)) "media")
                                   reason))))))
                ;; Resolution may complete or retire its owner synchronously.
                (if (eq request (appkit-media-inline-host-pending host))
                    (setf (appkit-media-inline-host-cancel host) cancel)
                  (when (and cancel (not (appkit-media--inline-alive-p host)))
                    (funcall cancel))))
            ((error quit)
             (setf (appkit-media-inline-host-pending host) nil)
             (signal (car err) (cdr err)))))))
     (t
      (let ((inline (appkit-media--inline-ensure host)))
        (pcase (or presentation
                   (unless (appkit-media--inline-toggle-p host) 'dedicated))
          ('dedicated
           (appkit-media-present-video-inline
            inline (appkit-media-playback-label plan)
            :owner (appkit-media-inline-host-owner host)))
          ('frame
           (appkit-media-present-video-inline
            inline (appkit-media-playback-label plan)
            :owner (appkit-media-inline-host-owner host)
            :display-function #'video-display-buffer-other-frame))
          (_
           (if (and (appkit-media--inline-toggle-p host)
                    (video-inline-target (appkit-media-video-inline-inline inline)))
               (appkit-media-video-inline-toggle inline)
             (appkit-media-video-inline-play inline)))))))))

(cl-defun appkit-media-inline-host-activate (host &optional presentation)
  "Activate HOST using its interaction policy, or promote to PRESENTATION.
PRESENTATION is nil, `dedicated', or `frame'.  Tracks also accept `external'.
A media interaction toggles animated images and videos; still images and open
interactions use a viewer."
  (unless (appkit-media-inline-host-live-p host)
    (user-error "Inline media belongs to a retired buffer span"))
  (when-let* ((track (appkit-media-inline-host-track host)))
    (cl-return-from appkit-media-inline-host-activate
      (appkit-media-track-activate track presentation)))
  (unless (memq presentation '(nil dedicated frame))
    (error "Unknown inline media presentation: %S" presentation))
  (unless (appkit-media--inline-canvas-p host)
    (user-error "Canvas inline media is unavailable"))
  (appkit-media--inline-use host presentation))

(defun appkit-media--inline-release-hidden ()
  "Release image playback outside every visible window.
Keep each host and its poster so returning to it can start a fresh player.
A parked host is still inside a render transaction and keeps its lease."
  (dolist (host (copy-sequence appkit-media--inline-image-hosts))
    (let ((inline (appkit-media-inline-host-inline host)))
      (cond
       ((or (null inline) (appkit-media-video-inline-closed-p inline))
        (setq appkit-media--inline-image-hosts
              (delq host appkit-media--inline-image-hosts)))
       ((and (not (appkit-media-inline-host-parked host))
             (not (appkit-media--inline-visible-p host)))
        (appkit-media-video-inline-close inline))))))

(defun appkit-media--inline-schedule-release-hidden ()
  "Release hidden image players after redisplay yields."
  (when (and appkit-media--inline-image-hosts
             (not (timerp appkit-media--inline-hidden-timer)))
    (setq appkit-media--inline-hidden-timer
          (run-at-time
           0 nil
           (lambda ()
             (setq appkit-media--inline-hidden-timer nil)
             (appkit-media--inline-release-hidden))))))

;; Buffer-local hooks no longer run for a buffer after its last window leaves.
(add-hook 'window-state-change-hook #'appkit-media--inline-schedule-release-hidden)

(defun appkit-media--inline-start-visible (&rest _ignored)
  "Start eligible visible media after committed buffer changes."
  (appkit-media--inline-prune)
  ;; Free old pipelines before opening newly visible animations.
  (appkit-media--inline-release-hidden)
  ;; Inspect visible overlays, not every image retained in chat history.
  (let (hosts)
    (when (image-type-available-p 'canvas)
      (dolist (window (get-buffer-window-list (current-buffer) nil t))
        (when (display-graphic-p (window-frame window))
          (dolist (host (appkit-media--inline-hosts-in-region
                         (window-start window) (or (window-end window) (point-max))))
            (unless (memq host hosts) (push host hosts))))))
    (dolist (host hosts)
      (when (appkit-media-inline-host-live-p host)
        (appkit-media--inline-refresh-thumbnail host))
      (cond
       ((not (appkit-media-inline-host-live-p host))
        (appkit-media--inline-close host))
       ((appkit-media--inline-autoplay-p host)
        (condition-case err
            (appkit-media--inline-use host 'inline)
          (error
           (setf (appkit-media-inline-host-autoplay-disabled-p host) t)
           (message "Inline media unavailable: %s" (error-message-string err)))))))))

(defun appkit-media--inline-schedule-start ()
  "Start visible media after the renderer yields, coalescing insertions."
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
  "Mount RANGES of OCCURRENCE, reclaiming OWNER's matching parked host."
  (let* ((poster (appkit-media--occurrence-image occurrence))
         (plan (plist-get (cdr poster) :appkit-media-playback))
         (descriptor (plist-get (cdr poster) :appkit-media-track))
         (interaction (and plan (appkit-media-playback-interaction plan)))
         (resource (and plan (appkit-media-playback-resource plan)))
         (host (or (appkit-media--inline-adopt occurrence owner)
                   (appkit-media--inline-host-create
                    :buffer (current-buffer) :owner owner
                    :identity (appkit-media--inline-identity poster))))
         map
         completed)
    (unwind-protect
        (progn
          (setf (appkit-media-inline-host-occurrence host) occurrence
                (appkit-media-inline-host-plan host) plan
                (appkit-media-inline-host-resource host)
                (or (and resource (appkit-media-resource-normalize resource))
                    (appkit-media-inline-host-resource host))
                (appkit-media-inline-host-thumbnail host) nil
                (appkit-media-inline-host-animated host)
                (and plan (or (appkit-media-playback-animated-p plan)
                              (and (eq (appkit-media-playback-animation plan) 'auto)
                                   (appkit-media-inline-host-animated host)))))
          (setq map (if descriptor
                        (appkit-media--track-mount host descriptor poster (caar ranges))
                      (and interaction (make-sparse-keymap))))
          (setf (appkit-media-inline-host-map host) map)
          (when (and map (not descriptor))
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
                                 (appkit-media--image-poster poster)))
              (when map
                (overlay-put overlay 'keymap map)
                (overlay-put overlay 'mouse-face 'highlight)
                ;; These maps own primary clicks, not button-style mouse-2.
                ;; Advertising follow-link would silently discard native taps.
                (overlay-put overlay 'help-echo
                             (cond
                              (descriptor "Left/right: select; RET: activate; C-RET: viewer; F: frame")
                              ((appkit-media--inline-toggle-p host)
                               "RET: play/pause; C-RET: open viewer")
                              (t "RET: open viewer"))))))
          (setf (appkit-media-inline-host-overlays host)
                (nreverse (appkit-media-inline-host-overlays host)))
          (setf (appkit-media-inline-host-parked host) nil)
          (push host appkit-media--inline-hosts)
          (add-hook 'before-change-functions #'appkit-media--inline-before-change nil t)
          (add-hook 'after-change-functions #'appkit-media--inline-reap nil t)
          (add-hook 'post-command-hook #'appkit-media--inline-start-visible nil t)
          (add-hook 'window-scroll-functions #'appkit-media--inline-start-visible nil t)
          (add-hook 'window-configuration-change-hook
                    #'appkit-media--inline-start-visible nil t)
          (add-hook 'kill-buffer-hook #'appkit-media--inline-release-all nil t)
          (add-hook 'change-major-mode-hook #'appkit-media--inline-release-all nil t)
          (when (and owner (not (appkit-media-inline-host-owner-handle host)))
            (setf (appkit-media-inline-host-owner-handle host)
                  (appkit-register-handle
                   owner 'function (lambda () (appkit-media--inline-close host)))))
          (appkit-media--inline-placed host)
          (when (or (appkit-media--inline-thumbnail host)
                    (appkit-media--playback-inline-autoplay-p plan))
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
      (appkit-media--inline-retire host)))
  (unless (text-property-not-all start end 'appkit-media-occurrence nil)
    (cl-return-from appkit-media-inline-mount nil))
  (let ((position start)
        (groups (make-hash-table :test #'eq))
        order hosts)
    (while (< position end)
      (let* ((occurrence (get-text-property position 'appkit-media-occurrence))
             (next (next-single-property-change
                    position 'appkit-media-occurrence nil end))
             (poster (and occurrence (appkit-media--occurrence-image occurrence)))
             (plan (and poster (plist-get (cdr poster) :appkit-media-playback)))
             (track (and poster (plist-get (cdr poster) :appkit-media-track))))
        (when (or track (and poster (plist-get (cdr poster) :appkit-media-thumbnail))
                  (and plan
                       (or (appkit-media-playback-interaction plan)
                           (appkit-media--playback-inline-autoplay-p plan))))
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
