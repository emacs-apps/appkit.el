;;; appkit-media-image.el --- Shared inline image rendering  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-apps/appkit.el

;;; Commentary:

;; Protocol-neutral image primitives for stateful chat applications.  This
;; module owns preview sizing, vertical image slices, prepared playback data,
;; and image-byte format detection.  Cacheable previews come from
;; `appkit-media-preview-image-from-file'; display goes through
;; `appkit-media-insert-image-slices' or `appkit-media-image-slice-rows'.
;; Backend payloads remain application concerns; `appkit-media-resource'
;; owns resource acquisition and caching.

;;; Code:

(require 'cl-lib)
(require 'image)
(require 'pcase)
(require 'seq)
(require 'svg nil t)
(require 'appkit-core)
(require 'appkit-geometry)
(require 'appkit-media-card)
(require 'video-source)

(declare-function video-canvas-copy "video-runtime" (image))
(declare-function video-source-poster
                  "video-runtime" (source width height &rest args))
(declare-function video-canvas-create "video-runtime" (width height &optional background))
(declare-function video-background-color "video-runtime" (&optional buffer position window))
(declare-function video-native-canvas-fill "video-module"
                  (image width height x y fill-width fill-height color))
(declare-function canvas-refresh "canvas" (image))
(declare-function video-session-create "video-runtime" (source &rest args))
(declare-function video-session-player "video-runtime" (session))
(declare-function video-session-close "video-runtime" (session))
(declare-function video-session-inline-create "video-inline" (session width height &rest args))
(declare-function video-inline-prepare "video-inline" (inline))
(declare-function appkit-media--inline-prune "appkit-media-inline" (&optional force))

(autoload 'appkit-media-inline-mount "appkit-media-inline"
  "Mount prepared media between START and END." nil)

(defun appkit-media--render-complete (owner start end success)
  "Reconcile OWNER's final prepared image spans after a content update.
Do not load the playback adapter for text-only buffers.  Failed updates only
retire stale hosts; they never start presentation of partial output."
  (cond
   ((not success)
    (when (featurep 'appkit-media-inline)
      (appkit-media--inline-prune t)))
   ((or (featurep 'appkit-media-inline)
        (text-property-not-all start end 'appkit-media-occurrence nil))
    (appkit-media-inline-mount start end owner))))

(defgroup appkit-media nil
  "Media rendering primitives for Appkit applications."
  :group 'appkit)

(defcustom appkit-media-video-play-icon-radius-divisor 8.0
  "Preview-height divisor used to derive the video play icon radius."
  :type 'number
  :group 'appkit-media)

(defcustom appkit-media-video-play-icon-circle-opacity 0.65
  "Opacity used for the black video play icon circle."
  :type 'number
  :group 'appkit-media)

(defcustom appkit-media-video-play-icon-triangle-opacity 0.65
  "Opacity used for the white video play icon triangle."
  :type 'number
  :group 'appkit-media)

(defcustom appkit-media-inline-animation-enabled t
  "When non-nil, autoplay visible eligible images through video.el Canvas."
  :type 'boolean
  :group 'appkit-media)

(defcustom appkit-media-preview-max-width 460
  "Default maximum pixel width for inline image previews."
  :type 'integer
  :group 'appkit-media)

(defcustom appkit-media-preview-max-height 360
  "Default maximum pixel height for inline image previews."
  :type 'integer
  :group 'appkit-media)

(defun appkit-media-inline-image-rendering-available-p (&optional frame)
  "Return non-nil when FRAME can render inline images.

FRAME defaults to the selected frame.  This query never selects FRAME."
  (and (or (null frame) (frame-live-p frame))
       (display-images-p frame)
       (or (image-type-available-p 'png)
           (image-type-available-p 'webp)
           (image-type-available-p 'jpeg)
           (image-type-available-p 'gif)
           (image-type-available-p 'imagemagick))))

(defun appkit-media-image-capable-frame (&optional buffer)
  "Return an image-capable frame suitable for rendering BUFFER.

Prefer BUFFER's canonical graphical display window, then another live
image-capable frame.  Return nil when no such frame exists."
  (or (appkit-geometry-display-frame
       buffer #'appkit-media-inline-image-rendering-available-p)
      (seq-find
       #'appkit-media-inline-image-rendering-available-p
       (frame-list))))

(defun appkit-media-image-object-valid-p (image)
  "Return non-nil when IMAGE can be rendered by Emacs."
  (and image
       (condition-case nil
           (progn
             (image-size image t)
             t)
         (error nil))))

(defun appkit-media-image-mime-type (file)
  "Return the SVG-embeddable image MIME type of FILE, or nil.

Prefer the file header over its name so extensionless cache files and stale
filename hints cannot select the wrong decoder."
  (when (and (stringp file) (file-readable-p file))
    (pcase (ignore-errors (image-type-from-file-header file))
      ('png "image/png")
      ((or 'jpeg 'jpg) "image/jpeg")
      ('gif "image/gif")
      ('webp "image/webp")
      ('svg "image/svg+xml")
      (_ nil))))

(defun appkit-media-circular-image-from-file (file pixel-size)
  "Return FILE center-cropped to a circular PIXEL-SIZE image, or nil.

The source is embedded in an SVG and clipped geometrically; `:mask
heuristic' is intentionally insufficient because it follows source colors
instead of the avatar outline.  Unsupported image formats or displays fall
back to nil so applications can retain their ordinary square image path."
  (when (and (stringp file)
             (file-readable-p file)
             (numberp pixel-size)
             (> pixel-size 0)
             (image-type-available-p 'svg)
             (fboundp 'svg-create)
             (fboundp 'svg-clip-path)
             (fboundp 'svg-circle)
             (fboundp 'svg-embed)
             (fboundp 'svg-image))
    (condition-case nil
        (when-let* ((mime-type (appkit-media-image-mime-type file)))
          (let* ((size (max 1 (round pixel-size)))
                 (radius (/ size 2.0))
                 (svg (svg-create size size))
                 (clip (svg-clip-path svg :id "appkit-avatar-clip")))
            (svg-circle clip radius radius radius)
            (svg-embed
             svg file mime-type nil
             :x 0 :y 0 :width size :height size
             :preserveAspectRatio "xMidYMid slice"
             :clip-path "url(#appkit-avatar-clip)")
            (let ((image
                   (svg-image
                    svg :ascent 'center :width size :height size)))
              (and (appkit-media-image-object-valid-p image) image))))
      (error nil))))

(defun appkit-media--cropped-image-from-file
    (file pixel-width pixel-height clip-id &optional radius scalablep insets)
  "Return FILE center-cropped to a fixed image box.

PIXEL-WIDTH and PIXEL-HEIGHT define the box.  CLIP-ID names its SVG clip
path.  When RADIUS is non-nil, round the box corners by that many pixels.
When SCALABLEP is non-nil, preserve the box aspect ratio as its height changes.
INSETS is an optional (TOP RIGHT BOTTOM LEFT) list of transparent pixels."
  (when (and (stringp file)
             (file-readable-p file)
             (numberp pixel-width)
             (> pixel-width 0)
             (numberp pixel-height)
             (> pixel-height 0)
             (image-type-available-p 'svg)
             (fboundp 'svg-create)
             (fboundp 'svg-clip-path)
             (fboundp 'svg-rectangle)
             (fboundp 'svg-embed)
             (fboundp 'svg-image))
    (condition-case nil
        (when-let* ((mime-type (appkit-media-image-mime-type file)))
          (let* ((width (max 1 (round pixel-width)))
                 (height (max 1 (round pixel-height)))
                 (raw-top (max 0 (round (or (nth 0 insets) 0))))
                 (raw-right (max 0 (round (or (nth 1 insets) 0))))
                 (raw-bottom (max 0 (round (or (nth 2 insets) 0))))
                 (raw-left (max 0 (round (or (nth 3 insets) 0))))
                 (top (min (1- height) raw-top))
                 (left (min (1- width) raw-left))
                 (right (min (- width left 1) raw-right))
                 (bottom (min (- height top 1) raw-bottom))
                 (content-width (- width left right))
                 (content-height (- height top bottom))
                 (svg (svg-create width height))
                 (clip (svg-clip-path svg :id clip-id)))
            (if radius
                (svg-rectangle clip left top content-width content-height
                               :rx radius :ry radius)
              (svg-rectangle clip left top content-width content-height))
            (svg-embed
             svg file mime-type nil
             :x left :y top :width content-width :height content-height
             :preserveAspectRatio "xMidYMid slice"
             :clip-path (format "url(#%s)" clip-id))
            (let ((image
                   (if scalablep
                       (svg-image
                        svg :ascent 'center :height height :scale 1.0)
                     (svg-image
                      svg :ascent 'center :width width :height height
                      :scale 1.0))))
              (and (appkit-media-image-object-valid-p image) image))))
      (error nil))))


(defun appkit-media-cropped-preview-image-from-file
    (file pixel-width pixel-height &optional insets)
  "Return FILE center-cropped to an exact preview box.

PIXEL-WIDTH and PIXEL-HEIGHT specify the box at default text scale.  Optional
INSETS is a (TOP RIGHT BOTTOM LEFT) list of transparent edge pixels.  The
returned descriptor records enough line slices to cover the complete box
height without distorting the source aspect ratio.  Return nil when SVG
cropping is unavailable."
  (when-let* ((image
               (appkit-media--cropped-image-from-file
                file pixel-width pixel-height
                "appkit-cropped-preview-clip" nil t insets)))
    (plist-put
     (cdr image) :appkit-media-nslices
     (max 1
          (ceiling
           (/ (float (max 1 pixel-height))
              (float (appkit-media--base-char-pixel-height))))))
    image))

(defun appkit-media-append-video-play-icon
    (svg width height &optional origin-x origin-y)
  "Append a centered play icon to SVG within WIDTH and HEIGHT.

ORIGIN-X and ORIGIN-Y position the box within a larger SVG."
  (when (and (fboundp 'svg-circle) (fboundp 'svg-polygon))
    (let* ((x (or origin-x 0))
           (y (or origin-y 0))
           (center-x (+ x (/ width 2.0)))
           (center-y (+ y (/ height 2.0)))
           (radius (/ height
                      (max 0.1
                           (float
                            appkit-media-video-play-icon-radius-divisor))))
           (offset (/ radius 8.0))
           (left (+ x offset (/ (- width radius) 2.0)))
           (right (+ x offset (/ (+ width radius) 2.0)))
           (top (+ y (/ (- height radius) 2.0)))
           (bottom (+ y (/ (+ height radius) 2.0))))
      (svg-circle
       svg center-x center-y radius
       :fill "#000000"
       :fill-opacity appkit-media-video-play-icon-circle-opacity)
      (svg-polygon
       svg
       (list (cons left top) (cons left bottom) (cons right center-y))
       :fill "#ffffff"
       :fill-opacity appkit-media-video-play-icon-triangle-opacity))))

(defun appkit-media--horizontal-strip-item-ratio (item)
  "Return ITEM's positive aspect ratio, or 1.0."
  (let* ((file (plist-get item :file))
         (declared-width (plist-get item :width))
         (declared-height (plist-get item :height))
         (file-size
          (and (not (and (numberp declared-width)
                         (numberp declared-height)))
               file
               (appkit-media--image-file-size-pixels file)))
         (width (or declared-width (car-safe file-size)))
         (height (or declared-height (cdr-safe file-size))))
    (if (and (numberp width) (> width 0)
             (numberp height) (> height 0))
        (/ (float width) height)
      1.0)))

(defun appkit-media-horizontal-strip-plan
    (items pixel-height pixel-gap &optional pixel-offset)
  "Return backend-neutral horizontal scene geometry for ITEMS.

Each item is a plist accepted by `appkit-media-horizontal-strip-image'.
PIXEL-HEIGHT fixes the scene height, PIXEL-GAP separates items, and optional
PIXEL-OFFSET identifies the scene x coordinate at the viewport's left edge.
The returned plist contains `:width', `:height', `:gap', `:offset', and
`:items'.  Each planned item retains its source plist and has integer `:x',
`:width', and `:id' values."
  (when (and items
             (numberp pixel-height)
             (> pixel-height 0)
             (numberp pixel-gap)
             (>= pixel-gap 0)
             (or (null pixel-offset) (numberp pixel-offset)))
    (let* ((height (max 1 (round pixel-height)))
           (gap (max 0 (round pixel-gap)))
           (geometry
            (cl-loop for item in items
                     for index from 0
                     for display-width = (plist-get item :display-width)
                     for width = (max
                                  1
                                  (round
                                   (if (and (numberp display-width)
                                            (> display-width 0))
                                       display-width
                                     (* height
                                        (appkit-media--horizontal-strip-item-ratio
                                         item)))))
                     collect
                     (list :item item
                           :id (or (plist-get item :id) index)
                           :width width)))
           (strip-width
            (+ (apply #'+ (mapcar (lambda (cell)
                                    (plist-get cell :width))
                                  geometry))
               (* gap (1- (length geometry)))))
           (offset
            (min (max 0 (round (or pixel-offset 0)))
                 (max 0 (1- strip-width))))
           (x 0))
      (dolist (cell geometry)
        (plist-put cell :x x)
        (setq x (+ x (plist-get cell :width) gap)))
      (list :width strip-width
            :height height
            :gap gap
            :offset offset
            :items geometry))))

(defvar-local appkit-media--horizontal-strip-images nil
  "Weak scene cache; the buffer's live media tracks own the descriptors.")

(defun appkit-media-horizontal-strip-image
    (items pixel-height pixel-gap &optional pixel-offset)
  "Return ITEMS rendered at PIXEL-HEIGHT with PIXEL-GAP scene spacing.

Each item may contain `:file', `:width', `:height', `:id', `:display-width',
`:fit', and `:play-icon'.  Optional PIXEL-OFFSET moves that scene x coordinate
to the left edge without changing the display width.  Item IDs become image
map hot spots in visible coordinates.  Return nil when SVG is unavailable.

Reuse live descriptors for unchanged scenes in the current buffer.  File
replacement, geometry and presentation settings participate in cache identity.
Do not mutate the returned descriptor.  Rasterization is deferred to display."
  (when (and (image-type-available-p 'svg)
             (fboundp 'svg-create)
             (fboundp 'svg-embed)
             (fboundp 'svg-image))
    (condition-case nil
        (when-let* ((plan
                     (appkit-media-horizontal-strip-plan
                      items pixel-height pixel-gap pixel-offset)))
          (let* ((height (plist-get plan :height))
                 (strip-width (plist-get plan :width))
                 (offset (plist-get plan :offset))
                 (geometry (plist-get plan :items))
                 (nslices
                  (max 1
                       (ceiling
                        (/ (float height)
                           (float (appkit-media--base-char-pixel-height))))))
                 (key
                  (list
                   plan nslices image-transform-smoothing
                   (and (cl-some (lambda (item) (plist-get item :play-icon)) items)
                        (list appkit-media-video-play-icon-radius-divisor
                              appkit-media-video-play-icon-circle-opacity
                              appkit-media-video-play-icon-triangle-opacity))
                   (mapcar
                    (lambda (item)
                      (when-let* ((file (plist-get item :file))
                                  ((stringp file))
                                  (attributes (file-attributes file)))
                        (list (file-attribute-modification-time attributes)
                              (file-attribute-status-change-time attributes)
                              (file-attribute-size attributes)
                              (file-attribute-inode-number attributes)
                              (file-attribute-device-number attributes))))
                    items)))
                 (cache
                  (or appkit-media--horizontal-strip-images
                      (setq appkit-media--horizontal-strip-images
                            (make-hash-table :test #'equal :weakness 'value)))))
            (or (gethash key cache)
                (let ((svg
                       (svg-create
                        strip-width height
                        :viewBox
                        (format "%d 0 %d %d" offset strip-width height)))
                      image-map)
                  (dolist (cell geometry)
                    (let* ((item (plist-get cell :item))
                           (file (plist-get item :file))
                           (x (plist-get cell :x))
                           (width (plist-get cell :width))
                           (id (plist-get cell :id))
                           (map-left (max 0 (- x offset)))
                           (map-right (min strip-width (- (+ x width) offset)))
                           (mime-type
                            (and (stringp file)
                                 (file-readable-p file)
                                 (appkit-media-image-mime-type file))))
                      (when mime-type
                        (svg-embed
                         svg file mime-type nil
                         :x x :y 0 :width width :height height
                         :preserveAspectRatio
                         (if (eq (plist-get item :fit) 'cover)
                             "xMidYMid slice"
                           "xMidYMid meet"))
                        (when (plist-get item :play-icon)
                          (appkit-media-append-video-play-icon
                           svg width height x 0)))
                      (when (< map-left map-right)
                        (push
                         (list
                          `(rect . ((,map-left . 0) . (,map-right . ,height)))
                          id
                          '(:pointer hand :help-echo "Open media"))
                         image-map))))
                  ;; Both maps use intrinsic pixels.  Supplying both avoids
                  ;; decoding just to rediscover the known, unscaled geometry.
                  (setq image-map (nreverse image-map))
                  (when-let* ((image
                               (svg-image
                                svg :ascent 'center :height height :scale 1.0
                                :map image-map :original-map (copy-tree image-map t)
                                :appkit-media-nslices nslices
                                :appkit-media-strip-widths
                                (mapcar (lambda (cell) (plist-get cell :width)) geometry)
                                :appkit-media-strip-offset offset)))
                    ;; Snapshot caller-owned plists only on a cache miss.
                    (puthash (copy-tree key) image cache))))))
      (error nil))))

(cl-defstruct (appkit-media--playback
               (:constructor appkit-media--playback-create)
               (:copier nil))
  "Immutable source playback plan, independent of mounted runtime state."
  (resource nil :read-only t)
  (kind nil :read-only t)
  (label nil :read-only t)
  (interaction nil :read-only t)
  (autoplay nil :read-only t)
  (animation nil :read-only t)
  (animated-p nil :read-only t)
  (resolve-function nil :read-only t)
  (cache-key nil :read-only t)
  (cache-directory nil :read-only t)
  (animation-loop-policy nil :read-only t)
  (fit nil :read-only t))

(cl-defstruct (appkit-media--thumbnail
               (:constructor appkit-media--thumbnail-create)
               (:copier nil))
  "Immutable thumbnail source and authored pixel geometry, never a player."
  (source nil :read-only t)
  (width nil :read-only t)
  (height nil :read-only t)
  (fit nil :read-only t)
  (background nil :read-only t)
  (animated-p nil :read-only t)
  (poster nil :read-only t))

(cl-defstruct (appkit-media--thumbnail-job
               (:constructor appkit-media--thumbnail-job-create))
  thumbnail buffer player session inline timer finished)

(defvar appkit-media--thumbnail-cache
  (make-hash-table :test #'equal :weakness 'value)
  "Weak cache of bounded posters keyed by source identity and authored geometry.")

(defvar appkit-media--thumbnail-waiting nil)
(defvar appkit-media--thumbnail-active nil)
(defvar appkit-media--thumbnail-timer nil)

(defun appkit-media--thumbnail-schedule ()
  "Coalesce asynchronous poster work outside rendering and redisplay."
  (unless (timerp appkit-media--thumbnail-timer)
    (setq appkit-media--thumbnail-timer
          (run-at-time 0 nil #'appkit-media--thumbnail-pump))))

(defun appkit-media--thumbnail-finish (job &optional error)
  "Release JOB's first-frame decoder exactly once, reporting ERROR."
  (unless (appkit-media--thumbnail-job-finished job)
    (setf (appkit-media--thumbnail-job-finished job) t)
    (when-let* ((timer (appkit-media--thumbnail-job-timer job)))
      (cancel-timer timer))
    (unwind-protect
        (progn
          (unwind-protect
              (when-let* ((session (appkit-media--thumbnail-job-session job)))
                (video-session-close session))
            (when-let* ((buffer (appkit-media--thumbnail-job-buffer job))
                        ((buffer-live-p buffer)))
              (kill-buffer buffer)))
          (when error
            ;; A failed source must not remain an invisible loading surface.
            (let* ((thumbnail (appkit-media--thumbnail-job-thumbnail job))
                   (canvas (appkit-media--thumbnail-poster thumbnail))
                   (width (appkit-media--thumbnail-width thumbnail))
                   (height (appkit-media--thumbnail-height thumbnail)))
              (video-native-canvas-fill canvas width height 0 0 width height #x993333)
              (canvas-refresh canvas))
            (message "Thumbnail unavailable: %s" error)))
      (setq appkit-media--thumbnail-active (delq job appkit-media--thumbnail-active))
      (when appkit-media--thumbnail-waiting (appkit-media--thumbnail-schedule)))))

(defun appkit-media--thumbnail-error (player error)
  "Retire a one-shot thumbnail PLAYER on native ERROR."
  (when-let* ((job (cl-find player appkit-media--thumbnail-active
                            :key #'appkit-media--thumbnail-job-player :test #'eq)))
    (appkit-media--thumbnail-finish job error)))

(defun appkit-media--thumbnail-pump ()
  "Run at most two one-shot native decoders, without waiting for a frame."
  (setq appkit-media--thumbnail-timer nil)
  (while (and appkit-media--thumbnail-waiting
              (< (length appkit-media--thumbnail-active) 2))
    (let* ((job (pop appkit-media--thumbnail-waiting))
           (thumbnail (appkit-media--thumbnail-job-thumbnail job))
           (buffer (generate-new-buffer " *appkit-thumbnail*")))
      (push job appkit-media--thumbnail-active)
      (setf (appkit-media--thumbnail-job-buffer job) buffer)
      (condition-case err
          (with-current-buffer buffer
            (setq-local face-remapping-alist
                        `((default (:background ,(appkit-media--thumbnail-background thumbnail)))))
            (let* ((width (appkit-media--thumbnail-width thumbnail))
                   (height (appkit-media--thumbnail-height thumbnail))
                   (session (video-session-create
                             (appkit-media--thumbnail-source thumbnail)
                             :kind 'image :muted t)))
              (setf (appkit-media--thumbnail-job-session job) session
                    (appkit-media--thumbnail-job-player job) (video-session-player session)
                    (appkit-media--thumbnail-job-timer job)
                    (run-at-time 15 nil #'appkit-media--thumbnail-finish
                                 job "First frame timed out"))
              (let ((inline
                     (video-session-inline-create
                      session width height
                      :fit (appkit-media--thumbnail-fit thumbnail)
                      :corner-radius (/ (min width height) 5.0)
                      :canvas (appkit-media--thumbnail-poster thumbnail)
                      :controls-p nil
                      :visible-function (lambda (_) t)
                      :alive-function (lambda (_) (not (appkit-media--thumbnail-job-finished job)))
                      :activate-function
                      (lambda (_inline _canvas)
                        ;; Close after dispatch unwinds, not during target traversal.
                        (unless (appkit-media--thumbnail-job-finished job)
                          (when-let* ((timer (appkit-media--thumbnail-job-timer job)))
                            (cancel-timer timer))
                          (setf (appkit-media--thumbnail-job-timer job)
                                (run-at-time 0 nil #'appkit-media--thumbnail-finish job)))))))
                (setf (appkit-media--thumbnail-job-inline job) inline)
                (video-inline-prepare inline))))
        (error (appkit-media--thumbnail-finish job (error-message-string err)))))))

(defun appkit-media--thumbnail-get (file width height fit &optional background)
  "Return FILE's shared poster plan for WIDTH, HEIGHT, FIT and BACKGROUND."
  (require 'video-inline)
  (setq background (or background (video-background-color)))
  (let* ((attributes (file-attributes file))
         (key (list (expand-file-name file)
                    (file-attribute-size attributes)
                    (file-attribute-modification-time attributes)
                    (file-attribute-status-change-time attributes)
                    width height fit background)))
    (or (gethash key appkit-media--thumbnail-cache)
        (let* ((poster (video-canvas-create width height "#999999"))
               (thumbnail (appkit-media--thumbnail-create
                           :source file :width width :height height
                           :animated-p (appkit-media--file-animated-p file)
                           :fit fit :poster poster :background background)))
          ;; All strings share this one first-frame pixel store, including
          ;; copies whose playback plan is later replaced with a video source.
          (plist-put (cdr poster) :width (cons (/ (float width) height) 'ch))
          (plist-put (cdr poster) :height '(1 . ch))
          (puthash key thumbnail appkit-media--thumbnail-cache)
          (setq appkit-media--thumbnail-waiting
                (nconc appkit-media--thumbnail-waiting
                       (list (appkit-media--thumbnail-job-create
                              :thumbnail thumbnail))))
          (add-hook 'video-player-error-hook #'appkit-media--thumbnail-error)
          (appkit-media--thumbnail-schedule)
          thumbnail))))

(defun appkit-media--image-poster (image)
  "Return IMAGE's shared first-frame pixel store, or IMAGE itself."
  (if-let* ((thumbnail (plist-get (cdr image) :appkit-media-thumbnail)))
      (appkit-media--thumbnail-poster thumbnail)
    image))

(cl-defstruct (appkit-media--occurrence
               (:constructor appkit-media--occurrence-create)
               (:copier nil))
  "Immutable image occurrence and its authored span geometry."
  (image nil :read-only t)
  (span-length nil :read-only t)
  (slices nil :read-only t))

(defun appkit-media--prepared-image-p (image)
  "Return non-nil when IMAGE needs an independently mounted occurrence."
  (or (plist-get (cdr image) :appkit-media-thumbnail)
      (plist-get (cdr image) :appkit-media-playback)
      (plist-get (cdr image) :appkit-media-track)))

(defun appkit-media--file-animated-p (file)
  "Return non-nil when local FILE contains multiple native animation frames."
  (when (and (stringp file) (not (file-remote-p file))
             (file-readable-p file) (file-regular-p file))
    (let ((metadata
           (pcase (video-source-format file)
             ('apng (video-source-apng-metadata file))
             ('webp (video-source-webp-metadata file))
             ('lottie (video-source-lottie-metadata file))
             (_ (video-source-gif-metadata file)))))
      (> (or (plist-get metadata :frames) 0) 1))))

(defun appkit-media-animated-image-p (image)
  "Return non-nil when IMAGE has known animation capability."
  (when (eq (car-safe image) 'image)
    (when-let* ((playback (plist-get (cdr image) :appkit-media-playback)))
      (appkit-media--playback-animated-p playback))))

(cl-defun appkit-media-image-with-resource
    (image resource &key (kind 'image) label (interaction 'media)
           (autoplay t) (animated 'auto) resolve-function cache-key
           cache-directory animation-loop-policy (fit 'contain))
  "Return a poster copy of IMAGE prepared to display RESOURCE.
Ordinary image descriptors are shallow-copied.  Native Canvas images are
explicitly snapshotted with their pixels into a static poster; a Lisp-only
copy would lose the backing store.  IMAGE, including a live Canvas, is not
reconfigured.

Playback metadata replaces any previous plan, including a previous resource.
ANIMATED defaults to local source metadata, or IMAGE's known capability when
RESOURCE has no local file.  Its `auto', t or nil directive is retained for
later resource resolution.  No acquisition or playback occurs here.
INTERACTION nil preserves enclosing actions; `media' supplies normal media
actions and `open' always opens the viewer.  AUTOPLAY is eligibility, not a
request to start playback.  RESOLVE-FUNCTION may acquire RESOURCE on demand."
  (when image
    (unless (memq kind '(image video))
      (error "Inline media kind must be image or video"))
    (unless (memq interaction '(nil media open))
      (error "Inline media interaction must be nil, media, or open"))
    (let* ((copy (if (eq (plist-get (cdr image) :type) 'canvas)
                     (video-canvas-copy image)
                   (cons 'image (copy-sequence (cdr image)))))
           (file (alist-get 'file resource))
           (thumbnail (plist-get (cdr image) :appkit-media-thumbnail))
           (animated-p
            (if (eq animated 'auto)
                (cond
                 ((and thumbnail (equal file (appkit-media--thumbnail-source thumbnail)))
                  (appkit-media--thumbnail-animated-p thumbnail))
                 (file (appkit-media--file-animated-p file))
                 (t (appkit-media-animated-image-p image)))
              (and animated t))))
      (setcdr copy
              (plist-put
               (cdr copy) :appkit-media-playback
               (appkit-media--playback-create
                :resource resource :kind kind :label label
                :interaction interaction :autoplay autoplay
                :animation animated :animated-p (and animated-p t)
                :resolve-function resolve-function :cache-key cache-key
                :cache-directory cache-directory
                :animation-loop-policy animation-loop-policy :fit fit)))
      copy)))

(defun appkit-media-image-display-string (image fallback)
  "Return FALLBACK displayed as IMAGE, or FALLBACK when IMAGE is nil.

Each occurrence has its own full-image display slice, so adjacent uses of a
cached image stay separate without copying its pixels.  Playback annotations
are inert until the containing text is mounted."
  (if (not image)
      fallback
    (let* ((prepared (appkit-media--prepared-image-p image))
           (string
            (propertize (or fallback " ")
                        'display (list '(slice 0 0 1.0 1.0)
                                       (appkit-media--image-poster image))
                        'rear-nonsticky '(display appkit-media-occurrence))))
      (when prepared
        (put-text-property 0 (length string) 'appkit-media-occurrence
                           (appkit-media--occurrence-create
                            :image image :span-length (length string) :slices 1)
                           string))
      string)))

(defun appkit-media-one-line-image-display-string (image fallback)
  "Return FALLBACK displayed as a current-line thumbnail for IMAGE.

IMAGE should come from `appkit-media-one-line-preview-image-from-file'.
Cached descriptors are copied and sized in character-relative units, so both
axes follow text zoom.  Numeric maximum widths are converted from their
default-scale pixel budget.  FALLBACK remains the underlying terminal text;
nil IMAGE returns it unchanged."
  (if (not image)
      fallback
    (let* ((render-image
            (appkit-media--image-with-height
             image (appkit-media-ch-height-spec 1)))
           (properties (cdr render-image))
           (max-width (plist-get properties :max-width))
           (thumbnail (plist-get properties :appkit-media-thumbnail)))
      (if thumbnail
          (setq properties
                (plist-put properties :width
                           (cons (/ (float (appkit-media--thumbnail-width thumbnail))
                                    (appkit-media--thumbnail-height thumbnail))
                                 'ch)))
        (cl-remf properties :width))
      (cl-remf properties :max-height)
      (when (numberp max-width)
        (setq properties
              (plist-put
               properties :max-width
               (cons (/ (float max-width)
                        (appkit-media--base-char-pixel-width))
                     'cw))))
      (setq properties (plist-put properties :scale 1.0))
      (setcdr render-image properties)
      (appkit-media-image-display-string render-image fallback))))

(defun appkit-media--display-image-spec (display)
  "Return an image spec nested in DISPLAY, including sliced displays."
  (cond
   ((and (consp display) (eq (car display) 'image)) display)
   ((and (consp display)
         (consp (car display))
         (eq (caar display) 'slice)
         (consp (cadr display))
         (eq (car (cadr display)) 'image))
    (cadr display))))

(defun appkit-media-image-slice-count (image)
  "Return the line count used to render IMAGE as vertical slices."
  (let ((explicit-slices
         (plist-get (cdr-safe image) :appkit-media-nslices)))
    (if (and (integerp explicit-slices) (> explicit-slices 0))
        explicit-slices
      (let* ((size (ignore-errors (image-size image nil (selected-frame))))
             (height (cdr-safe size)))
        (max 1 (if (numberp height) (round height) 1))))))

(defun appkit-media-insert-slice-newline ()
  "Insert a newline between image slices without an extra line gap."
  (let ((newline-start (point)))
    (insert "\n")
    (add-text-properties newline-start (point)
                         '(line-height t
                           rear-nonsticky (line-height)))))

(defun appkit-media--image-with-height (image height)
  "Return IMAGE displayed at pixel or character-relative HEIGHT.

Static Canvas posters copy their actual pixels before display sizing.
Live Canvas descriptors retain the identity of their externally updated
pixel buffers.  Other image descriptors are copied as before."
  (if (eq (plist-get (cdr-safe image) :type) 'canvas)
      (let ((render-image
             (if (plist-get (cdr image) :video-static-poster)
                 (video-canvas-copy image)
               image)))
        (plist-put (cdr render-image) :height height)
        render-image)
    (let ((properties (copy-sequence (cdr-safe image))))
      (cons 'image (plist-put properties :height height)))))

(defun appkit-media--line-slice-geometry (image)
  "Return (RENDER-IMAGE SLICE-COUNT SLICE-HEIGHT) for displaying IMAGE.

RENDER-IMAGE is a copy sized to SLICE-COUNT current text lines.
IMAGE is not mutated."
  (let* ((slice-count (appkit-media-image-slice-count image))
         (slice-height (appkit-media--char-pixel-height)))
    (list (appkit-media--image-with-height
           image (* slice-count slice-height))
          slice-count
          slice-height)))

(defun appkit-media-image-slice-rows (image)
  "Return IMAGE as one current-line display string per slice.

This is the non-inserting counterpart of
`appkit-media-insert-image-slices'.  Each string is a single space
whose `display' is one row of a copy sized to N current text lines.
IMAGE is not mutated.  After `text-scale-mode', rebuild and call this
again so the rows follow the new line height."
  (pcase-let* ((`(,render-image ,slice-count ,slice-height)
                (appkit-media--line-slice-geometry image))
               (prepared (appkit-media--prepared-image-p render-image))
               (occurrence
                (and prepared
                     (appkit-media--occurrence-create
                      :image render-image :span-length 1 :slices slice-count))))
    (cl-loop for slice-index below slice-count
             collect
             (let ((row
                    (propertize
                     " "
                     'display (list (list 'slice
                                          0
                                          (* slice-index slice-height)
                                          1.0
                                          slice-height)
                                    (appkit-media--image-poster render-image))
                     'rear-nonsticky '(display appkit-media-occurrence))))
               (when occurrence
                 (put-text-property 0 1 'appkit-media-occurrence occurrence row))
               row))))

(defun appkit-media-insert-image-slices
    (image &optional action prefix-string fallback help-echo)
  "Insert IMAGE as vertical line slices with optional ACTION.

IMAGE should come from `appkit-media-preview-image-from-file'.  This
function does not mutate IMAGE.  It copies IMAGE, sizes the copy to N
times the current line, and inserts one slice per line.  After
`text-scale-mode', rebuild the buffer and call this again.

Insert PREFIX-STRING before every slice after the first.  FALLBACK is
the image's protocol alt text.  HELP-ECHO describes ACTION."
  (let ((start (point)))
    (pcase-let*
        ((`(,render-image ,slice-count ,slice-height)
          (appkit-media--line-slice-geometry image))
         (prepared (appkit-media--prepared-image-p render-image))
         (label (or fallback "[image]"))
         (occurrence
          (and prepared
               (appkit-media--occurrence-create
                :image render-image :span-length (length label)
                :slices slice-count))))
      (dotimes (slice-index slice-count)
        (when (> slice-index 0)
          (appkit-media-insert-slice-newline)
          (when prefix-string
            (insert prefix-string)))
        (let ((slice-start (point))
              (slice (list 0
                           (* slice-index slice-height)
                           1.0
                           slice-height)))
          (insert-image (appkit-media--image-poster render-image) label nil slice)
          (when occurrence
            (add-text-properties
             slice-start (point)
             (list 'appkit-media-occurrence occurrence
                   'rear-nonsticky '(display appkit-media-occurrence))))
          (appkit-media-add-action-properties
           slice-start (point) action (or help-echo "Open media"))))
      (when occurrence
        (appkit-media-inline-mount start (point))))))

(defun appkit-media--char-pixel-width ()
  "Return one default-face column width for the current render target."
  (save-excursion
    (let ((window (appkit-geometry-display-window)))
      (max 1
           (or (and window
                    (ignore-errors (window-font-width window 'default)))
               (and (fboundp 'string-pixel-width)
                    (ignore-errors
                      (string-pixel-width
                       (propertize " " 'face 'default)
                       (current-buffer))))
               (frame-char-width)
               1)))))

(defun appkit-media--char-pixel-height ()
  "Return the default character height for the current buffer in pixels.

Use a window displaying the target buffer when possible.  This deliberately
does not use `line-pixel-height': asynchronous rendering can run while the
selected window displays an unrelated full-size image, making that function
return the source image height instead of a character height."
  (save-excursion
    (let ((window (appkit-geometry-display-window)))
      (max 1
           (or (and window
                    (ignore-errors
                      (window-font-height window 'default)))
               (ignore-errors (default-line-height))
               (frame-char-height)
               16)))))

(defun appkit-media--base-char-pixel-width ()
  "Return unscaled default-face width for the current render target."
  (let ((window (appkit-geometry-display-window)))
    (max 1
         (or (and window
                  (frame-char-width (window-frame window)))
             (frame-char-width)
             1))))

(defun appkit-media--base-char-pixel-height ()
  "Return unscaled default-face height for the current render target."
  (let ((window (appkit-geometry-display-window)))
    (max 1
         (or (and window
                  (frame-char-height (window-frame window)))
             (frame-char-height)
             16))))

(defun appkit-media--pixels->chars-width (pixels)
  "Convert PIXELS to character columns using current frame metrics."
  (max 1
       (ceiling (/ (float (max 1 pixels))
                   (float (appkit-media--char-pixel-width))))))

(defun appkit-media--pixels->chars-height (pixels)
  "Convert PIXELS to text lines using current frame metrics."
  (max 1
       (ceiling (/ (float (max 1 pixels))
                   (float (appkit-media--char-pixel-height))))))

(defun appkit-media-ch-height-spec (characters)
  "Return the `(HEIGHT . ch)' spec for CHARACTERS text lines."
  (cons (max 1 characters) 'ch))

(defun appkit-media--image-header-size (file)
  "Read PNG, GIF or WebP FILE dimensions without decoding its pixels.
Only a fixed-size header is read.  Other formats retain the normal decoder
fallback, including its orientation and format-specific sizing rules."
  (ignore-errors
    (let ((bytes (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert-file-contents-literally file nil 0 32)
                   (buffer-string))))
      (cl-labels ((uint (offset count little-endian)
                    (let ((value 0))
                      (dotimes (index count value)
                        (setq value
                              (+ (ash value 8)
                                 (aref bytes
                                       (+ offset
                                          (if little-endian
                                              (- count index 1)
                                            index)))))))))
        (let ((size
               (cond
                ((appkit-media--bytes-prefix-p
                  bytes 0 '(137 80 78 71 13 10 26 10))
                 (cons (uint 16 4 nil) (uint 20 4 nil)))
                ((or (appkit-media--bytes-prefix-p bytes 0 '(71 73 70 56 55 97))
                     (appkit-media--bytes-prefix-p bytes 0 '(71 73 70 56 57 97)))
                 (cons (uint 6 2 t) (uint 8 2 t)))
                ((appkit-media--webp-bytes-p-at bytes 0)
                 (cond
                  ((appkit-media--bytes-prefix-p bytes 12 '(86 80 56 88))
                   (cons (1+ (uint 24 3 t)) (1+ (uint 27 3 t))))
                  ((appkit-media--bytes-prefix-p bytes 12 '(86 80 56 76))
                   (let ((bits (uint 21 4 t)))
                     (cons (1+ (logand bits #x3fff))
                           (1+ (logand (ash bits -14) #x3fff)))))
                  ((and (appkit-media--bytes-prefix-p bytes 12 '(86 80 56 32))
                        (appkit-media--bytes-prefix-p bytes 23 '(157 1 42)))
                   (cons (logand (uint 26 2 t) #x3fff)
                         (logand (uint 28 2 t) #x3fff))))))))
          (when (and size (> (car size) 0) (> (cdr size) 0))
            size))))))

(defun appkit-media--image-file-size-pixels (file)
  "Return FILE image size in pixels as (WIDTH . HEIGHT), or nil."
  (if (eq (video-source-format file) 'lottie)
      (let ((metadata (video-source-lottie-metadata file)))
        (cons (plist-get metadata :width) (plist-get metadata :height)))
    (or (appkit-media--image-header-size file)
        (ignore-errors
          (image-size (create-image file nil nil :ascent 'center) t)))))

(defun appkit-media-preview-height-chars
    (image-size max-width max-height)
  "Return preview row count for IMAGE-SIZE within MAX-WIDTH and MAX-HEIGHT.

MAX-WIDTH and MAX-HEIGHT are a default-scale pixel budget.  Row count
uses unscaled face metrics so `text-scale-mode' does not shrink N on a
client that rebuilds the `:height Nch' descriptor."
  (let* ((character-width (float (appkit-media--char-pixel-width)))
         (character-height (float (appkit-media--base-char-pixel-height)))
         (max-columns
          (max 1 (ceiling (/ (float (max 1 max-width)) character-width))))
         (max-rows
          (max 1 (ceiling (/ (float (max 1 max-height)) character-height))))
         (image-width (max 1.0 (float (car image-size))))
         (image-height (max 1.0 (float (cdr image-size))))
         (image-columns (/ image-width character-width))
         (image-rows (/ image-height character-height))
         (scale (min 1.0
                     (/ (float max-columns) (max 1.0 image-columns))
                     (/ (float max-rows) (max 1.0 image-rows)))))
    (max 1
         (min max-rows
              (round (* image-rows scale))))))

(defun appkit-media-lottie-poster (file max-height &optional max-width)
  "Return a drawn static Canvas poster for local Lottie FILE.
Fit the intrinsic geometry within MAX-HEIGHT and optional MAX-WIDTH pixels.
Renderer errors propagate; displays without Canvas return nil."
  (when (and (display-graphic-p) (image-type-available-p 'canvas))
    (require 'video-runtime)
    (let* ((metadata (video-source-lottie-metadata file))
           (width (plist-get metadata :width))
           (height (plist-get metadata :height))
           (scale (min (/ (float (max 1 max-height)) height)
                       (if max-width (/ (float (max 1 max-width)) width)
                         most-positive-fixnum)))
           (poster (video-source-poster
                    file (max 1 (round (* width scale)))
                    (max 1 (round (* height scale))))))
      poster)))

(defun appkit-media-preview-image-from-file
    (file &optional max-width max-height)
  "Create a cacheable inline preview from FILE.

MAX-WIDTH and MAX-HEIGHT are a default-scale pixel budget and default
to `appkit-media-preview-max-width' and `appkit-media-preview-max-height'.
The returned image uses `:height (N . ch)' and `:appkit-media-nslices
N'.  N uses unscaled face metrics so `text-scale-mode' does not change
the cached descriptor.  Do not mutate the returned image.  The original local
resource and its animation capability are retained with passive interaction.

Display is a separate step: call `appkit-media-insert-image-slices' or
`appkit-media-image-slice-rows' so a copy is sized to N current lines."
  (let* ((safe-max-width
          (max 1 (if (numberp max-width)
                     max-width
                   appkit-media-preview-max-width)))
         (safe-max-height
          (max 1 (if (numberp max-height)
                     max-height
                   appkit-media-preview-max-height)))
         (file-size (appkit-media--image-file-size-pixels file))
         (target-height-characters
          (if (consp file-size)
              (appkit-media-preview-height-chars
               file-size safe-max-width safe-max-height)
            (appkit-media--pixels->chars-height safe-max-height)))
         (height-spec
          (appkit-media-ch-height-spec target-height-characters))
         (image
          (if (eq (video-source-format file) 'lottie)
              (when-let* ((poster (appkit-media-lottie-poster
                                  file safe-max-height safe-max-width)))
                (plist-put (cdr poster) :appkit-media-nslices
                           target-height-characters)
                poster)
            (cl-loop
             for type in (if (image-type-available-p 'imagemagick)
                             '(nil imagemagick)
                           '(nil))
             for candidate =
             (ignore-errors
               (create-image file type nil
                             :height height-spec
                             :appkit-media-nslices target-height-characters
                             :scale 1.0
                             :ascent 'center))
             when (appkit-media-image-object-valid-p candidate)
             return candidate))))
    (appkit-media-image-with-resource
     image `((file . ,file)) :interaction nil)))

(defun appkit-media-one-line-preview-image-from-file (file &optional max-width fit)
  "Prepare a nonblocking, one-row thumbnail for local FILE.

MAX-WIDTH is an authored, default-scale pixel budget, defaulting to two
unscaled character columns capped by `appkit-media-preview-max-width'.
FIT defaults to `cover'; `contain' retains the full source aspect.
The immutable `:appkit-media-thumbnail' records source, geometry, fit and a
bounded shared first-frame Canvas, independently of the playback resource.
One-shot decoders run asynchronously with bounded concurrency and retire
after their first frame.  Static and disabled-autoplay thumbnails therefore
work even in standalone strings; animation remains occurrence-owned.
Without a native decoder (including source SVG), use the ordinary Emacs image
decoder or return nil for text fallback; never wrap the source in another SVG.

Display with `appkit-media-one-line-image-display-string'."
  (when (and (stringp file) (not (file-remote-p file))
             (file-readable-p file) (file-regular-p file))
    (let* ((file (expand-file-name file))
           (width (max 1 (round
                          (if (numberp max-width)
                              max-width
                            (min appkit-media-preview-max-width
                                 (* 2 (appkit-media--base-char-pixel-width)))))))
           (height (appkit-media--base-char-pixel-height))
           (fit (or fit 'cover)))
      (unless (memq fit '(cover contain))
        (error "Invalid thumbnail fit: %S" fit))
      (if (and (image-type-available-p 'canvas)
               (or (memq (ignore-errors (image-type-from-file-header file))
                         '(png jpeg gif webp))
                   (memq (video-source-format file) '(apng webp lottie))))
          (let ((thumbnail (appkit-media--thumbnail-get file width height fit)))
            (appkit-media-image-with-resource
             (list 'image :type 'pbm :data "P1\n2 2\n0 1\n1 0\n"
                   :width width :height height :scale 1.0
                   :appkit-media-nslices 1 :appkit-media-thumbnail thumbnail)
             `((file . ,file)) :interaction nil :fit fit))
        (ignore-errors
          (appkit-media-image-with-resource
           (create-image file nil nil :height '(1 . ch)
                         :max-width width :scale 1.0 :appkit-media-nslices 1)
           `((file . ,file)) :interaction nil :fit fit))))))

(defun appkit-media--bytes-prefix-p (bytes offset prefix-bytes)
  "Return non-nil when BYTES at OFFSET starts with PREFIX-BYTES."
  (and (stringp bytes)
       (integerp offset)
       (>= offset 0)
       (<= (+ offset (length prefix-bytes)) (length bytes))
       (cl-loop for byte in prefix-bytes
                for index from 0
                always (= (aref bytes (+ offset index)) byte))))

(defun appkit-media--webp-bytes-p-at (bytes offset)
  "Return non-nil when BYTES has a WebP signature at OFFSET."
  (and (appkit-media--bytes-prefix-p bytes offset '(82 73 70 70))
       (appkit-media--bytes-prefix-p bytes (+ offset 8) '(87 69 66 80))))

(defun appkit-media--known-image-signature-at-p (bytes offset)
  "Return non-nil when BYTES has a known image signature at OFFSET."
  (and (stringp bytes)
       (integerp offset)
       (>= offset 0)
       (<= offset (length bytes))
       (or (appkit-media--bytes-prefix-p
            bytes offset '(137 80 78 71 13 10 26 10))
           (appkit-media--bytes-prefix-p bytes offset '(255 216 255))
           (appkit-media--bytes-prefix-p bytes offset '(71 73 70 56 55 97))
           (appkit-media--bytes-prefix-p bytes offset '(71 73 70 56 57 97))
           (appkit-media--webp-bytes-p-at bytes offset))))

(defun appkit-media-normalize-image-bytes (bytes)
  "Strip a stray leading newline before recognized image BYTES."
  (cond
   ((and (stringp bytes)
         (>= (length bytes) 2)
         (eq (aref bytes 0) ?\n)
         (appkit-media--known-image-signature-at-p bytes 1))
    (substring bytes 1))
   ((and (stringp bytes)
         (>= (length bytes) 3)
         (eq (aref bytes 0) ?\r)
         (eq (aref bytes 1) ?\n)
         (appkit-media--known-image-signature-at-p bytes 2))
    (substring bytes 2))
   (t bytes)))

(defun appkit-media-bytes-to-extension (bytes fallback-extension)
  "Infer an image extension from BYTES, else return FALLBACK-EXTENSION."
  (cond
   ((appkit-media--bytes-prefix-p bytes 0 '(137 80 78 71 13 10 26 10))
    "png")
   ((appkit-media--bytes-prefix-p bytes 0 '(255 216 255))
    "jpg")
   ((or (appkit-media--bytes-prefix-p bytes 0 '(71 73 70 56 55 97))
        (appkit-media--bytes-prefix-p bytes 0 '(71 73 70 56 57 97)))
    "gif")
   ((appkit-media--webp-bytes-p-at bytes 0) "webp")
   (t fallback-extension)))

(provide 'appkit-media-image)

;;; appkit-media-image.el ends here
