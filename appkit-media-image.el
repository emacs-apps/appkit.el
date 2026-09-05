;;; appkit-media-image.el --- Shared inline image rendering  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Protocol-neutral image primitives for stateful chat applications.  This
;; module owns preview sizing, vertical image slices, bounded inline animation,
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
  "When non-nil, play bounded animated images inside rendered buffers."
  :type 'boolean
  :group 'appkit-media)

(defcustom appkit-media-inline-animation-max-duration 10.0
  "Maximum duration in seconds for automatic inline image playback."
  :type 'number
  :group 'appkit-media)

(defcustom appkit-media-inline-animation-max-file-size (* 8 1024 1024)
  "Maximum source size in bytes eligible for inline image playback."
  :type 'integer
  :group 'appkit-media)

(defcustom appkit-media-preview-max-width 460
  "Default maximum pixel width for inline image previews."
  :type 'integer
  :group 'appkit-media)

(defcustom appkit-media-preview-max-height 360
  "Default maximum pixel height for inline image previews."
  :type 'integer
  :group 'appkit-media)

(defvar-local appkit-media--inline-animation-window-starts nil
  "Last scanned window starts for inline animation discovery.")

(defvar-local appkit-media--inline-animation-occurrences nil
  "Weak registry of mutable inline animation occurrences in this buffer.

Cached image descriptors must never be registered here.  Each key is the
occurrence-specific copy created by `appkit-media-insert-image-slices'.")

(defun appkit-media--inline-animation-occurrence-p (image)
  "Return non-nil when IMAGE is a mutable inline animation occurrence."
  (and (appkit-media-inline-animation-image-p image)
       (plist-get
        (cdr image) :appkit-media-inline-animation-occurrence)))

(defun appkit-media--make-inline-animation-occurrence (image)
  "Return a mutable occurrence copy of animated image descriptor IMAGE.

Eligibility metadata is copied, while playback state belongs exclusively to
the returned image spec.  IMAGE remains suitable for immutable caches."
  (let ((occurrence (copy-tree image)))
    (plist-put (cdr occurrence)
               :appkit-media-inline-animation-occurrence t)
    (plist-put (cdr occurrence)
               :appkit-media-inline-animation-played nil)
    (plist-put (cdr occurrence)
               :appkit-media-inline-animation-reset-timer nil)
    occurrence))

(defun appkit-media--inline-animation-occurrence-registry ()
  "Return the current buffer's weak animation occurrence registry."
  (or appkit-media--inline-animation-occurrences
      (setq appkit-media--inline-animation-occurrences
            (make-hash-table :test #'eq :weakness 'key))))

(defun appkit-media--register-inline-animation-occurrence (image)
  "Track mutable animation occurrence IMAGE in the current buffer."
  (puthash image t
           (appkit-media--inline-animation-occurrence-registry))
  (add-hook 'kill-buffer-hook
            #'appkit-media--teardown-inline-animation-occurrences nil t)
  image)

(defun appkit-media--teardown-inline-animation-occurrences ()
  "Stop and forget every inline animation occurrence in this buffer."
  (when (hash-table-p appkit-media--inline-animation-occurrences)
    (maphash (lambda (image _present)
               (appkit-media-stop-inline-animation image))
             appkit-media--inline-animation-occurrences)
    (clrhash appkit-media--inline-animation-occurrences))
  (setq appkit-media--inline-animation-occurrences nil
        appkit-media--inline-animation-window-starts nil))

(defun appkit-media-stop-buffer-inline-animations (&optional buffer)
  "Stop and forget inline animation occurrences owned by BUFFER.

BUFFER defaults to the current buffer.  This is safe when no animated images
have been displayed."
  (with-current-buffer (or buffer (current-buffer))
    (appkit-media--teardown-inline-animation-occurrences)))

(defun appkit-media-inline-image-rendering-available-p (&optional frame)
  "Return non-nil when FRAME can render inline images.

FRAME defaults to the selected frame."
  (if frame
      (and (frame-live-p frame)
           (with-selected-frame frame
             (appkit-media-inline-image-rendering-available-p)))
    (and (display-images-p)
         (or (image-type-available-p 'png)
             (image-type-available-p 'webp)
             (image-type-available-p 'jpeg)
             (image-type-available-p 'gif)
             (image-type-available-p 'imagemagick)))))

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

(defun appkit-media--one-line-thumbnail-from-file
    (file pixel-width pixel-height)
  "Return FILE center-cropped to a compact one-line thumbnail.

PIXEL-WIDTH and PIXEL-HEIGHT define the exact rounded output box."
  (when-let* ((radius (max 1 (/ (min pixel-width pixel-height) 5.0)))
              (image
               (appkit-media--cropped-image-from-file
                file pixel-width pixel-height
                "appkit-one-line-thumbnail-clip" radius)))
    (plist-put (cdr image) :appkit-media-nslices 1)
    image))

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

(defun appkit-media--file-size (file)
  "Return FILE size in bytes, or nil when it is unavailable."
  (when (and (stringp file) (file-exists-p file))
    (file-attribute-size (file-attributes file))))

(defun appkit-media--inline-animation-frame-data (image)
  "Return (FRAME-COUNT . DURATION) for multi-frame IMAGE, or nil."
  (when-let* ((multi (ignore-errors (image-multi-frame-p image))))
    (let* ((count (car multi))
           (delay (or (and (numberp (cdr multi)) (cdr multi))
                      image-default-frame-delay))
           (duration (* count delay)))
      (and (> count 1)
           (> duration 0)
           (cons count duration)))))

(defun appkit-media--mark-inline-animation-image (image file)
  "Mark bounded multi-frame IMAGE from FILE for inline playback."
  (when (and appkit-media-inline-animation-enabled
             (appkit-media-image-object-valid-p image)
             (let ((size (appkit-media--file-size file)))
               (and size
                    (<= size
                        appkit-media-inline-animation-max-file-size))))
    (when-let* ((frame-data
                 (appkit-media--inline-animation-frame-data image))
                (duration (cdr frame-data))
                ((<= duration
                     appkit-media-inline-animation-max-duration)))
      (plist-put (cdr image) :appkit-media-inline-animation t)
      (plist-put (cdr image)
                 :appkit-media-inline-animation-duration
                 duration)))
  image)

(defun appkit-media-inline-animation-image-p (image)
  "Return non-nil when IMAGE is marked for bounded inline animation."
  (and (eq (car-safe image) 'image)
       (plist-get (cdr image) :appkit-media-inline-animation)))

(defun appkit-media-image-display-string (image fallback)
  "Return FALLBACK displayed as IMAGE, or FALLBACK when IMAGE is nil.

Animated cache descriptors are copied into buffer-owned playback occurrences
before display, so callers do not need to invoke Appkit's private animation
registry functions."
  (if (not image)
      fallback
    (let ((render-image
           (if (appkit-media-inline-animation-image-p image)
               (appkit-media--make-inline-animation-occurrence image)
             image)))
      (when (not (eq render-image image))
        (appkit-media--register-inline-animation-occurrence render-image)
        (appkit-media--install-inline-animation-discovery))
      (propertize (or fallback " ")
                  'display render-image
                  'rear-nonsticky '(display)))))

(defun appkit-media-one-line-image-display-string (image fallback)
  "Return FALLBACK displayed as a current-line thumbnail for IMAGE.

IMAGE should come from `appkit-media-one-line-preview-image-from-file'.
The cached descriptor is copied and sized to the current line in pixels;
the original is not mutated.  FALLBACK remains the underlying terminal
text.  Return FALLBACK unchanged when IMAGE is nil."
  (if (not image)
      fallback
    (let* ((animated-p (appkit-media-inline-animation-image-p image))
           (source-image
            (if animated-p
                (appkit-media--make-inline-animation-occurrence image)
              image)))
      (pcase-let ((`(,render-image ,_slice-count ,slice-height)
                   (appkit-media--line-slice-geometry source-image)))
        (when animated-p
          (appkit-media--register-inline-animation-occurrence render-image)
          (appkit-media--install-inline-animation-discovery))
        (propertize
         (or fallback " ")
         'display
         (list (list 'slice 0 0 1.0 slice-height)
               render-image)
         'rear-nonsticky '(display))))))

(defun appkit-media-stop-inline-animation (image)
  "Stop bounded inline playback for IMAGE and reset it to frame zero."
  (when (appkit-media--inline-animation-occurrence-p image)
    (when-let* ((timer (image-animate-timer image)))
      (cancel-timer timer))
    (when-let* ((timer
                 (plist-get
                  (cdr image)
                  :appkit-media-inline-animation-reset-timer)))
      (when (timerp timer)
        (cancel-timer timer)))
    (ignore-errors (image-show-frame image 0 t))
    (plist-put (cdr image)
               :appkit-media-inline-animation-reset-timer nil)
    (plist-put (cdr image) :appkit-media-inline-animation-played nil)))

(defun appkit-media--finish-inline-animation (image)
  "Finish one inline playback cycle for IMAGE."
  (when (appkit-media--inline-animation-occurrence-p image)
    (when-let* ((timer (image-animate-timer image)))
      (cancel-timer timer))
    (ignore-errors (image-show-frame image 0 t))
    (plist-put (cdr image)
               :appkit-media-inline-animation-reset-timer nil)
    (plist-put (cdr image) :appkit-media-inline-animation-played nil)))

(defun appkit-media-start-inline-animation (image)
  "Play marked IMAGE once when its current buffer is visible."
  (when (and appkit-media-inline-animation-enabled
             (appkit-media--inline-animation-occurrence-p image)
             (not (plist-get
                   (cdr image) :appkit-media-inline-animation-played))
             (get-buffer-window (current-buffer) t))
    (let ((duration
           (plist-get
            (cdr image) :appkit-media-inline-animation-duration)))
      (when (and (numberp duration) (> duration 0))
        (plist-put (cdr image) :appkit-media-inline-animation-played t)
        (image-animate image 0 nil)
        (plist-put
         (cdr image)
         :appkit-media-inline-animation-reset-timer
         (run-at-time (+ duration 0.4) nil
                      #'appkit-media--finish-inline-animation image))
        t))))

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

(defun appkit-media--start-window-inline-animations
    (window &optional start end)
  "Start animations visible in WINDOW between START and END."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (let ((position (or start (window-start window)))
          (end (or end (window-end window t) (point-max))))
      (while (< position end)
        (when-let* ((image
                     (appkit-media--display-image-spec
                      (get-text-property position 'display))))
          (appkit-media-start-inline-animation image))
        (setq position
              (or (next-single-property-change
                   position 'display nil end)
                  end)))
      (setf (alist-get window
                       appkit-media--inline-animation-window-starts
                       nil nil #'eq)
            (window-start window)))))

(defun appkit-media--start-window-inline-animations-after-scroll
    (window display-start)
  "Start animations after WINDOW has scrolled to DISPLAY-START."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (let ((display-end
           (save-excursion
             (goto-char display-start)
             (vertical-motion (1+ (window-body-height window)) window)
             (point))))
      (appkit-media--start-window-inline-animations
       window display-start display-end))))

(defun appkit-media--start-buffer-inline-animations-after-command ()
  "Discover animations when a displayed window moves after a command."
  (setq appkit-media--inline-animation-window-starts
        (seq-filter (lambda (entry) (window-live-p (car entry)))
                    appkit-media--inline-animation-window-starts))
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (unless (equal
             (alist-get window
                        appkit-media--inline-animation-window-starts
                        nil nil #'eq)
             (window-start window))
      (appkit-media--start-window-inline-animations window))))

(defun appkit-media-image-slice-count (image)
  "Return the line count used to render IMAGE as vertical slices."
  (let* ((properties (cdr-safe image))
         (explicit-slices
          (plist-get properties :appkit-media-nslices))
         (size (and (appkit-media-image-object-valid-p image)
                    (ignore-errors
                      (image-size image nil (selected-frame)))))
         (height (and (consp size) (cdr size))))
    (max 1
         (cond
          ((and (integerp explicit-slices) (> explicit-slices 0))
           explicit-slices)
          ((numberp height) (round height))
          (t 1)))))

(defun appkit-media-insert-slice-newline ()
  "Insert a newline between image slices without an extra line gap."
  (let ((newline-start (point)))
    (insert "\n")
    (add-text-properties newline-start (point)
                         '(line-height t
                           rear-nonsticky (line-height)))))

(defun appkit-media--install-inline-animation-discovery ()
  "Install buffer-local hooks that discover visible inline animations."
  (add-hook 'window-state-change-functions
            #'appkit-media--start-window-inline-animations nil t)
  (add-hook 'window-scroll-functions
            #'appkit-media--start-window-inline-animations-after-scroll nil t)
  (add-hook 'post-command-hook
            #'appkit-media--start-buffer-inline-animations-after-command nil t)
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (appkit-media--start-window-inline-animations window)))

(defun appkit-media--image-with-pixel-height (image pixel-height)
  "Return IMAGE displayed at PIXEL-HEIGHT.

Canvas descriptors retain their object identity because Emacs associates their
mutable pixel buffers by `eq'.  Other image descriptors are copied so cached
`:height Nch' values remain reusable after `text-scale-mode'."
  (if (eq (plist-get (cdr-safe image) :type) 'canvas)
      (progn
        (plist-put (cdr image) :height pixel-height)
        image)
    (let ((properties (copy-sequence (cdr-safe image))))
      (cons 'image (plist-put properties :height pixel-height)))))

(defun appkit-media--line-slice-geometry (image)
  "Return (RENDER-IMAGE SLICE-COUNT SLICE-HEIGHT) for displaying IMAGE.

RENDER-IMAGE is a copy sized to SLICE-COUNT current text lines.
IMAGE is not mutated."
  (let* ((slice-count (appkit-media-image-slice-count image))
         (slice-height (appkit-media--char-pixel-height)))
    (list (appkit-media--image-with-pixel-height
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
  (pcase-let ((`(,render-image ,slice-count ,slice-height)
               (appkit-media--line-slice-geometry image)))
    (cl-loop for slice-index below slice-count
             collect
             (propertize
              " "
              'display (list (list 'slice
                                   0
                                   (* slice-index slice-height)
                                   1.0
                                   slice-height)
                             render-image)
              'rear-nonsticky '(display)))))

(defun appkit-media-insert-image-slices
    (image &optional action prefix-string fallback help-echo)
  "Insert IMAGE as vertical line slices with optional ACTION.

IMAGE should come from `appkit-media-preview-image-from-file'.  This
function does not mutate IMAGE.  It copies IMAGE, sizes the copy to N
times the current line, and inserts one slice per line.  After
`text-scale-mode', rebuild the buffer and call this again.

Insert PREFIX-STRING before every slice after the first.  FALLBACK is
the image's protocol alt text.  HELP-ECHO describes ACTION."
  (let* ((animated-p (appkit-media-inline-animation-image-p image))
         (source-image
          (if animated-p
              (appkit-media--make-inline-animation-occurrence image)
            image)))
    (pcase-let
        ((`(,render-image ,slice-count ,slice-height)
          (appkit-media--line-slice-geometry source-image))
         (label (or fallback "[image]")))
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
          (insert-image render-image label nil slice)
          (appkit-media-add-action-properties
           slice-start (point) action (or help-echo "Open media"))))
      (when animated-p
        (appkit-media--register-inline-animation-occurrence render-image)
        (appkit-media--install-inline-animation-discovery)))))

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

(defun appkit-media--image-file-size-pixels (file)
  "Return FILE image size in pixels as (WIDTH . HEIGHT), or nil."
  (let ((probe (ignore-errors
                 (create-image file nil nil :ascent 'center))))
    (and (appkit-media-image-object-valid-p probe)
         (ignore-errors (image-size probe t)))))

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

(defun appkit-media-preview-image-from-file
    (file &optional max-width max-height)
  "Create a cacheable inline preview from FILE.

MAX-WIDTH and MAX-HEIGHT are a default-scale pixel budget and default
to `appkit-media-preview-max-width' and `appkit-media-preview-max-height'.
The returned image uses `:height (N . ch)' and `:appkit-media-nslices
N'.  N uses unscaled face metrics so `text-scale-mode' does not change
the cached descriptor.  Do not mutate the returned image.

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
          (ignore-errors
            (create-image file nil nil
                          :height height-spec
                          :appkit-media-nslices target-height-characters
                          :scale 1.0
                          :ascent 'center))))
    (unless (appkit-media-image-object-valid-p image)
      (when (image-type-available-p 'imagemagick)
        (setq image
              (ignore-errors
                (create-image file 'imagemagick nil
                              :height height-spec
                              :appkit-media-nslices
                              target-height-characters
                              :scale 1.0
                              :ascent 'center)))))
    (and (appkit-media-image-object-valid-p image)
         (appkit-media--mark-inline-animation-image image file))))

(defun appkit-media-one-line-preview-image-from-file (file &optional max-width)
  "Create a compact one-row thumbnail for local FILE.

MAX-WIDTH is the rendered pixel width.  It defaults to two current character
columns, capped by `appkit-media-preview-max-width', following Telega's
two-column, one-line thumbnail geometry.  A graphical display uses a fixed-size
center-cropped SVG thumbnail.  Displays without SVG support retain the ordinary
image decoder with explicit one-line height and maximum-width constraints.

Display with `appkit-media-one-line-image-display-string'."
  (let* ((safe-max-width
          (max 1
               (if (numberp max-width)
                   max-width
                 (min appkit-media-preview-max-width
                      (* 2 (appkit-media--char-pixel-width))))))
         (line-height (appkit-media--base-char-pixel-height)))
    (or (appkit-media--one-line-thumbnail-from-file
         file safe-max-width line-height)
        (let ((image
               (appkit-media-preview-image-from-file
                file safe-max-width line-height)))
          (when (appkit-media-image-object-valid-p image)
            (plist-put (cdr image) :max-width safe-max-width)
            image)))))

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

(defun appkit-media--buffer-uint32-be (position)
  "Read one unsigned big-endian 32-bit value at buffer POSITION."
  (+ (ash (char-after position) 24)
     (ash (char-after (1+ position)) 16)
     (ash (char-after (+ position 2)) 8)
     (char-after (+ position 3))))

(defun appkit-media--png-frame-end (start)
  "Return end position of a complete PNG frame at START, or nil."
  (let ((position (+ start 8))
        end)
    (when (and (<= (+ start 8) (point-max))
               (cl-loop for byte in '(137 80 78 71 13 10 26 10)
                        for offset from 0
                        always (= (char-after (+ start offset)) byte)))
      (catch 'done
        (while (<= (+ position 12) (point-max))
          (let* ((length (appkit-media--buffer-uint32-be position))
                 (chunk-end (+ position 12 length)))
            (when (> chunk-end (point-max))
              (throw 'done nil))
            (when (and (= (char-after (+ position 4)) ?I)
                       (= (char-after (+ position 5)) ?E)
                       (= (char-after (+ position 6)) ?N)
                       (= (char-after (+ position 7)) ?D))
              (setq end chunk-end)
              (throw 'done end))
            (setq position chunk-end))))
      end)))

(defun appkit-media-png-stream-pop-latest (&optional buffer)
  "Remove complete PNG frames from BUFFER and return only the latest.

BUFFER defaults to the current buffer and must be unibyte.  Any incomplete
trailing frame remains buffered for the next `process-filter' chunk.  Signal an
error when complete input does not begin with a PNG signature."
  (with-current-buffer (or buffer (current-buffer))
    (when enable-multibyte-characters
      (error "Appkit PNG stream buffer must be unibyte"))
    (let ((position (point-min))
          latest-start
          latest-end
          frame-end)
      (while (setq frame-end (appkit-media--png-frame-end position))
        (setq latest-start position
              latest-end frame-end
              position frame-end))
      (when (and (< position (point-max))
                 (>= (- (point-max) position) 8)
                 (not (appkit-media--png-frame-end position)))
        (unless
            (cl-loop for byte in '(137 80 78 71 13 10 26 10)
                     for offset from 0
                     always (= (char-after (+ position offset)) byte))
          (error "Appkit PNG stream contains invalid bytes")))
      (when latest-start
        (let ((latest
               (buffer-substring-no-properties latest-start latest-end)))
          (delete-region (point-min) position)
          latest)))))

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
