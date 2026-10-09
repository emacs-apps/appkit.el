;;; appkit-media-track.el --- Prepared horizontal media tracks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;;; Commentary:
;; A prepared track contains only a static image and immutable geometry and
;; adapters.  Each mounted occurrence borrows the existing inline host's
;; overlays, owner fence and video lease; cached source text never changes.

;;; Code:

(require 'cl-lib)
(require 'appkit-media-inline)
(require 'appkit-ui)

(cl-defstruct (appkit-media--track-descriptor
               (:constructor appkit-media--track-descriptor-create)
               (:copier nil))
  "Immutable geometry and application policy for a prepared media track."
  (plan nil :read-only t)
  (label nil :read-only t)
  (session-function nil :read-only t)
  (open-function nil :read-only t))

(cl-defstruct (appkit-media-track
               (:constructor appkit-media--track-create)
               (:copier nil))
  "One occurrence's selection; its inline host owns all runtime resources."
  (host nil :read-only t)
  (descriptor nil :read-only t)
  (index 0) poster inline-index muted)

(cl-defun appkit-media-track-image
    (items pixel-height pixel-gap &key label session-function open-function)
  "Prepare ITEMS as an immutable horizontal track, or return nil.
ITEMS contain :file, :width, :height, :display-width, :fit and :playable-p.
PIXEL-HEIGHT and PIXEL-GAP determine horizontal strip geometry.  At least
one readable preview file and SVG rendering are required.

SESSION-FUNCTION receives (INDEX MUTED) and returns an AppKit video session;
nil declines inline playback and falls back to OPEN-FUNCTION.  OPEN-FUNCTION
receives (INDEX INLINE PRESENTATION), with the exact selected live INLINE or
nil.  PRESENTATION is nil, `dedicated', `frame', or `external'.  These adapters
run in the mounted occurrence's buffer, not the preparing buffer.

Insert through the image display/slice APIs.  Surface commits mount each
occurrence independently; elsewhere call `appkit-media-inline-mount'."
  (when (cl-some (lambda (item)
                   (let ((file (plist-get item :file)))
                     (and (stringp file) (file-readable-p file))))
                 items)
    (let* ((prepared
            (cl-loop for item in items for index from 0
                     collect
                     (list :file (plist-get item :file)
                           :width (plist-get item :width)
                           :height (plist-get item :height)
                           :display-width (plist-get item :display-width)
                           :fit (plist-get item :fit)
                           :playable-p (and (plist-get item :playable-p) t)
                           :play-icon (and (plist-get item :playable-p) t)
                           :id (intern (format "appkit-media-track-%d" index)))))
           (plan (appkit-media-horizontal-strip-plan
                  prepared pixel-height pixel-gap)))
      (when plan
        ;; Freeze inferred widths so future selection cannot change the layout
        ;; when preview metadata or the calling buffer's text scale changes.
        (dolist (cell (plist-get plan :items))
          (plist-put (plist-get cell :item) :display-width (plist-get cell :width)))
        (when-let* ((image (appkit-media-horizontal-strip-image
                           prepared (plist-get plan :height) (plist-get plan :gap))))
          (cons 'image
                (plist-put
                 (copy-sequence (cdr image)) :appkit-media-track
                 (appkit-media--track-descriptor-create
                  :plan plan :label label :session-function session-function
                  :open-function open-function))))))))

(defun appkit-media-track-at-point (&optional position)
  "Return the live track at POSITION in the current buffer, or nil."
  (when-let* ((host (appkit-media-inline-host-at-point position)))
    (appkit-media-inline-host-track host)))

(defun appkit-media--track-live-host (track)
  "Return TRACK's live host, or signal a retired occurrence."
  (unless (and (appkit-media-track-p track)
               (appkit-media-inline-host-live-p (appkit-media-track-host track)))
    (user-error "Media track belongs to a retired buffer span"))
  (appkit-media-track-host track))

(defun appkit-media-track-inline (track)
  "Return TRACK's exact selected live inline, or nil."
  (let* ((host (appkit-media-track-host track))
         (inline (appkit-media-inline-host-inline host)))
    (and (appkit-media-inline-host-live-p host)
         (eql (appkit-media-track-index track)
              (appkit-media-track-inline-index track))
         (not (appkit-media-video-inline-closed-p inline))
         inline)))

(defun appkit-media--track-plan (track)
  "Return TRACK's immutable complete scene geometry."
  (appkit-media--track-descriptor-plan (appkit-media-track-descriptor track)))

(defun appkit-media--track-poster (track index)
  "Build INDEX's translated static poster, preserving TRACK's slice geometry."
  (let* ((plan (appkit-media--track-plan track))
         (cells (plist-get plan :items))
         (image (appkit-media-horizontal-strip-image
                 (mapcar (lambda (cell) (plist-get cell :item)) cells)
                 (plist-get plan :height) (plist-get plan :gap)
                 (plist-get (nth index cells) :x))))
    (unless image
      (user-error "Unable to reveal media item %d" (1+ index)))
    (let ((copy (cons 'image (copy-sequence (cdr image))))
          (poster (appkit-media-track-poster track)))
      (dolist (property '(:height :appkit-media-nslices))
        (setcdr copy (plist-put (cdr copy) property (plist-get (cdr poster) property))))
      copy)))

(defun appkit-media--track-select-index (track index poster)
  "Commit TRACK's INDEX with an already prepared POSTER, releasing its old lease."
  (let* ((host (appkit-media--track-live-host track))
         (inline (appkit-media-inline-host-inline host)))
    ;; Detach before closing: late close/present callbacks from this lease
    ;; must not overwrite the replacement's poster or selection.
    (setf (appkit-media-track-index track) index
          (appkit-media-track-poster track) poster
          (appkit-media-track-inline-index track) nil
          (appkit-media-inline-host-inline host) nil)
    (unwind-protect
        (when inline (appkit-media-video-inline-close inline))
      (appkit-media--inline-show-canvas host nil poster))))

(defun appkit-media-track-select (track delta)
  "Select TRACK's relative DELTA item, wrapping without changing source text.
Prepare replacement geometry before retiring the old inline lease."
  (let* ((host (appkit-media--track-live-host track))
         (count (length (plist-get (appkit-media--track-plan track) :items)))
         (index (mod (+ (appkit-media-track-index track) delta) count)))
    (with-current-buffer (appkit-media-inline-host-buffer host)
      (unless (= index (appkit-media-track-index track))
        (appkit-media--track-select-index
         track index (appkit-media--track-poster track index)))
      (when (eq (window-buffer (selected-window)) (current-buffer))
        (set-window-hscroll (selected-window) 0)))
    index))

(defun appkit-media--track-scene (track)
  "Create TRACK's composite Canvas without modifying any prepared image."
  (let* ((host (appkit-media-track-host track))
         (plan (appkit-media--track-plan track))
         (width (plist-get plan :width))
         (height (plist-get plan :height))
         (poster (appkit-media-track-poster track))
         (offset (or (plist-get (cdr poster) :appkit-media-strip-offset) 0))
         (anchor (car (appkit-media-inline-host-overlays host)))
         (background (video-background-color
                      (appkit-media-inline-host-buffer host) (overlay-start anchor)))
         (canvas (video-canvas-create width height background)))
    (dolist (cell (plist-get plan :items))
      (let* ((item (plist-get cell :item))
             (file (plist-get item :file)))
        (when file
          (video-canvas-draw-source
           canvas width height file (- (plist-get cell :x) offset) 0
           (plist-get cell :width) height
           :fit (or (plist-get item :fit) 'contain) :background background))))
    ;; Canvas pixels use intrinsic strip coordinates; display scaling and
    ;; image-map scaling follow the same authored height as the static SVG.
    (dolist (property '(:height :appkit-media-nslices :map :original-map
                        :appkit-media-strip-widths :appkit-media-strip-offset))
      (when-let* ((value (plist-get (cdr poster) property)))
        (setcdr canvas (plist-put (cdr canvas) property (copy-tree value t)))))
    (canvas-refresh canvas)
    canvas))

(defun appkit-media--track-release-unclaimed-session (session)
  "Close SESSION only if construction left it live without any presentation."
  (when (and (appkit-media-video-session-live-p session)
             (zerop (video-session-presentation-count
                     (appkit-media-video-session-video-session session))))
    (appkit-media-video-session-close session)))

(defun appkit-media--track-ensure-inline (track)
  "Return TRACK's selected inline, creating it lazily if policy allows."
  (or (appkit-media-track-inline track)
      (let* ((host (appkit-media--track-live-host track))
             (index (appkit-media-track-index track))
             (plan (appkit-media--track-plan track))
             (cell (nth index (plist-get plan :items)))
             (item (plist-get cell :item))
             (function (appkit-media--track-descriptor-session-function
                        (appkit-media-track-descriptor track))))
        (when (and (plist-get item :playable-p) function
                   (appkit-media--inline-canvas-p host))
          (let ((session (funcall function index (appkit-media-track-muted track)))
                inline completed)
            (when session
              (unwind-protect
                  (progn
                    (appkit-media--track-live-host track)
                    (unless (= index (appkit-media-track-index track))
                      (user-error "Media selection changed during session creation"))
                    (setq inline
                          (appkit-media-video-inline-create
                           session (plist-get cell :width) (plist-get plan :height)
                           :poster (appkit-media-track-poster track)
                           :fit (or (plist-get item :fit) 'contain)
                           :canvas (appkit-media--track-scene track)
                           :canvas-width (plist-get plan :width)
                           :canvas-height (plist-get plan :height)
                           :destination-x
                           (- (plist-get cell :x)
                              (or (plist-get (cdr (appkit-media-track-poster track))
                                             :appkit-media-strip-offset) 0))
                           :anchor (car (appkit-media-inline-host-overlays host))
                           :visible-function
                           (lambda (_) (appkit-media--inline-visible-p host))
                           :alive-function
                           (lambda (_) (appkit-media-inline-host-live-p host))
                           :activate-function
                           (lambda (native canvas)
                             (when-let* ((surface (appkit-media-inline-host-inline host))
                                         ((eq native
                                              (appkit-media-video-inline-inline surface))))
                               (appkit-media--inline-show-canvas host native canvas)))
                           :close-function
                           (lambda (surface)
                             (when (eq surface (appkit-media-inline-host-inline host))
                               (setf (appkit-media-inline-host-inline host) nil
                                     (appkit-media-track-inline-index track) nil)
                               (appkit-media--inline-show-canvas
                                host surface (appkit-media-track-poster track))))))
                    (setf (appkit-media-inline-host-inline host) inline
                          (appkit-media-track-inline-index track) index)
                    (appkit-media-video-inline-bind-controls
                     inline (appkit-media-inline-host-map host))
                    (setq completed t)
                    inline)
                (unless completed
                  (if inline
                      (appkit-media-video-inline-close inline)
                    (appkit-media--track-release-unclaimed-session session))))))))))

(defun appkit-media-track-activate (track &optional presentation)
  "Activate or toggle TRACK, or open its selected item in PRESENTATION.
PRESENTATION is nil, `dedicated', `frame', or `external'.  Frame promotion
prepares and presents a selected video if possible; other explicit
presentations forward an already live inline without starting playback."
  (unless (memq presentation '(nil dedicated frame external))
    (error "Unknown track presentation: %S" presentation))
  (let* ((host (appkit-media--track-live-host track))
         (existing (appkit-media-track-inline track))
         (open (appkit-media--track-descriptor-open-function
                (appkit-media-track-descriptor track)))
         inline completed)
    (with-current-buffer (appkit-media-inline-host-buffer host)
      (unwind-protect
          (progn
            (setq inline (if (memq presentation '(nil frame))
                             (appkit-media--track-ensure-inline track)
                           existing))
            (appkit-media--track-live-host track)
            (prog1
                (cond
                 ((and (null presentation) inline)
                  (if existing
                      (appkit-media-video-inline-toggle inline)
                    (appkit-media-video-inline-play inline)))
                 ((and inline
                       (or (eq presentation 'frame)
                           (and (eq presentation 'dedicated) (not open))))
                  (appkit-media-present-video-inline
                   inline (appkit-media--track-descriptor-label
                           (appkit-media-track-descriptor track))
                   :owner (appkit-media-inline-host-owner host)
                   :display-function (and (eq presentation 'frame)
                                          #'video-display-buffer-other-frame)))
                 (open (funcall open (appkit-media-track-index track) inline presentation))
                 (t (user-error "No media opener for this track item")))
              (setq completed t)))
        (when (and (not completed) inline (not (eq existing inline)))
          (appkit-media-video-inline-close inline))))))

(defun appkit-media--track-at-point ()
  "Return the track at point or signal an interactive error."
  (or (appkit-media-track-at-point) (user-error "No media track at point")))

(defun appkit-media-track-next ()
  "Select the next item in the track at point."
  (interactive)
  (appkit-media-track-select (appkit-media--track-at-point) 1))

(defun appkit-media-track-previous ()
  "Select the previous item in the track at point."
  (interactive)
  (appkit-media-track-select (appkit-media--track-at-point) -1))

(defun appkit-media-track-open ()
  "Activate the selected item in the track at point."
  (interactive)
  (appkit-media-track-activate (appkit-media--track-at-point)))

(defun appkit-media-track-open-dedicated ()
  "Open the selected track item in its dedicated viewer."
  (interactive)
  (appkit-media-track-activate (appkit-media--track-at-point) 'dedicated))

(defun appkit-media-track-frame ()
  "Promote the selected track video to another frame."
  (interactive)
  (appkit-media-track-activate (appkit-media--track-at-point) 'frame))

(defun appkit-media-track-open-external ()
  "Open the selected track item externally."
  (interactive)
  (appkit-media-track-activate (appkit-media--track-at-point) 'external))

(defun appkit-media-track-toggle-muted ()
  "Toggle the track's canonical audio state, including before activation."
  (interactive)
  (let* ((track (appkit-media--track-at-point))
         (inline (appkit-media-track-inline track))
         (muted (not (if inline (appkit-media-video-inline-muted-p inline)
                       (appkit-media-track-muted track)))))
    (setf (appkit-media-track-muted track) muted)
    (when inline (appkit-media-video-inline-set-muted inline muted))
    (message "Video audio %s" (if muted "muted" "unmuted"))))

(defun appkit-media-track-loop ()
  "Toggle looping for the selected initialized track video."
  (interactive)
  (let ((inline (appkit-media-track-inline (appkit-media--track-at-point))))
    (unless inline (user-error "Current media has no initialized inline video"))
    (appkit-media-video-inline-toggle-loop inline)))

(defvar appkit-media-track-map
  (let ((map (make-sparse-keymap)))
    (dolist (binding '(("<right>" . appkit-media-track-next)
                       ("<left>" . appkit-media-track-previous)
                       ("RET" . appkit-media-track-open)
                       ("<return>" . appkit-media-track-open)
                       ("C-RET" . appkit-media-track-open-dedicated)
                       ("C-<return>" . appkit-media-track-open-dedicated)
                       ("S-RET" . appkit-media-track-open-external)
                       ("S-<return>" . appkit-media-track-open-external)
                       ("m" . appkit-media-track-toggle-muted)
                       ("L" . appkit-media-track-loop)
                       ("F" . appkit-media-track-frame)))
      (define-key map (kbd (car binding)) (cdr binding)))
    map)
  "Commands shared by independently mounted track occurrences.
M-RET remains the enclosing application's action.")

(defun appkit-media--track-hotspot (track index event)
  "Activate exact INDEX in TRACK only when EVENT still belongs to its host."
  (let* ((start (event-start event))
         (window (posn-window start))
         (position (posn-point start))
         (host (appkit-media--track-live-host track)))
    (unless (and (windowp window) (window-live-p window)
                 (eq (window-buffer window) (appkit-media-inline-host-buffer host))
                 (integer-or-marker-p position)
                 (with-current-buffer (window-buffer window)
                   (eq track (appkit-media-track-at-point position))))
      (user-error "Media hotspot belongs to a retired buffer span"))
    (unless (= index (appkit-media-track-index track))
      ;; Pointer activation uses the clicked viewport, unlike keyboard reveal.
      (appkit-media--track-select-index track index (appkit-media-track-poster track)))
    (appkit-media-track-activate track)))

(defun appkit-media--track-mount (host descriptor poster position)
  "Attach typed state to HOST for DESCRIPTOR and POSTER at POSITION.
Return an occurrence-local keymap; the inline host owns all overlays/hooks."
  (let* ((track (appkit-media--track-create
                 :host host :descriptor descriptor :poster poster))
         (map (make-sparse-keymap))
         (outer (get-text-property position 'keymap)))
    (setf (appkit-media-inline-host-track host) track)
    (set-keymap-parent
     map (if outer
             (make-composed-keymap (list appkit-media-track-map outer))
           appkit-media-track-map))
    (cl-loop for cell in (plist-get (appkit-media--track-descriptor-plan descriptor) :items)
             for index from 0
             do (let ((selected index))
                  (appkit-ui-bind-image-map
                   map (plist-get cell :id)
                   (lambda (event)
                     (interactive "e")
                     (appkit-media--track-hotspot track selected event)))))
    map))

(provide 'appkit-media-track)
;;; appkit-media-track.el ends here
