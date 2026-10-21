;;; appkit-media-playback.el --- Immutable media playback plans  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Keywords: lisp, media

;;; Commentary:

;; A playback plan is the common source of truth for inline media and
;; dedicated video presentations.  It contains no acquired bytes, player, or
;; lease; those belong to the resource coordinator and video.el.  Applications
;; may build a plan once and present it in either place.

;;; Code:

(require 'cl-lib)

(cl-defstruct (appkit-media-playback
               (:constructor appkit-media--playback-create)
               (:copier nil))
  "Immutable policy for presenting one canonical media resource."
  resource
  kind
  label
  interaction
  autoplay
  muted
  inline-autoplay
  loop
  animation
  animated-p
  resolve-function
  cache-key
  cache-directory
  cache-policy
  live
  request-headers
  animation-loop-policy
  fit)

(cl-defun appkit-media-playback-create
    (&key resource (kind 'image) label (interaction 'media)
          (autoplay t) muted inline-autoplay loop (animation 'auto)
          animated-p resolve-function cache-key cache-directory cache-policy
          live request-headers animation-loop-policy (fit 'contain))
  "Return an immutable playback plan for RESOURCE.

KIND is `image' or `video'.  INTERACTION is nil, `media', or `open'.
AUTOPLAY is eligibility for the existing image-animation path.
ANIMATION is the `auto', t, or nil directive retained for later source
capability resolution.  MUTED, INLINE-AUTOPLAY, and LOOP are explicit playback
policy; they are not inferred from KIND.  INLINE-AUTOPLAY applies only to
video and is never enabled for ordinary inline video by default.
RESOLVE-FUNCTION is used when an inline poster has no acquired resource yet.

This constructor performs no acquisition and creates no player.  CACHE-POLICY
is nil, `automatic', or `none'.  REQUEST-HEADERS are copied transport headers;
they are not part of resource or cache identity."
  (unless (memq kind '(image video))
    (error "Inline media kind must be image or video"))
  (unless (memq interaction '(nil media open))
    (error "Inline media interaction must be nil, media, or open"))
  (unless (memq cache-policy '(nil automatic none))
    (error "Unsupported media cache policy: %S" cache-policy))
  (unless (or (null resolve-function) (functionp resolve-function))
    (error "Media resolve function must be callable"))
  (appkit-media--playback-create
   :resource resource
   :kind kind
   :label label
   :interaction interaction
   :autoplay (and autoplay t)
   :muted (and muted t)
   :inline-autoplay (and inline-autoplay t)
   :loop (and loop t)
   :animation animation
   :animated-p (and animated-p t)
   :resolve-function resolve-function
   :cache-key cache-key
   :cache-directory cache-directory
   :cache-policy cache-policy
   :live (and live t)
   :request-headers (copy-tree request-headers)
   :animation-loop-policy animation-loop-policy
   :fit fit))

(provide 'appkit-media-playback)
;;; appkit-media-playback.el ends here
