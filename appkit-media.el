;;; appkit-media.el --- Shared media runtime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Aggregates protocol-neutral media card, image, resource, playback, and video
;; primitives.  Applications remain responsible for adapting backend objects
;; and for owning transfer/cache state visible in their user interfaces.

;;; Code:

(require 'appkit-media-card)
(require 'appkit-media-image)
(require 'appkit-media-resource)
(require 'appkit-media-effect)
(require 'appkit-media-player)
(require 'appkit-media-video)

(provide 'appkit-media)

;;; appkit-media.el ends here
