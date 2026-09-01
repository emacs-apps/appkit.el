;;; appkit.el --- Runtime primitives for stateful buffer applications  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el
;; Version: 0.3.0
;; Package-Requires: ((emacs "32.0") (plz "0.8") (video "0.1.0"))

;;; Commentary:

;; appkit is a runtime layer for stateful Emacs buffer applications.  It owns
;; App/Surface lifecycle, mutation boundaries, keyed history projection,
;; telega-style same-buffer chat input mechanics, media, and reusable
;; presentation geometry.  It deliberately does not own
;; application business objects, protocol adapters, or client branding.

;;; Code:

(require 'appkit-cleanup)
(require 'appkit-core)
(require 'appkit-loop)
(require 'appkit-routing)
(require 'appkit-context)
(require 'appkit-effect)
(require 'appkit-source)
(require 'appkit-command)
(require 'appkit-app)
(require 'appkit-geometry)
(require 'appkit-surface)
(require 'appkit-resource)
(require 'appkit-task-queue)
(require 'appkit-transaction)
(require 'appkit-position)
(require 'appkit-ewoc)
(require 'appkit-projection)
(require 'appkit-scroll)
(require 'appkit-compose)
(require 'appkit-compose-edit)
(require 'appkit-chat-compose)
(require 'appkit-chat-history)
(require 'appkit-chat-completion)
(require 'appkit-chat-emoji)
(require 'appkit-chat-timeline)
(require 'appkit-media)
(require 'appkit-name-color)
(require 'appkit-ui)
(require 'appkit-markup)
(require 'appkit-markup-ui)
(require 'appkit-markup-codec)
(require 'appkit-markup-markdown-ts)
(require 'appkit-markup-codecs)
(require 'appkit-markup-compose)
(require 'appkit-presentation)
(require 'appkit-mode-line)
(require 'appkit-chat-avatar)
(require 'appkit-chat-ins)
(require 'appkit-discussion)
(require 'appkit-directory)
(require 'appkit-evil)

(provide 'appkit)

;;; appkit.el ends here
