;;; appkit-mode-line.el --- Reusable mode-line presentation helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Client-neutral helpers for installing cached mode-line providers and
;; rendering clickable indicators.  Protocol-specific state and hooks stay in
;; the consuming client.

;;; Code:

(require 'cl-lib)

(defun appkit-mode-line-install (provider)
  "Install mode-line PROVIDER in `mode-line-misc-info' once."
  (unless (member provider mode-line-misc-info)
    (setq mode-line-misc-info
          (append mode-line-misc-info (list provider)))))

(defun appkit-mode-line-uninstall (provider)
  "Remove all occurrences of mode-line PROVIDER."
  (setq mode-line-misc-info (delete provider mode-line-misc-info)))

(defun appkit-mode-line-update-cache (cache-symbol format)
  "Set CACHE-SYMBOL to formatted mode-line FORMAT and return its value.

This function deliberately does not force a redisplay.  Runtime and network
callbacks may update presentation state here, while Emacs consumes the new
cache during its normal redisplay cycle.  Interactive commands that install or
remove a provider may call `force-mode-line-update' at their command boundary."
  (set cache-symbol (format-mode-line format))
  (symbol-value cache-symbol))

(cl-defun appkit-mode-line-indicator
    (text &key prefix face command help-echo)
  "Return a mode-line indicator displaying TEXT.

PREFIX is prepended only when TEXT is non-nil.  FACE styles the indicator.
COMMAND is invoked by the primary mouse button and HELP-ECHO describes that
action."
  (when text
    (concat
     (or prefix "")
     (apply #'propertize
            text
            (append
             (when face (list 'face face))
             (when command
               (list 'local-map (make-mode-line-mouse-map 'mouse-1 command)
                     'mouse-face 'mode-line-highlight))
             (when help-echo (list 'help-echo help-echo)))))))

(provide 'appkit-mode-line)

;;; appkit-mode-line.el ends here
