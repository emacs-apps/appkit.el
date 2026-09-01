;;; appkit-name-color.el --- Deterministic name color presentation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Protocol-neutral name highlighting.  Clients provide a stable identity
;; string and retain ownership of self/system distinctions and protocol
;; objects; Appkit only maps that key onto a reusable face palette.

;;; Code:

(require 'subr-x)
(require 'appkit-core)

(defface appkit-name-color-1
  '((t :foreground "LightSkyBlue"))
  "Face palette entry 1 for identity-keyed names."
  :group 'appkit)

(defface appkit-name-color-2
  '((t :foreground "PaleGreen"))
  "Face palette entry 2 for identity-keyed names."
  :group 'appkit)

(defface appkit-name-color-3
  '((t :foreground "Khaki"))
  "Face palette entry 3 for identity-keyed names."
  :group 'appkit)

(defface appkit-name-color-4
  '((t :foreground "LightSalmon"))
  "Face palette entry 4 for identity-keyed names."
  :group 'appkit)

(defface appkit-name-color-5
  '((t :foreground "Plum1"))
  "Face palette entry 5 for identity-keyed names."
  :group 'appkit)

(defface appkit-name-color-6
  '((t :foreground "LightSteelBlue"))
  "Face palette entry 6 for identity-keyed names."
  :group 'appkit)

(defface appkit-name-color-7
  '((t :foreground "Aquamarine"))
  "Face palette entry 7 for identity-keyed names."
  :group 'appkit)

(defface appkit-name-color-8
  '((t :foreground "Wheat"))
  "Face palette entry 8 for identity-keyed names."
  :group 'appkit)

(defcustom appkit-name-color-palette
  '(appkit-name-color-1
    appkit-name-color-2
    appkit-name-color-3
    appkit-name-color-4
    appkit-name-color-5
    appkit-name-color-6
    appkit-name-color-7
    appkit-name-color-8)
  "Faces used for deterministic identity-keyed name highlighting.

An empty palette disables shared name coloring.  Clients may pass a
surface-specific palette to `appkit-name-color-face' without changing this
default."
  :type '(repeat face)
  :group 'appkit)

(defun appkit-name-color--index (identity count)
  "Return a stable palette index for string IDENTITY and positive COUNT."
  (let* ((bytes (encode-coding-string identity 'utf-8-unix))
         (digest (secure-hash 'sha1 bytes))
         ;; Eight hexadecimal digits are enough to distribute a small visual
         ;; palette while keeping conversion inside an exact 32-bit range.
         (prefix (substring digest 0 8)))
    (mod (string-to-number prefix 16) count)))

(defun appkit-name-color-face (identity &optional palette)
  "Return the deterministic face for stable string IDENTITY.

PALETTE defaults to `appkit-name-color-palette' and may be a list or vector.
Return nil when IDENTITY is nil or empty, or when the selected palette is
empty.  Signal an error for other malformed inputs.  The UTF-8 hash is stable
across buffers and Emacs sessions; clients should prefer immutable protocol
identities over mutable display names."
  (let ((faces (or palette appkit-name-color-palette)))
    (unless (or (listp faces) (vectorp faces))
      (error "Appkit name color palette must be a list or vector"))
    (cond
     ((null identity) nil)
     ((not (stringp identity))
      (error "Appkit name color identity must be a string: %S" identity))
     ((or (string-empty-p identity) (zerop (length faces))) nil)
     (t
      (elt faces
           (appkit-name-color--index identity (length faces)))))))

(provide 'appkit-name-color)

;;; appkit-name-color.el ends here
