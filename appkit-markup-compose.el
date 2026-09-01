;;; appkit-markup-compose.el --- Source-backed semantic composition -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;;; Commentary:

;; Telega-inspired per-send codec selection, but with one immutable semantic
;; capture shared by preview and provider encoding.  Appkit owns source parsing;
;; clients own active codec configuration, drafts, wire encoding, and transport.

;;; Code:

(require 'cl-lib)
(require 'appkit-compose)
(require 'appkit-markup-codec)
(require 'appkit-markup-codecs)
(require 'appkit-markup-ui)
(cl-defstruct (appkit-markup-compose-capture
               (:constructor appkit-markup-compose--capture-create)
               (:copier nil))
  (generation nil :read-only t)
  (codec nil :read-only t)
  (context nil :read-only t)
  (object-printer nil :read-only t)
  (parse-result nil :read-only t))

(cl-defstruct (appkit-markup-compose-output
               (:constructor appkit-markup-compose--output-create)
               (:copier nil))
  (generation nil :read-only t)
  (source-codec nil :read-only t)
  (output-codec nil :read-only t)
  (document nil :read-only t)
  (side-channels nil :read-only t)
  (source nil :read-only t)
  (losses nil :read-only t))

(defvar-local appkit-markup-compose-codecs nil
  "Client-owned ordered codec names for the current compose surface.")

(defvar-local appkit-markup-compose-active-codec nil
  "Visible active codec for the current compose surface.")

(defvar-local appkit-markup-compose-context-function nil
  "Optional function returning local codec context.")

(defvar-local appkit-markup-compose-object-classifier nil
  "Optional structured input-object classifier.")

(defvar-local appkit-markup-compose-object-printer nil
  "Optional provider object printer for compose output.")

(defun appkit-markup-compose--validate-codecs (names)
  "Return an owned, non-empty validated codec list NAMES."
  (unless (and (proper-list-p names) names)
    (signal 'appkit-markup-codec-error '(invalid-compose-codecs)))
  (let ((names (copy-sequence names)))
    (dolist (name names)
      (appkit-markup-codec name))
    (unless (= (length names) (length (delete-dups (copy-sequence names))))
      (signal 'appkit-markup-codec-error '(duplicate-compose-codec)))
    names))

(cl-defun appkit-markup-compose-setup
    (&key codecs active-codec context-function object-classifier object-printer)
  "Configure markup composition policy in the current compose buffer.

CODECS is client-owned precedence.  ACTIVE-CODEC defaults to its first member.
The three optional callbacks remain protocol-owned and run only during an
explicit capture or output operation, never during passive display."
  (let ((codecs (appkit-markup-compose--validate-codecs codecs)))
    (setq-local appkit-markup-compose-codecs codecs)
    (setq-local appkit-markup-compose-active-codec
                (or active-codec (car codecs)))
    (unless (memq appkit-markup-compose-active-codec codecs)
      (signal 'appkit-markup-codec-error '(active-codec-not-configured))))
  (dolist (function (list context-function object-classifier object-printer))
    (unless (or (null function) (functionp function))
      (signal 'appkit-markup-codec-error '(invalid-compose-callback))))
  (setq-local appkit-markup-compose-context-function context-function)
  (setq-local appkit-markup-compose-object-classifier object-classifier)
  (setq-local appkit-markup-compose-object-printer object-printer)
  appkit-markup-compose-active-codec)

(defun appkit-markup-compose--prefix-index (prefix)
  "Return Telega-style universal PREFIX codec index."
  (if (not (consp prefix))
      0
    (let ((value (car prefix))
          (index 0))
      (unless (and (integerp value) (> value 0))
        (user-error "Invalid markup codec prefix"))
      (while (> value 1)
        (unless (= (% value 4) 0)
          (user-error "Markup codec prefix is not a power of C-u"))
        (setq value (/ value 4)
              index (1+ index)))
      index)))

(defun appkit-markup-compose-select-codec (&optional prefix)
  "Return the codec selected by PREFIX for the current compose surface.

No prefix uses the active codec.  One `C-u' selects the second configured
codec, two select the third, matching Telega's send interaction."
  (unless appkit-markup-compose-codecs
    (signal 'appkit-markup-codec-error '(compose-not-configured)))
  (if (null prefix)
      appkit-markup-compose-active-codec
    (let* ((index (appkit-markup-compose--prefix-index prefix))
           (codec (nth index appkit-markup-compose-codecs)))
      (or codec (user-error "No markup codec configured at prefix index %d"
                            index)))))

(defun appkit-markup-compose-set-active-codec (name)
  "Set current active codec NAME and return it."
  (interactive
   (list
    (intern
     (completing-read
      "Markup codec: "
      (mapcar #'symbol-name appkit-markup-compose-codecs)
      nil t nil nil
      (and appkit-markup-compose-active-codec
           (symbol-name appkit-markup-compose-active-codec))))))
  (unless (memq name appkit-markup-compose-codecs)
    (user-error "Markup codec is not configured: %s" name))
  (unless (eq name appkit-markup-compose-active-codec)
    (setq-local appkit-markup-compose-active-codec name)
    (appkit-compose-touch)
    (force-mode-line-update))
  name)

(defun appkit-markup-compose-codec-label (&optional name)
  "Return presentation label for codec NAME or the active codec."
  (appkit-markup-codec-label
   (appkit-markup-codec (or name appkit-markup-compose-active-codec))))

(defun appkit-markup-compose--context ()
  "Return client context for the current explicit compose operation."
  (when appkit-markup-compose-context-function
    (funcall appkit-markup-compose-context-function)))

(defun appkit-markup-compose-parse-capture (capture codec)
  "Parse generic compose CAPTURE through CODEC.

CAPTURE is the plist returned by `appkit-compose-capture'."
  (let ((generation (plist-get capture :generation))
        (source (plist-get capture :value))
        (context (appkit-markup-compose--context)))
    (unless (and (integerp generation) (>= generation 0) (stringp source))
      (signal 'appkit-markup-codec-error '(invalid-compose-capture)))
    (appkit-markup-compose--capture-create
     :generation generation
     :codec codec
     :context context
     :object-printer appkit-markup-compose-object-printer
     :parse-result
     (appkit-markup-parse
      codec (copy-sequence source)
      :context context
      :object-classifier appkit-markup-compose-object-classifier))))

(defun appkit-markup-compose-capture (&optional prefix)
  "Capture and parse current semantic source using PREFIX codec selection."
  (let ((codec (appkit-markup-compose-select-codec prefix)))
    (appkit-markup-compose-parse-capture
     (appkit-compose-capture) codec)))

(defun appkit-markup-compose-document (capture)
  "Return the immutable semantic document from CAPTURE."
  (unless (appkit-markup-compose-capture-p capture)
    (signal 'appkit-markup-codec-error '(invalid-compose-capture)))
  (appkit-markup-parse-result-document
   (appkit-markup-compose-capture-parse-result capture)))

(cl-defun appkit-markup-compose-preview
    (capture &key prefix properties (final-newline-p t))
  "Insert a pure native preview of CAPTURE at point.

No client action, object renderer, transport, hook, or codec callback runs after
capture.  PREFIX and PROPERTIES have the native markup UI meanings."
  (appkit-markup-ui-insert-document
   (appkit-markup-compose-document capture)
   :prefix prefix :properties properties
   :final-newline-p final-newline-p
   :interactive-p nil))

(cl-defun appkit-markup-compose-output
    (capture output-codec &key object-printer)
  "Print CAPTURE once for provider OUTPUT-CODEC.

The returned value carries the exact capture generation, semantic document,
side channels, property-preserving source, and losses.  It is transport-ready
client input, not a transport operation."
  (unless (appkit-markup-compose-capture-p capture)
    (signal 'appkit-markup-codec-error '(invalid-compose-capture)))
  (let* ((parse-result
          (appkit-markup-compose-capture-parse-result capture))
         (document (appkit-markup-parse-result-document parse-result))
         (printed
          (appkit-markup-print
           output-codec document
           :context (appkit-markup-compose-capture-context capture)
           :object-printer
           (or object-printer
               (appkit-markup-compose-capture-object-printer capture)))))
    (appkit-markup-compose--output-create
     :generation (appkit-markup-compose-capture-generation capture)
     :source-codec (appkit-markup-compose-capture-codec capture)
     :output-codec output-codec
     :document document
     :side-channels (copy-tree
                     (appkit-markup-parse-result-side-channels parse-result))
     :source (copy-sequence (appkit-markup-print-result-source printed))
     :losses (copy-sequence (appkit-markup-print-result-losses printed)))))

(provide 'appkit-markup-compose)

;;; appkit-markup-compose.el ends here
