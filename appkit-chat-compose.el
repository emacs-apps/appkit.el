;;; appkit-chat-compose.el --- Chat-specific multi-part compose surfaces  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Provide the chat-specific multi-part editor built on `appkit-chatbuf'.
;; Committed draft items are generated timeline rows and the trailing composer
;; edits one item at a time.  This geometry suits social post threads; ordinary
;; other clients may use `appkit-compose' only for source generations and
;; view-local effect ownership.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-chat-timeline)
(require 'appkit-compose)
(require 'appkit-chatbuf)
(require 'appkit-core)
(require 'appkit-ui)

(require 'appkit-surface)

(defvar-local appkit-chat-compose--items nil
  "Committed compose items as plists with `:id' and `:text'.")

(defvar-local appkit-chat-compose--input-item nil
  "Item plist for the current composer input, or nil.")

(defvar-local appkit-chat-compose--editing nil
  "Index of the committed item loaded into the composer, or nil.")

(defvar-local appkit-chat-compose--serial 0
  "Serial used to assign stable compose item identifiers.")

(defvar-local appkit-chat-compose--surface nil
  "Exact compose Surface, retained after stop to reject stale refresh requests.")

(defvar-local appkit-chat-compose-context-function nil
  "Function returning generated compose context text, or nil.")

(defvar-local appkit-chat-compose-status-fields-function nil
  "Function returning generated compose status field plists, or nil.")

(defvar-local appkit-chat-compose-attachments-function nil
  "Function returning a generated compose attachment section, or nil.")

(defvar-local appkit-chat-compose-parts-function nil
  "Function returning a list of compose part plists, or nil.")

(defvar-local appkit-chat-compose-footer-function nil
  "Function returning generated compose footer text, or nil.")

(defun appkit-chat-compose--callback-value (function)
  "Call FUNCTION when callable, returning nil when it is nil."
  (when function
    (unless (functionp function)
      (error "Appkit chat compose callback is not callable: %S" function))
    (funcall function)))

(defun appkit-chat-compose--attachment-string (attachment)
  "Return the generated string for one ATTACHMENT row."
  (unless (listp attachment)
    (error "Appkit chat compose attachment must be a plist: %S" attachment))
  (let* ((preview (plist-get attachment :preview))
         (label (or (plist-get attachment :label) "[attachment]"))
         (description (plist-get attachment :description))
         (description-label (or (plist-get attachment :description-label)
                                "Description"))
         (state (plist-get attachment :state))
         (text "  "))
    (unless (stringp label)
      (error "Appkit chat compose attachment label must be a string: %S"
             attachment))
    (when preview
      (setq text (concat text (propertize " " 'display preview
                                          'rear-nonsticky '(display)))))
    (setq text (concat text label))
    (when (and (stringp description)
               (not (string-empty-p description)))
      (setq text (concat text (format "  %s: %s"
                                      description-label description))))
    (when (and (stringp state)
               (not (string-empty-p state)))
      (setq text (concat text (format "  [%s]" state))))
    (concat text "\n")))

(defun appkit-chat-compose--attachments-string (section)
  "Return generated attachment SECTION text, or an empty string.

SECTION is a plist with `:title' and `:items'.  An empty item list
occupies no frame text."
  (unless (listp section)
    (error "Appkit chat compose attachment section must be a plist: %S" section))
  (let ((title (or (plist-get section :title) "Attachments"))
        (items (plist-get section :items)))
    (unless (stringp title)
      (error "Appkit chat compose attachment title must be a string: %S" section))
    (unless (listp items)
      (error "Appkit chat compose attachment items must be a list: %S" section))
    (if items
        (concat title "\n"
                (mapconcat #'appkit-chat-compose--attachment-string items ""))
      "")))

(defun appkit-chat-compose--ensure-newline (text)
  "Return TEXT ending with a newline, or an empty string."
  (cond
   ((or (null text) (string-empty-p text)) "")
   ((string-suffix-p "\n" text) text)
   (t (concat text "\n"))))

(defun appkit-chat-compose--copy-item (item)
  "Return a shallow copy of ITEM."
  (copy-sequence (or item (list :attachments nil))))

(defun appkit-chat-compose--ensure-item-id (item)
  "Return ITEM with a stable `:id' assigned when missing."
  (if (plist-get item :id)
      item
    (setq-local appkit-chat-compose--serial (1+ appkit-chat-compose--serial))
    (plist-put (appkit-chat-compose--copy-item item)
               :id appkit-chat-compose--serial)))

(defun appkit-chat-compose--item-has-consumer-metadata-p (item)
  "Return non-nil when ITEM carries meaningful client-owned metadata."
  (cl-loop for (key value) on item by #'cddr
           thereis (and (not (memq key '(:id :text)))
                        value)))

(defun appkit-chat-compose--input-text ()
  "Return the current composer input without text properties."
  (if (appkit-chatbuf-input-start-position)
      (substring-no-properties (or (appkit-chatbuf-input-string) ""))
    ""))

(defun appkit-chat-compose--merge-input (item)
  "Return ITEM with composer input text and current input metadata."
  (let ((merged (appkit-chat-compose--ensure-item-id
                 (appkit-chat-compose--copy-item
                  (or appkit-chat-compose--input-item item)))))
    (plist-put merged :text (appkit-chat-compose--input-text))))

(defun appkit-chat-compose-body ()
  "Return the current editable compose body without text properties."
  (appkit-chat-compose--input-text))

(defun appkit-chat-compose-body-region-bounds ()
  "Return the editable compose body bounds, or nil."
  (appkit-chatbuf-input-region-bounds))

(defun appkit-chat-compose-body-start-position ()
  "Return the start position of the editable compose body, or nil."
  (appkit-chatbuf-input-start-position))

(defun appkit-chat-compose-body-end-position ()
  "Return the end position of the editable compose body, or nil."
  (cdr (appkit-chatbuf-input-region-bounds)))

(defun appkit-chat-compose-bodies ()
  "Return every compose body, including the live composer input."
  (let ((texts (mapcar (lambda (item)
                         (or (plist-get item :text) ""))
                       appkit-chat-compose--items))
        (input (appkit-chat-compose-body)))
    (cond
     ((integerp appkit-chat-compose--editing)
      (setf (nth appkit-chat-compose--editing texts) input)
      texts)
     ((or (not (string-empty-p input))
          (appkit-chat-compose--item-has-consumer-metadata-p
           appkit-chat-compose--input-item)
          (null texts))
      (append texts (list input)))
     (t texts))))

(defun appkit-chat-compose-set-items (items)
  "Replace the complete chat-compose draft with ITEMS.

Each item is a plist with optional `:text' and client-owned metadata.  The last
item becomes the live composer input; earlier items become generated rows.
Programmatic replacement preserves the current source generation and operation;
use `appkit-compose-reset' explicitly to discard the compose session.
Bind `inhibit-read-only' for authorized replacement of a frozen draft."
  (barf-if-buffer-read-only)
  (unless (listp items)
    (error "Appkit chat compose items must be a list: %S" items))
  (let* ((copies
          (mapcar (lambda (item)
                    (appkit-chat-compose--ensure-item-id
                     (appkit-chat-compose--copy-item item)))
                  (or items (list (list :text "" :attachments nil)))))
         (last (car (last copies))))
    (appkit-compose-without-tracking
      (setq-local appkit-chat-compose--items (butlast copies)
                  appkit-chat-compose--input-item last
                  appkit-chat-compose--editing nil)
      (appkit-chatbuf-init-state)
      (appkit-chatbuf-input-state-set (or (plist-get last :text) ""))
      (when (appkit-chatbuf-input-start-position)
        (appkit-chatbuf-input-set-text (or (plist-get last :text) "")))
      (appkit-chat-compose-refresh))
    (set-buffer-modified-p nil)
    (appkit-chat-compose-items)))

(defun appkit-chat-compose-items ()
  "Return compose items aligned with `appkit-chat-compose-bodies'.

Committed rows keep their stored metadata.  The live composer input is
merged into the edited item, or appended as a new item."
  (let ((texts (appkit-chat-compose-bodies))
        (index 0)
        items)
    (dolist (text texts)
      (let ((item
             (cond
              ((eq index appkit-chat-compose--editing)
               (appkit-chat-compose--merge-input
                (nth index appkit-chat-compose--items)))
              ((< index (length appkit-chat-compose--items))
               (plist-put
                (appkit-chat-compose--copy-item
                 (nth index appkit-chat-compose--items))
                :text text))
              (t
               (appkit-chat-compose--merge-input
                appkit-chat-compose--input-item)))))
        (push item items))
      (setq index (1+ index)))
    (nreverse items)))

(defun appkit-chat-compose-item-index-at-point ()
  "Return the committed item index at point, or nil."
  (get-text-property (point) 'appkit-chat-compose-item-index))

(defun appkit-chat-compose-current-part-index ()
  "Return the 0-based compose part that contains point, or nil."
  (cond
   ((appkit-chatbuf-point-in-input-p)
    (or appkit-chat-compose--editing (length appkit-chat-compose--items)))
   ((appkit-chat-compose-item-index-at-point))
   (appkit-chat-compose--items
    (1- (length appkit-chat-compose--items)))
   (t 0)))

(defun appkit-chat-compose-current-item ()
  "Return the compose item plist for the current part."
  (let ((index (or (appkit-chat-compose-current-part-index) 0)))
    (cond
     ((and (integerp appkit-chat-compose--editing)
           (or (appkit-chatbuf-point-in-input-p)
               (eq index appkit-chat-compose--editing)))
      (appkit-chat-compose--merge-input
       (nth appkit-chat-compose--editing appkit-chat-compose--items)))
     ((and (integerp index)
           (< index (length appkit-chat-compose--items)))
      (nth index appkit-chat-compose--items))
     (t
      (appkit-chat-compose--merge-input appkit-chat-compose--input-item)))))

(defun appkit-chat-compose-update-current-item (item)
  "Replace the current compose item with ITEM.
Bind `inhibit-read-only' for authorized updates to a frozen draft."
  (barf-if-buffer-read-only)
  (let ((index (or (appkit-chat-compose-current-part-index) 0))
        (updated (appkit-chat-compose--copy-item item)))
    (cond
     ((and (integerp appkit-chat-compose--editing)
           (or (appkit-chatbuf-point-in-input-p)
               (eq index appkit-chat-compose--editing)))
      (setq-local appkit-chat-compose--input-item updated)
      (setf (nth appkit-chat-compose--editing appkit-chat-compose--items)
            (appkit-chat-compose--merge-input updated)))
     ((and (integerp index)
           (< index (length appkit-chat-compose--items)))
      (setf (nth index appkit-chat-compose--items)
            (appkit-chat-compose--ensure-item-id updated)))
     (t
      (setq-local appkit-chat-compose--input-item updated)))
    (appkit-compose-touch)
    (appkit-chat-compose-refresh)
    updated))

(defun appkit-chat-compose--resolved-parts ()
  "Return the client part list aligned with committed items."
  (if appkit-chat-compose-parts-function
      (let ((parts (appkit-chat-compose--callback-value
                    appkit-chat-compose-parts-function)))
        (unless (listp parts)
          (error "Appkit chat compose parts must be a list: %S" parts))
        (or parts (list nil)))
    (list (list :attachments
                (appkit-chat-compose--callback-value
                 appkit-chat-compose-attachments-function)))))

(defun appkit-chat-compose--print-row (row)
  "Insert one generated compose ROW."
  (let* ((item (appkit-chat-timeline-row-payload row))
         (index (plist-get item :index))
         (part (plist-get item :part))
         (editing (eq index appkit-chat-compose--editing))
         (title (plist-get part :title))
         (text (or (plist-get item :text) ""))
         (start (point)))
    (when (and (stringp title) (not (string-empty-p title)))
      (insert (appkit-chat-compose--ensure-newline title)))
    (when editing
      (insert (propertize "(editing in composer)\n" 'face 'shadow)))
    (insert text)
    (unless (or (string-empty-p text)
                (string-suffix-p "\n" text))
      (insert "\n"))
    (when-let* ((attachments (plist-get part :attachments)))
      (insert (appkit-chat-compose--attachments-string attachments)))
    (add-text-properties
     start (point)
     (list 'appkit-chat-compose-item-index index
           'appkit-chat-compose-item-id (plist-get item :id)))))

(defun appkit-chat-compose--timeline-entries ()
  "Return timeline entries for committed compose items."
  (let ((parts (appkit-chat-compose--resolved-parts))
        (index 0)
        entries)
    (dolist (item appkit-chat-compose--items)
      (let ((part (or (nth index parts) (car (last parts)))))
        (push (list :id (plist-get item :id)
                    :index index
                    :text (if (eq index appkit-chat-compose--editing)
                              (appkit-chat-compose--input-text)
                            (or (plist-get item :text) ""))
                    :part part)
              entries))
      (setq index (1+ index)))
    (nreverse entries)))

(defun appkit-chat-compose--header-line ()
  "Return the current compose header-line string."
  (let* ((fields (appkit-chat-compose--callback-value
                  appkit-chat-compose-status-fields-function))
         (text (appkit-compose-status-fields-string fields)))
    (when (and fields (not (listp fields)))
      (error "Appkit chat compose status fields must be a list: %S" fields))
    (or text "")))

(defun appkit-chat-compose--current-attachments ()
  "Return the attachment section for the current composer item, or nil."
  (let* ((parts (appkit-chat-compose--resolved-parts))
         (index (or (appkit-chat-compose-current-part-index) 0))
         (part (or (nth index parts) (car (last parts)))))
    (or (plist-get part :attachments)
        (appkit-chat-compose--callback-value
         appkit-chat-compose-attachments-function))))

(defun appkit-chat-compose--frame-header ()
  "Return generated header text placed above committed draft rows.

A non-empty header ends with a blank line so it does not run into
the first draft row, attachment footer, or prompt."
  (let ((text (appkit-chat-compose--ensure-newline
               (appkit-chat-compose--callback-value
                appkit-chat-compose-context-function))))
    (if (string-empty-p text)
        ""
      (concat text "\n"))))

(defun appkit-chat-compose--frame-footer ()
  "Return generated footer text placed above the composer.

A non-empty footer ends with a blank line so the prompt stays on its
own line.  Chat timelines use EWOC `nosep', so clients cannot rely on
automatic separators between header, footer, and prompt."
  (let ((text (appkit-chat-compose--ensure-newline
               (concat
                (when-let* ((section (appkit-chat-compose--current-attachments)))
                  (appkit-chat-compose--attachments-string section))
                (or (appkit-chat-compose--callback-value
                     appkit-chat-compose-footer-function)
                    "")))))
    (if (string-empty-p text)
        ""
      (concat text "\n"))))

(defun appkit-chat-compose--bind-composer ()
  "Install the trailing compose prompt and input when missing."
  (appkit-chatbuf-bind-input-region
   :visible-p t
   :prompt ">>> "
   :input-text (or (appkit-chatbuf-input-state)
                   (appkit-chat-compose--input-text)
                   "")))

(defun appkit-chat-compose--initialize-mode ()
  "Preserve a client-derived compose mode, initializing a fresh host otherwise."
  (when-let* ((surface (appkit-current-surface)))
    (unless (eq (appkit-surface-type-name (appkit-surface-type surface))
                'appkit-chat-compose)
      (error "Buffer already owns another Generated Surface")))
  (unless (derived-mode-p 'appkit-chat-compose-mode)
    (appkit-chat-compose-mode)))

(defconst appkit-chat-compose--surface-type
  (appkit-surface-type-create
   :name 'appkit-chat-compose
   :mode #'appkit-chat-compose--initialize-mode
   :init (lambda (_context _input) (appkit-next :model nil :render t))
   :update (lambda (_context model message)
             (unless (eq message 'refresh)
               (error "Unsupported compose Surface message: %S" message))
             (appkit-next :model model :render t))
   :renderer-factory #'appkit-chat-compose--renderer))

(defun appkit-chat-compose--renderer (_surface)
  "Create the renderer for the actual editable compose host."
  (appkit-generated-renderer-create
   :mount (lambda (_surface _app-read-view _model)
            (appkit-chat-timeline-ensure
             :printer #'appkit-chat-compose--print-row
             :anchor-property 'appkit-chat-compose-item-id))
   :merge (lambda (_old new) new)
   :render #'appkit-chat-compose--render
   :recover nil
   :unmount (lambda (_surface)
              (appkit-compose--shutdown))))

(defun appkit-chat-compose--ensure-surface (app)
  "Mount the current compose buffer under APP, or as a standalone Surface."
  (if-let* ((surface (appkit-current-surface)))
      (progn
        (unless (eq (appkit-surface-type-name (appkit-surface-type surface))
                    'appkit-chat-compose)
          (error "Buffer already owns another Generated Surface"))
        (when (and app (not (eq app (appkit-surface-app surface))))
          (error "Compose Surface already belongs to another App"))
        (setq-local appkit-chat-compose--surface surface))
    (setq-local appkit-chat-compose--surface
                (appkit-open-generated-surface
                 appkit-chat-compose--surface-type
                 :app app :identity (and app (list 'compose (current-buffer)))
                 :buffer (current-buffer)))))

(defun appkit-chat-compose--render (_surface _app-read-view _model _request)
  "Render generated compose rows while preserving the live editor input."
  (appkit-compose-without-tracking
    (appkit-chat-timeline-sync
     (appkit-chat-timeline-project
      (appkit-chat-compose--timeline-entries)
      (lambda (entry) (plist-get entry :id))))
    (appkit-chat-timeline-set-frame
     (appkit-chat-compose--frame-header)
     (appkit-chat-compose--frame-footer)
     :bind-input-function #'appkit-chat-compose--bind-composer
     :composer-visible-p t)
    (setq-local header-line-format '(:eval (appkit-chat-compose--header-line)))
    (force-mode-line-update))
  nil)

(defun appkit-chat-compose-refresh ()
  "Refresh compose presentation without rewriting input or reviving its host."
  (if appkit-chat-compose--surface
      (appkit-surface-send appkit-chat-compose--surface 'refresh)
    (appkit-chat-compose--ensure-surface nil))
  (appkit-chat-compose-body-region-bounds))

(defun appkit-chat-compose-display-string ()
  "Return visible compose text, including header-line chrome."
  (concat (appkit-chat-compose--header-line)
          (unless (string-empty-p (appkit-chat-compose--header-line))
            "\n")
          (buffer-substring-no-properties (point-min) (point-max))))

(defun appkit-chat-compose-goto-part (index)
  "Load compose part INDEX into the composer, or focus new input.
Navigation preserves the complete draft and its semantic generation."
  (barf-if-buffer-read-only)
  (let ((committed (and (integerp index)
                        (<= 0 index)
                        (< index (length appkit-chat-compose--items)))))
    (unless (or (eq index appkit-chat-compose--editing)
                (and (null appkit-chat-compose--editing) (not committed)))
      (appkit-compose-without-tracking
        (appkit-chat-compose--flush-input)
        (when committed
          (let ((item (nth index appkit-chat-compose--items)))
            (setq-local appkit-chat-compose--editing index
                        appkit-chat-compose--input-item
                        (appkit-chat-compose--copy-item item))
            (appkit-chatbuf-input-set-text (or (plist-get item :text) ""))))
        (appkit-chat-compose-refresh)))
    (goto-char (or (appkit-chatbuf-input-start-position) (point-max)))))

(defun appkit-chat-compose--write-back-edit ()
  "Store the current edit and leave a fresh empty composer input."
  (when (integerp appkit-chat-compose--editing)
    (setf (nth appkit-chat-compose--editing appkit-chat-compose--items)
          (appkit-chat-compose--merge-input
           (nth appkit-chat-compose--editing appkit-chat-compose--items)))
    (setq-local appkit-chat-compose--editing nil
                appkit-chat-compose--input-item (list :attachments nil))
    (appkit-chatbuf-input-set-text "")))

(defun appkit-chat-compose--flush-input ()
  "Write back an edit or commit composer input with content or metadata."
  (cond
   ((integerp appkit-chat-compose--editing)
    (appkit-chat-compose--write-back-edit))
   ((or (not (string-empty-p (appkit-chat-compose--input-text)))
        (appkit-chat-compose--item-has-consumer-metadata-p
         appkit-chat-compose--input-item))
    (setq-local appkit-chat-compose--items
                (append appkit-chat-compose--items
                        (list (appkit-chat-compose--merge-input
                               appkit-chat-compose--input-item))))
    (setq-local appkit-chat-compose--input-item (list :attachments nil))
    (appkit-chatbuf-input-set-text ""))))

(defun appkit-chat-compose-add-item ()
  "Insert an empty draft item after the current part and edit it."
  (interactive)
  (barf-if-buffer-read-only)
  (let ((index (1+ (or (appkit-chat-compose-current-part-index) 0))))
    (appkit-compose-without-tracking
      (appkit-chat-compose--flush-input)
      (setq index (min index (length appkit-chat-compose--items)))
      (let ((item (appkit-chat-compose--ensure-item-id
                   (list :text "" :attachments nil))))
        (setq-local appkit-chat-compose--items
                    (append (cl-subseq appkit-chat-compose--items 0 index)
                            (list item)
                            (cl-subseq appkit-chat-compose--items index)))
        (appkit-chat-compose-goto-part index)))
    (appkit-compose-touch)
    index))

(defun appkit-chat-compose-drop-item (&optional index)
  "Remove compose item INDEX, or the current part when INDEX is nil."
  (interactive)
  (barf-if-buffer-read-only)
  (let ((target (or index (appkit-chat-compose-current-part-index))))
    (unless (and (integerp target)
                 (<= 0 target)
                 (< target (length appkit-chat-compose--items)))
      (user-error "No committed compose item to remove"))
    (appkit-compose-without-tracking
      (when (eq target appkit-chat-compose--editing)
        (setq-local appkit-chat-compose--editing nil
                    appkit-chat-compose--input-item
                    (list :attachments nil))
        (appkit-chatbuf-input-set-text ""))
      (when (and (integerp appkit-chat-compose--editing)
                 (> appkit-chat-compose--editing target))
        (setq-local appkit-chat-compose--editing
                    (1- appkit-chat-compose--editing)))
      (setq-local appkit-chat-compose--items
                  (append (cl-subseq appkit-chat-compose--items 0 target)
                          (cl-subseq appkit-chat-compose--items (1+ target))))
      (appkit-chat-compose-refresh)
      (appkit-chat-compose-goto-part
       (min target (max 0 (1- (length appkit-chat-compose--items))))))
    (appkit-compose-touch)
    target))

(defun appkit-chat-compose-edit-at-point ()
  "Load the committed compose item at point into the composer."
  (interactive)
  (barf-if-buffer-read-only)
  (if-let* ((index (appkit-chat-compose-item-index-at-point)))
      (appkit-chat-compose-goto-part index)
    (user-error "No compose item at point")))

(defvar appkit-chat-compose-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'appkit-chat-compose-edit-at-point)
    (define-key map (kbd "e") #'appkit-chat-compose-edit-at-point)
    (define-key map (kbd "d") #'appkit-chat-compose-drop-item)
    map)
  "Keymap for generated compose draft rows.")

(define-minor-mode appkit-chat-compose-timeline-mode
  "Enable keys on generated compose draft rows."
  :init-value nil
  :lighter nil
  :keymap appkit-chat-compose-timeline-mode-map)

(cl-defun appkit-chat-compose-setup
    (&key app context-function status-fields-function attachments-function
          parts-function footer-function)
  "Configure generated compose callbacks and refresh the current buffer.

APP is an optional live appkit app that should own the compose Surface.
CONTEXT-FUNCTION returns a context string.  STATUS-FIELDS-FUNCTION returns a
list of field plists with `:label', `:value', and optional `:action'.
ATTACHMENTS-FUNCTION returns an attachment-section plist for a single part.
PARTS-FUNCTION returns a list of part plists with optional `:title' and
`:attachments'; when it is set, it replaces ATTACHMENTS-FUNCTION.
FOOTER-FUNCTION returns the generated footer string.  Client callbacks own
all protocol semantics and may be nil when a section is not needed."
  (dolist (entry `((:context . ,context-function)
                   (:status-fields . ,status-fields-function)
                   (:attachments . ,attachments-function)
                   (:parts . ,parts-function)
                   (:footer . ,footer-function)))
    (let ((function (cdr entry)))
      (when (and function (not (functionp function)))
        (error "Appkit chat compose %s callback is not callable: %S"
               (car entry) function))))
  (appkit-chat-compose--initialize-mode)
  (setq-local appkit-chat-compose-context-function context-function
              appkit-chat-compose-status-fields-function status-fields-function
              appkit-chat-compose-attachments-function attachments-function
              appkit-chat-compose-parts-function parts-function
              appkit-chat-compose-footer-function footer-function)
  (appkit-chat-compose--ensure-surface app)
  (appkit-chatbuf-use-timeline-mode #'appkit-chat-compose-timeline-mode)
  (appkit-chat-compose-refresh))

(define-derived-mode appkit-chat-compose-mode appkit-chatbuf-mode "Appkit-Chat-Compose"
  "Major mode for a standalone compose surface on a chatbuf.

Committed draft items are generated timeline rows.  The trailing
composer holds the current uncommitted or in-edit body."
  (setq-local header-line-format '(:eval (appkit-chat-compose--header-line)))
  (setq-local require-final-newline nil)
  (buffer-enable-undo)
  (setq-local appkit-chat-compose--items nil)
  (setq-local appkit-chat-compose--input-item (list :attachments nil))
  (setq-local appkit-chat-compose--editing nil)
  (setq-local appkit-chat-compose--serial 0)
  (appkit-compose-setup
   :snapshot-function #'appkit-chat-compose-items
   :source-bounds-function #'appkit-chatbuf-input-region-bounds))

(provide 'appkit-chat-compose)

;;; appkit-chat-compose.el ends here
