;;; appkit-media-card.el --- Backend-neutral media card actions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Text-property and callback protocol shared by media card renderers.  Media
;; payloads remain application-owned; appkit only locates a card context and
;; dispatches its zero-argument actions.

;;; Code:

(require 'cl-lib)
(require 'appkit-ui)

(defconst appkit-media-card-context-property 'appkit-media-card-context
  "Text property carrying a backend-neutral media card context.")

(defvar-local appkit-media-card-fallback-context-function nil
  "Function returning a media card context when point is outside a card.

Applications may use this to select the primary media object for the current
message.  A context stored in `appkit-media-card-context-property' at point or
at the beginning of its line always takes precedence, so messages containing
multiple media cards remain segment-aware.")

(defun appkit-media-add-action-properties (start end callback help-echo)
  "Attach CALLBACK mouse and keyboard actions between START and END.

CALLBACK is a zero-argument function.  HELP-ECHO defaults to `Activate'.  Do
nothing when CALLBACK is not callable or the region is empty."
  (appkit-ui-add-action start end callback :help-echo help-echo))

(cl-defun appkit-media-card-context-create
    (&key payload kind title open-action download-action cancel-action
          save-as-action copy-url-action)
  "Create and return a backend-neutral media card action context.

PAYLOAD remains owned by the application adapter.  KIND and TITLE are
presentation hints.  OPEN-ACTION, DOWNLOAD-ACTION, CANCEL-ACTION,
SAVE-AS-ACTION, and COPY-URL-ACTION are optional zero-argument functions."
  (list :payload payload
        :kind kind
        :title title
        :open-action open-action
        :download-action download-action
        :cancel-action cancel-action
        :save-as-action save-as-action
        :copy-url-action copy-url-action))

(defun appkit-media-card-context-at-point (&optional position)
  "Return the media card context at POSITION.

POSITION defaults to point.  Look first at POSITION, then at the beginning of
its line.  When neither carries a context, call the buffer-local fallback in
`appkit-media-card-fallback-context-function'.  An out-of-range POSITION has no
text-property context and therefore proceeds directly to the fallback."
  (let* ((position (or position (point)))
         (in-buffer-p (and (integer-or-marker-p position)
                           (<= (point-min) position)
                           (<= position (point-max))))
         (line-position
          (and in-buffer-p
               (save-excursion
                 (goto-char position)
                 (line-beginning-position))))
         (context
          (or (and in-buffer-p
                   (< position (point-max))
                   (get-text-property
                    position appkit-media-card-context-property))
              (and line-position
                   (< line-position (point-max))
                   (get-text-property
                    line-position appkit-media-card-context-property)))))
    (or context
        (when (functionp appkit-media-card-fallback-context-function)
          (funcall appkit-media-card-fallback-context-function)))))

(defun appkit-media-card-action-function (action &optional context)
  "Return ACTION's callback from media card CONTEXT.

When CONTEXT is nil, use `appkit-media-card-context-at-point'."
  (let ((context (or context (appkit-media-card-context-at-point))))
    (and context
         (plist-get context
                    (intern (format ":%s-action" action))))))

(defun appkit-media-card-action-inapt-reason (action &optional context)
  "Return why media card ACTION is unavailable, or nil when it is apt.

When CONTEXT is nil, use the media card context at point."
  (let ((context (or context (appkit-media-card-context-at-point))))
    (cond
     ((null context) "No media at point")
     ((not (functionp
            (appkit-media-card-action-function action context)))
      (format "%s unavailable" (capitalize (symbol-name action))))
     (t nil))))

(defun appkit-media-card-call-action (action &optional context)
  "Invoke media card ACTION from CONTEXT or the context at point."
  (let* ((context (or context (appkit-media-card-context-at-point)))
         (callback
          (and context
               (appkit-media-card-action-function action context))))
    (unless context
      (user-error "No media at point"))
    (unless (functionp callback)
      (user-error "Media action `%s' is unavailable" action))
    (funcall callback)))

(defun appkit-media-card-open ()
  "Open or play the media card at point."
  (interactive)
  (appkit-media-card-call-action 'open))

(defun appkit-media-card-download ()
  "Download or retry the media card at point."
  (interactive)
  (appkit-media-card-call-action 'download))

(defun appkit-media-card-cancel-download ()
  "Cancel the media card download at point."
  (interactive)
  (appkit-media-card-call-action 'cancel))

(defun appkit-media-card-save-as ()
  "Save the media card at point to a chosen file."
  (interactive)
  (appkit-media-card-call-action 'save-as))

(defun appkit-media-card-copy-url ()
  "Copy the media card URL at point."
  (interactive)
  (appkit-media-card-call-action 'copy-url))

(provide 'appkit-media-card)

;;; appkit-media-card.el ends here
