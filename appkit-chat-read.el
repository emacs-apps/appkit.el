;;; appkit-chat-read.el --- Shared chat reading state -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;;; Commentary:

;; Protocol adapters retain authoritative read receipts and message ordering.
;; This controller separates that moving frontier from the stable boundary at
;; which the current reading session began.  Keys are opaque and exact; counts
;; cannot identify a boundary.  State belongs to the current Surface, not to an
;; account or to a reused buffer name.

;;; Code:

(require 'cl-lib)
(require 'appkit-chat-timeline)
(require 'appkit-chat-ins)

(defcustom appkit-chat-show-unread-divider t
  "Whether chat timelines display their reading-session unread boundary."
  :type 'boolean
  :group 'appkit)

(defface appkit-chat-unread-divider
  '((t :inherit font-lock-keyword-face :weight bold))
  "Shared separator before a chat reading session's first unread message."
  :group 'appkit)

(cl-defstruct (appkit-chat-read--state
               (:constructor appkit-chat-read--state-create))
  owner
  current-key
  boundary-key
  dismissed-p)

(defvar-local appkit-chat-read--state nil
  "Read presentation state owned by this buffer's current Surface.")

(defun appkit-chat-read--current-state ()
  "Return reading state, replacing it when the owning Surface changes."
  (let ((owner (or (appkit-current-surface) (current-buffer))))
    (unless (and (appkit-chat-read--state-p appkit-chat-read--state)
                 (eq owner (appkit-chat-read--state-owner
                            appkit-chat-read--state)))
      (setq appkit-chat-read--state
            (appkit-chat-read--state-create :owner owner)))
    appkit-chat-read--state))

(cl-defun appkit-chat-read-update
    (keys &key first-unread-key (known-p t))
  "Return the stable unread boundary present in ordered message KEYS.

FIRST-UNREAD-KEY is an exact protocol-authoritative key, possibly outside the
loaded window.  With KNOWN-P nil, retain the previous authoritative frontier.
A known nil key means no known unread message, not an unknown position.

The first unread key latches independently of subsequent read receipts.  It
remains fixed across acknowledgements, history loading and redraws.  A missing
key is not replaced by a nearby message.  Explicit read-all may dismiss the
boundary with `appkit-chat-read-clear'; explicit unread navigation may choose
the current frontier with `appkit-chat-read-rebase'."
  (let ((state (appkit-chat-read--current-state)))
    (when known-p
      (unless (equal first-unread-key
                     (appkit-chat-read--state-current-key state))
        (setf (appkit-chat-read--state-current-key state) first-unread-key
              (appkit-chat-read--state-dismissed-p state) nil))
      (unless (or (appkit-chat-read--state-boundary-key state)
                  (appkit-chat-read--state-dismissed-p state))
        (setf (appkit-chat-read--state-boundary-key state) first-unread-key)))
    (let ((boundary (appkit-chat-read--state-boundary-key state)))
      (and appkit-chat-show-unread-divider boundary
           (member boundary keys) boundary))))

(defun appkit-chat-read-clear ()
  "Dismiss the current reading boundary after explicit read-all succeeds.
Do not re-latch the same stale authoritative frontier on the next redraw."
  (let ((state (appkit-chat-read--current-state)))
    (setf (appkit-chat-read--state-boundary-key state) nil
          (appkit-chat-read--state-dismissed-p state) t)))

(defun appkit-chat-read-rebase ()
  "Choose the latest authoritative frontier as this session's boundary.
Use for explicit unread navigation, not for ordinary read receipts."
  (let ((state (appkit-chat-read--current-state)))
    (setf (appkit-chat-read--state-dismissed-p state) nil
          (appkit-chat-read--state-boundary-key state)
          (appkit-chat-read--state-current-key state))))

(defun appkit-chat-read-target (&optional position eligible-p)
  "Return the exact projected message row represented by POSITION or point.
ELIGIBLE-P, when non-nil, receives a row and decides its read capability.
Point in the composer means the newest eligible loaded row, never an unseen
account-wide message.  Separator text and non-message buffer regions have no
read target.  This function neither moves point nor acknowledges anything."
  (let ((position (or position (point))))
    (when (and (appkit-chat-timeline-live-p)
               (<= (point-min) position) (<= position (point-max))
               (not (get-text-property position 'appkit-chat-read-divider)))
      (if (appkit-chatbuf-point-in-input-p position)
          (let* ((ewoc (appkit-chat-timeline-ewoc))
                 (node (ewoc-nth ewoc -1))
                 target)
            (while (and node (not target))
              (let ((row (ewoc-data node)))
                (when (or (null eligible-p) (funcall eligible-p row))
                  (setq target row)))
              (setq node (ewoc-prev ewoc node)))
            target)
        (when-let* ((key (appkit-chat-timeline-key-at-point position))
                    (row (appkit-chat-timeline-row key)))
          (and (or (null eligible-p) (funcall eligible-p row)) row))))))

(defun appkit-chat-read-insert-divider (width &optional properties)
  "Insert the shared unread divider across WIDTH, with extra PROPERTIES.
The divider is presentation, never itself a read-capable message target."
  (appkit-chat-ins-insert-divider-row
   "Unread Messages" 'appkit-chat-unread-divider width
   (append properties '(appkit-chat-read-divider t))))

(provide 'appkit-chat-read)

;;; appkit-chat-read.el ends here
