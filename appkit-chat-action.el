;;; appkit-chat-action.el --- Ephemeral chat activity -*- lexical-binding: t; -*-

;;; Commentary:
;; Applications own identity, protocol states and wording.  This module owns
;; common expiry, lifecycle-safe wakeups and edit-driven send throttling.
;; Entries are plists with :expires-at, :updated-at and optional :display-name.
;; Timers are execution resources, never part of an application's data snapshot.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'appkit-core)

(defun appkit-chat-action-live-p (entry &optional now)
  "Whether ENTRY expires strictly after NOW, defaulting to the current time."
  (let ((expiry (plist-get entry :expires-at)))
    (and (numberp expiry) (> expiry (or now (float-time))))))

(defun appkit-chat-action-active-entries (entries &optional now)
  "Return live ENTRIES ordered by recent activity, then display name.
Do not mutate caller-owned lists or entries."
  (let ((now (or now (float-time))))
    (sort (seq-filter (lambda (entry) (appkit-chat-action-live-p entry now)) entries)
          (lambda (left right)
            (let ((a (or (plist-get left :updated-at) 0))
                  (b (or (plist-get right :updated-at) 0)))
              (if (= a b)
                  (string-lessp (downcase (or (plist-get left :display-name) ""))
                                (downcase (or (plist-get right :display-name) "")))
                (> a b)))))))

(defun appkit-chat-action-next-expiry (entries)
  "Return the earliest numeric deadline in ENTRIES, or nil."
  (let (next)
    (dolist (entry entries next)
      (when-let* ((expiry (plist-get entry :expires-at))
                  ((numberp expiry)))
        (setq next (if next (min next expiry) expiry))))))

(defun appkit-chat-action-watch-expiry (owner expiry callback)
  "Call CALLBACK once at EXPIRY, while Appkit OWNER still owns the timer.
Return an Appkit handle; cancel it before replacing the deadline.  Late
callbacks from a cancelled handle or stopped owner cannot affect a successor.
CALLBACK should commit state or enqueue rendering, not render synchronously."
  (when (and expiry (appkit-owner-live-p owner))
    (let (handle)
      (setq handle
            (appkit-register-handle
             owner 'timer
             (run-at-time
              (max 0.01 (- expiry (float-time))) nil
              (lambda ()
                (when (and (appkit-handle-alive-p handle)
                           (appkit-owner-live-p owner))
                  (appkit-cancel-handle handle)
                  (funcall callback))))))
      handle)))

(cl-defstruct (appkit-chat-action-sender
               (:constructor appkit-chat-action-sender-create))
  "One composer's edit-driven outgoing activity, not its unsent draft."
  action
  sent-at)

(defun appkit-chat-action-send (sender action interval send-function &optional now)
  "Report ACTION through SEND-FUNCTION when changed or INTERVAL has elapsed.
Nil ACTION clears previously reported activity once.  Call this on actual
input edits; an untouched draft must never keep advertising activity.
Return non-nil when a send was attempted.  Attempts, including failed ones,
are throttled, so an unavailable service cannot cause one request per key."
  (let ((now (or now (float-time))))
    (when (or (not (eq action (appkit-chat-action-sender-action sender)))
              (and action
                   (or (null (appkit-chat-action-sender-sent-at sender))
                       (>= (- now (appkit-chat-action-sender-sent-at sender)) interval))))
      (setf (appkit-chat-action-sender-action sender) action
            (appkit-chat-action-sender-sent-at sender) now)
      (funcall send-function action)
      t)))

(provide 'appkit-chat-action)
;;; appkit-chat-action.el ends here
