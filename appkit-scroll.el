;;; appkit-scroll.el --- Window-edge observation for Appkit views  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-apps/appkit.el
;; Version: 0.3.0
;; Package-Requires: ((emacs "31.1"))

;;; Commentary:

;; This module centralizes window geometry and hook lifecycle for applications
;; that page content near a buffer edge.  It deliberately does not own cursors,
;; loading state, exhaustion, request cancellation, or retry policy.

;;; Code:

(require 'cl-lib)
(require 'appkit-core)
(require 'appkit-surface)

(cl-defstruct (appkit-scroll-observer
               (:constructor appkit-scroll-observer--create))
  "Window-edge observer owned by one Generated Surface."
  owner
  buffer
  start-boundary-function
  end-boundary-function
  start-function
  end-function
  post-command-function
  window-scroll-function
  handles
  deferred-check-handle
  checking-p
  active-p
  visibility-function
  visible-p
  window-state-function)

(defun appkit-scroll-near-start-p (position start threshold)
  "Return non-nil when POSITION is within THRESHOLD characters of START.

Nil THRESHOLD disables the predicate.  Negative thresholds behave as zero."
  (and (numberp position)
       (numberp start)
       (numberp threshold)
       (< position (+ start (max 0 threshold)))))

(defun appkit-scroll-near-end-p (position end threshold)
  "Return non-nil when POSITION is within THRESHOLD characters of END.

Nil THRESHOLD disables the predicate.  Negative thresholds behave as zero."
  (and (numberp position)
       (numberp end)
       (numberp threshold)
       (> position (- end (max 0 threshold)))))

(defun appkit-scroll-window-visible-start-position
    (window &optional start-bound)
  "Return WINDOW's visible start in the current buffer, or nil.

Numeric START-BOUND clamps the result after application-owned leading content."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (when-let* ((visible-start (window-start window)))
      (if (numberp start-bound)
          (max visible-start start-bound)
        visible-start))))

(defun appkit-scroll-window-visible-end-position (window &optional end-bound)
  "Return WINDOW's verified visible end in the current buffer, or nil.

Numeric END-BOUND clamps the result before application-owned trailing content.
An unredisplayed window may report `point-max' even when that position is not
visible; reject such stale measurements instead of triggering edge actions."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (when-let* ((visible-end (window-end window t))
                (probe (max (point-min) (1- visible-end)))
                ((pos-visible-in-window-p probe window t)))
      (if (numberp end-bound)
          (min visible-end end-bound)
        visible-end))))

(defun appkit-scroll-window-visible-range
    (window &optional start-bound end-bound)
  "Return WINDOW's visible range in the current buffer.

The result is a cons cell (START . END), or nil when WINDOW is dead, displays a
foreign buffer, or has no measurable end.  Numeric START-BOUND and END-BOUND
clamp the result to application-owned content, excluding headers, footers, or
composers when required."
  (when-let* ((visible-start
               (appkit-scroll-window-visible-start-position
                window start-bound))
              (visible-end
               (appkit-scroll-window-visible-end-position window end-bound)))
    (cons visible-start visible-end)))

(defun appkit-scroll-observer--boundary (function fallback)
  "Call boundary FUNCTION, or return FALLBACK when FUNCTION is nil."
  (if function (funcall function) fallback))

(defun appkit-scroll-observer--check-window (observer window)
  "Run OBSERVER callbacks for live WINDOW.

Return `checked' after a reliable measurement, `unmeasured' when WINDOW is
eligible but has not been redisplayed reliably, deferred during an active
client pass, and nil otherwise."
  (let ((owner (appkit-scroll-observer-owner observer))
        (buffer (appkit-scroll-observer-buffer observer)))
    (when (and (appkit-scroll-observer-active-p observer)
               (not (appkit-scroll-observer-checking-p observer))
               (appkit-surface-live-p owner)
               (buffer-live-p buffer)
               (window-live-p window)
               (eq (window-buffer window) buffer))
      (if appkit-loop--active-loop
          (progn
            (appkit-scroll-observer--defer-check observer)
            'deferred)
        (with-current-buffer buffer
          (let* ((start
                  (appkit-scroll-observer--boundary
                   (appkit-scroll-observer-start-boundary-function observer)
                   (point-min)))
                 (end
                  (appkit-scroll-observer--boundary
                   (appkit-scroll-observer-end-boundary-function observer)
                   (point-max)))
                 (range
                  (appkit-scroll-window-visible-range window start end)))
            (if (not range)
                'unmeasured
              (setf (appkit-scroll-observer-checking-p observer) t)
              (unwind-protect
                  (progn
                    (when-let* ((function
                                 (appkit-scroll-observer-start-function
                                  observer)))
                      (funcall function window (car range) start))
                    (when-let* ((function
                                 (appkit-scroll-observer-end-function observer)))
                      (funcall function window (cdr range) end)))
                (setf (appkit-scroll-observer-checking-p observer) nil))
              'checked)))))))

(defun appkit-scroll-observer--run-deferred-check (observer)
  "Run OBSERVER's one deferred post-redisplay measurement."
  (let ((handle (appkit-scroll-observer-deferred-check-handle observer)))
    (setf (appkit-scroll-observer-deferred-check-handle observer) nil
          (appkit-scroll-observer-handles observer)
          (delq handle (appkit-scroll-observer-handles observer)))
    (when handle
      (appkit-retire-handle handle)))
  (when (and (appkit-scroll-observer-active-p observer)
             (appkit-surface-live-p (appkit-scroll-observer-owner observer)))
    (appkit-scroll-observer--check-visibility observer)
    (dolist (window
             (get-buffer-window-list
              (appkit-scroll-observer-buffer observer) nil t))
      (appkit-scroll-observer--check-window observer window))))

(defun appkit-scroll-observer--defer-check (observer)
  "Schedule one lifecycle-owned post-redisplay check for OBSERVER."
  (when (and (appkit-scroll-observer-active-p observer)
             (appkit-surface-live-p (appkit-scroll-observer-owner observer))
             (not (appkit-scroll-observer-deferred-check-handle observer)))
    (let* ((timer
            (run-with-idle-timer
             0 nil #'appkit-scroll-observer--run-deferred-check observer))
           (handle
            (appkit-register-handle
             (appkit-scroll-observer-owner observer) 'timer timer)))
      (setf (appkit-scroll-observer-deferred-check-handle observer) handle
            (appkit-scroll-observer-handles observer)
            (cons handle (appkit-scroll-observer-handles observer))))))

(defun appkit-scroll-observer-check (observer &optional window)
  "Check OBSERVER against WINDOW or every window displaying its buffer.

Applications should call this after committing a page to their projection so a
short result can immediately request another eligible page.  Edge callbacks
must synchronously close their loading gate before starting asynchronous work.
An unredisplayed window defers one lifecycle-owned check instead of exposing a
stale visible edge."
  (unless (appkit-scroll-observer-p observer)
    (error "Appkit scroll observer is invalid: %S" observer))
  (appkit-scroll-observer--check-visibility observer)
  (let ((unmeasured-p nil))
    (if window
        (setq unmeasured-p
              (eq 'unmeasured
                  (appkit-scroll-observer--check-window observer window)))
      (dolist (candidate
               (get-buffer-window-list
                (appkit-scroll-observer-buffer observer) nil t))
        (when (eq 'unmeasured
                  (appkit-scroll-observer--check-window observer candidate))
          (setq unmeasured-p t))))
    (when unmeasured-p
      (appkit-scroll-observer--defer-check observer))))

(defun appkit-scroll-observer-cancel (observer)
  "Cancel OBSERVER and remove its registered hooks exactly once."
  (when (and (appkit-scroll-observer-p observer)
             (appkit-scroll-observer-active-p observer))
    (setf (appkit-scroll-observer-active-p observer) nil)
    (dolist (handle (appkit-scroll-observer-handles observer))
      (appkit-cancel-handle handle))
    (setf (appkit-scroll-observer-handles observer) nil
          (appkit-scroll-observer-deferred-check-handle observer) nil)
    t))

(cl-defun appkit-scroll-observer-install
    (surface &key start-boundary-function end-boundary-function
             start-function end-function visibility-function)
  "Install and return a window-edge observer owned by SURFACE.

START-BOUNDARY-FUNCTION and END-BOUNDARY-FUNCTION run in SURFACE's buffer and
return numeric application-content boundaries.  Their defaults are
`point-min' and `point-max'.

START-FUNCTION and END-FUNCTION receive (WINDOW POSITION BOUNDARY).  POSITION
is the actual visible edge, including mouse-wheel, scroll-bar, keyboard, and
indirect-window scrolling and window resizing.  Callbacks own proximity
thresholds and all
application request gates.

VISIBILITY-FUNCTION receives a boolean when the buffer gains or loses its
last displayed window, including when another window or frame closes it.
Callbacks run after redisplay in the Surface buffer; hiding one of several
windows does not report a loss of visibility.  At least one callback is needed."
  (unless (appkit-surface-live-p surface)
    (error "Cannot observe scrolling for a dead Generated Surface"))
  (unless (or (functionp start-function) (functionp end-function)
              (functionp visibility-function))
    (error "Appkit scroll observer needs an edge or visibility callback"))
  (dolist (function (list start-boundary-function end-boundary-function
                          start-function end-function visibility-function))
    (unless (or (null function) (functionp function))
      (error "Appkit scroll observer callback is invalid: %S" function)))
  (let* ((buffer (appkit-surface-buffer surface))
         (observer
          (appkit-scroll-observer--create
           :owner surface
           :buffer buffer
           :start-boundary-function start-boundary-function
           :end-boundary-function end-boundary-function
           :start-function start-function
           :end-function end-function
           :visibility-function visibility-function
           :visible-p (and (get-buffer-window-list buffer nil t) t)
           :active-p t))
         (post-command-function
          (lambda ()
            (appkit-scroll-observer--defer-check observer)))
         (window-scroll-function
          (lambda (_window &optional _display-start)
            (appkit-scroll-observer--defer-check observer)))
         (window-state-function
          (lambda (_frame)
            (when (or (appkit-scroll-observer-visible-p observer)
                      (get-buffer-window-list buffer nil t))
              (appkit-scroll-observer--defer-check observer)))))
    (setf (appkit-scroll-observer-post-command-function observer)
          post-command-function
          (appkit-scroll-observer-window-scroll-function observer)
          window-scroll-function
          (appkit-scroll-observer-window-state-function observer)
          window-state-function)
    (with-current-buffer buffer
      (add-hook 'post-command-hook post-command-function nil t)
      (add-hook 'window-scroll-functions window-scroll-function nil t)
      (add-hook 'window-size-change-functions window-scroll-function nil t))
    (setf (appkit-scroll-observer-handles observer)
          (list
           (appkit-register-handle
            surface 'hook
            (list 'post-command-hook post-command-function t buffer))
           (appkit-register-handle
            surface 'hook
            (list 'window-scroll-functions window-scroll-function t buffer))
           (appkit-register-handle
            surface 'hook
            (list 'window-size-change-functions window-scroll-function t buffer))
           (appkit-register-handle
            surface 'function
            (lambda ()
              (setf (appkit-scroll-observer-active-p observer) nil)))))
    (when visibility-function
      (dolist (hook '(window-state-change-functions delete-frame-functions))
        (add-hook hook window-state-function)
        (push (appkit-register-handle
               surface 'hook (list hook window-state-function nil nil))
              (appkit-scroll-observer-handles observer))))
    observer))

(defun appkit-scroll-observer--check-visibility (observer)
  "Notify OBSERVER when its buffer gains or loses its last displayed window."
  (when (and (appkit-scroll-observer-active-p observer)
             (not (appkit-scroll-observer-checking-p observer))
             (appkit-surface-live-p (appkit-scroll-observer-owner observer))
             (buffer-live-p (appkit-scroll-observer-buffer observer))
             (appkit-scroll-observer-visibility-function observer))
    (if appkit-loop--active-loop
        (appkit-scroll-observer--defer-check observer)
      (let ((visible (and (get-buffer-window-list
                           (appkit-scroll-observer-buffer observer) nil t) t)))
        (unless (eq visible (appkit-scroll-observer-visible-p observer))
          (setf (appkit-scroll-observer-visible-p observer) visible
                (appkit-scroll-observer-checking-p observer) t)
          (unwind-protect
              (with-current-buffer (appkit-scroll-observer-buffer observer)
                (funcall (appkit-scroll-observer-visibility-function observer) visible))
            (setf (appkit-scroll-observer-checking-p observer) nil)))))))

(defun appkit-scroll-visible-property-values (property)
  "Return distinct PROPERTY values intersecting this buffer's visible windows."
  (let ((seen (make-hash-table :test #'equal)) values)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (when-let* ((range (appkit-scroll-window-visible-range window)))
        (let ((position (car range)) (end (cdr range)))
          (while (< position end)
            (when-let* ((value (get-text-property position property))
                        ((not (gethash value seen))))
              (puthash value t seen)
              (push value values))
            (setq position (next-single-property-change position property nil end))))))
    (nreverse values)))

(defun appkit-scroll-observe-property (surface property function)
  "Call FUNCTION with SURFACE when its visible PROPERTY values change.
Use the same Surface-owned observer as history paging.  FUNCTION runs in
the Surface buffer, including when its last visible window disappears."
  (let* ((previous (appkit-scroll-visible-property-values property))
         (check (lambda (&rest _)
                  (let ((current (appkit-scroll-visible-property-values property)))
                    (unless (equal current previous)
                      (setq previous current)
                      (funcall function surface))))))
    (appkit-scroll-observer-install
     surface :start-function check :end-function check :visibility-function check)))

(provide 'appkit-scroll)

;;; appkit-scroll.el ends here
