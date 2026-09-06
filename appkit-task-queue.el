;;; appkit-task-queue.el --- Owner-scoped bounded task scheduling  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; A lifecycle-owned FIFO for asynchronous work.  Keys are unique across
;; queued and active tasks, concurrency is bounded, and completion callbacks
;; are protected by per-run tokens so stale or duplicate callbacks are inert.

;;; Code:

(require 'cl-lib)
(require 'appkit-core)

(cl-defstruct (appkit-task-queue--task
               (:constructor appkit-task-queue--task-create)
               (:copier nil))
  key
  start
  finish
  sequence
  state
  token
  cancellation)

(cl-defstruct (appkit-task-queue
               (:constructor appkit-task-queue--make)
               (:copier nil)
               (:conc-name appkit-task-queue--))
  owner
  limit
  tasks
  waiting
  waiting-tail
  active-count
  queued-count
  next-sequence
  lifecycle-handle
  alive-p
  pumping-p)

(defun appkit-task-queue--check (queue)
  "Return QUEUE, or signal a type error."
  (unless (appkit-task-queue-p queue)
    (signal 'wrong-type-argument (list 'appkit-task-queue-p queue)))
  queue)

(defun appkit-task-queue-live-p (queue)
  "Return non-nil when QUEUE and its lifecycle owner are live."
  (and (appkit-task-queue-p queue)
       (appkit-task-queue--alive-p queue)
       (appkit-owner-live-p
        (appkit-task-queue--owner queue))))

(defun appkit-task-queue-owner (queue)
  "Return the lifecycle owner of QUEUE."
  (appkit-task-queue--owner (appkit-task-queue--check queue)))

(defun appkit-task-queue-limit (queue)
  "Return the maximum concurrency of QUEUE."
  (appkit-task-queue--limit (appkit-task-queue--check queue)))

(defun appkit-task-queue-active-count (queue)
  "Return the number of active tasks in QUEUE."
  (appkit-task-queue--active-count
   (appkit-task-queue--check queue)))

(defun appkit-task-queue-queued-count (queue)
  "Return the number of waiting tasks in QUEUE."
  (appkit-task-queue--queued-count
   (appkit-task-queue--check queue)))

(defun appkit-task-queue-total-count (queue)
  "Return the total number of active and waiting tasks in QUEUE."
  (+ (appkit-task-queue-active-count queue)
     (appkit-task-queue-queued-count queue)))

(cl-defun appkit-task-queue-pending-p
    (queue &optional (key nil key-supplied-p))
  "Return non-nil when QUEUE has pending work.

When KEY is supplied, return non-nil only when an equal key is active or
waiting."
  (appkit-task-queue--check queue)
  (if key-supplied-p
      (let ((missing (make-symbol "missing")))
        (not (eq (gethash key (appkit-task-queue--tasks queue) missing)
                 missing)))
    (> (appkit-task-queue-total-count queue) 0)))

(defun appkit-task-queue-pending-keys (queue)
  "Return QUEUE's pending keys in original submission order."
  (appkit-task-queue--check queue)
  (let (tasks)
    (maphash (lambda (_key task) (push task tasks))
             (appkit-task-queue--tasks queue))
    (mapcar #'appkit-task-queue--task-key
            (sort tasks
                  (lambda (left right)
                    (< (appkit-task-queue--task-sequence left)
                       (appkit-task-queue--task-sequence right)))))))

(defun appkit-task-queue--enqueue (queue task)
  "Append TASK to QUEUE's waiting list."
  (let ((cell (list task)))
    (if (appkit-task-queue--waiting-tail queue)
        (setcdr (appkit-task-queue--waiting-tail queue) cell)
      (setf (appkit-task-queue--waiting queue) cell))
    (setf (appkit-task-queue--waiting-tail queue) cell
          (appkit-task-queue--queued-count queue)
          (1+ (appkit-task-queue--queued-count queue)))))

(defun appkit-task-queue--dequeue (queue)
  "Remove and return QUEUE's first waiting task."
  (when-let* ((cell (appkit-task-queue--waiting queue)))
    (setf (appkit-task-queue--waiting queue) (cdr cell)
          (appkit-task-queue--queued-count queue)
          (1- (appkit-task-queue--queued-count queue)))
    (unless (appkit-task-queue--waiting queue)
      (setf (appkit-task-queue--waiting-tail queue) nil))
    (car cell)))

(defun appkit-task-queue--remove-waiting (queue task)
  "Remove TASK from QUEUE's waiting list and return non-nil on success."
  (let ((cell (appkit-task-queue--waiting queue))
        previous)
    (while (and cell (not (eq (car cell) task)))
      (setq previous cell
            cell (cdr cell)))
    (when cell
      (if previous
          (setcdr previous (cdr cell))
        (setf (appkit-task-queue--waiting queue) (cdr cell)))
      (when (eq cell (appkit-task-queue--waiting-tail queue))
        (setf (appkit-task-queue--waiting-tail queue) previous))
      (setf (appkit-task-queue--queued-count queue)
            (1- (appkit-task-queue--queued-count queue)))
      t)))

(defun appkit-task-queue--current-active-p (queue task)
  "Return non-nil when TASK is QUEUE's current active task for its key."
  (and (eq (appkit-task-queue--task-state task) 'active)
       (eq (gethash (appkit-task-queue--task-key task)
                    (appkit-task-queue--tasks queue))
           task)))

(defun appkit-task-queue--retire-active (queue task state)
  "Retire active TASK from QUEUE and set its terminal STATE."
  (when (appkit-task-queue--current-active-p queue task)
    (remhash (appkit-task-queue--task-key task)
             (appkit-task-queue--tasks queue))
    (setf (appkit-task-queue--active-count queue)
          (1- (appkit-task-queue--active-count queue))
          (appkit-task-queue--task-state task) state
          (appkit-task-queue--task-token task) nil
          (appkit-task-queue--task-cancellation task) nil)
    t))

(defun appkit-task-queue--valid-cancellation-p (object)
  "Return non-nil when OBJECT is a supported cancellation value."
  (or (null object) (functionp object) (appkit-handle-p object)))

(defun appkit-task-queue--cancel-object (object)
  "Invoke supported cancellation OBJECT exactly once."
  (cond
   ((null object) nil)
   ((appkit-handle-p object) (appkit-cancel-handle object))
   ((functionp object) (funcall object))
   (t (error "Unsupported appkit task cancellation object: %S" object))))

(defun appkit-task-queue--warn-secondary-error (context error-data)
  "Display a warning for secondary ERROR-DATA arising in CONTEXT."
  (display-warning
   'appkit-task-queue
   (format "%s: %s" context (error-message-string error-data))
   :warning))

(defun appkit-task-queue--resignal (error-data)
  "Re-signal ERROR-DATA captured by `condition-case'."
  (signal (car error-data) (cdr error-data)))

(defun appkit-task-queue--pump (queue)
  "Start as much waiting work in QUEUE as its limit permits."
  (when (appkit-task-queue-live-p queue)
    (if (appkit-task-queue--pumping-p queue)
        nil
      (let (first-error normal-exit-p)
        (setf (appkit-task-queue--pumping-p queue) t)
        (unwind-protect
            (progn
              (while (and (appkit-task-queue-live-p queue)
                          (< (appkit-task-queue--active-count queue)
                             (appkit-task-queue--limit queue))
                          (> (appkit-task-queue--queued-count queue) 0))
                (let* ((task (appkit-task-queue--dequeue queue))
                       (token (make-symbol "appkit-task-token"))
                       (starting-p t)
                       completion-pending-p
                       pending-arguments
                       start-returned-p
                       error-data)
                  ;; Register TASK as active before START can call COMPLETION.
                  (setf (appkit-task-queue--task-state task) 'active
                        (appkit-task-queue--task-token task) token
                        (appkit-task-queue--active-count queue)
                        (1+ (appkit-task-queue--active-count queue)))
                  (condition-case err
                      (unwind-protect
                          (let ((cancellation
                                 (funcall
                                  (appkit-task-queue--task-start task)
                                  (lambda (&rest arguments)
                                    (when (and
                                           (appkit-task-queue-live-p queue)
                                           (appkit-task-queue--current-active-p
                                            queue task)
                                           (eq token
                                               (appkit-task-queue--task-token
                                                task)))
                                      (if starting-p
                                          (unless completion-pending-p
                                            (setq completion-pending-p t
                                                  pending-arguments arguments)
                                            t)
                                        (appkit-task-queue--complete
                                         queue task token arguments)))))))
                            (setq starting-p nil)
                            (unless (appkit-task-queue--valid-cancellation-p
                                     cancellation)
                              (error
                               "Task starter returned unsupported cancellation: %S"
                               cancellation))
                            (cond
                             ((and (appkit-task-queue-live-p queue)
                                   (appkit-task-queue--current-active-p
                                    queue task)
                                   (eq token
                                       (appkit-task-queue--task-token task)))
                              (setf (appkit-task-queue--task-cancellation task)
                                    cancellation))
                             ;; Cancellation may have happened reentrantly
                             ;; inside START, before it returned its handle.
                             ((eq (appkit-task-queue--task-state task)
                                  'cancelled)
                              (appkit-task-queue--cancel-object cancellation))
                             ;; Keep compatibility with tasks completed by
                             ;; unusual reentrant code outside this callback.
                             ((eq (appkit-task-queue--task-state task)
                                  'finished)
                              (appkit-retire-handle cancellation)))
                            ;; Mark START returned before delivering a deferred
                            ;; synchronous completion.  Its finisher may signal,
                            ;; but the returned cancellation is now known and
                            ;; can be retired by `--complete'.
                            (setq start-returned-p t)
                            (when completion-pending-p
                              (appkit-task-queue--complete
                               queue task token pending-arguments)))
                        (unless start-returned-p
                          (setq starting-p nil)
                          (when (and
                                 (appkit-task-queue--current-active-p queue task)
                                 (eq token
                                     (appkit-task-queue--task-token task)))
                            (appkit-task-queue--retire-active
                             queue task 'start-failed))))
                    (error (setq error-data err))
                    (quit (setq error-data err)))
                  (when error-data
                    (if first-error
                        (appkit-task-queue--warn-secondary-error
                         "Additional task starter failed" error-data)
                      (setq first-error error-data)))))
              (setq normal-exit-p t))
          (setf (appkit-task-queue--pumping-p queue) nil)
          ;; An arbitrary nonlocal exit is not caught by `condition-case'.
          ;; Its task was retired by the inner unwind; fill the released slot
          ;; before allowing that exit to continue unwinding.
          (unless normal-exit-p
            (condition-case err
                (appkit-task-queue--pump queue)
              (error
               (appkit-task-queue--warn-secondary-error
                "Task queue pump failed during nonlocal cleanup" err))
              (quit
               (appkit-task-queue--warn-secondary-error
                "Task queue pump quit during nonlocal cleanup" err)))))
        (when first-error
          (appkit-task-queue--resignal first-error))))))

(defun appkit-task-queue--complete (queue task token arguments)
  "Complete TASK in QUEUE for TOKEN, passing ARGUMENTS to its finisher."
  (when (and (appkit-task-queue-live-p queue)
             (appkit-task-queue--current-active-p queue task)
             (eq token (appkit-task-queue--task-token task)))
    (let ((finish (appkit-task-queue--task-finish task))
          (cancellation (appkit-task-queue--task-cancellation task))
          finish-error
          pump-error
          pump-attempted-p)
      ;; Retire before FINISH so duplicate and reentrant callbacks are inert.
      (appkit-task-queue--retire-active queue task 'finished)
      (appkit-retire-handle cancellation)
      (unwind-protect
          (progn
            (condition-case err
                (when finish (apply finish arguments))
              (error (setq finish-error err))
              (quit (setq finish-error err)))
            (setq pump-attempted-p t)
            (condition-case err
                (appkit-task-queue--pump queue)
              (error (setq pump-error err))
              (quit (setq pump-error err)))
            (when (and finish-error pump-error)
              (appkit-task-queue--warn-secondary-error
               "Task queue pump failed after finisher failure" pump-error))
            (cond
             (finish-error (appkit-task-queue--resignal finish-error))
             (pump-error (appkit-task-queue--resignal pump-error))))
        ;; A throw or another non-signal exit from FINISH must not strand the
        ;; next waiting task.  Errors and quits use the normal path above.
        (unless pump-attempted-p
          (condition-case err
              (appkit-task-queue--pump queue)
            (error
             (appkit-task-queue--warn-secondary-error
              "Task queue pump failed during finisher cleanup" err))
            (quit
             (appkit-task-queue--warn-secondary-error
              "Task queue pump quit during finisher cleanup" err)))))
      t)))

(defun appkit-task-queue-create (owner limit)
  "Create a bounded FIFO owned by live app or view OWNER.

LIMIT must be a positive integer.  Stopping OWNER closes the returned queue,
cancels its active tasks, drops its waiting tasks, and makes stale completion
callbacks inert."
  (unless (appkit-owner-live-p owner)
    (error "Cannot create a task queue for a dead appkit owner"))
  (unless (and (integerp limit) (> limit 0))
    (error "Appkit task queue limit must be positive: %S" limit))
  (let ((queue
         (appkit-task-queue--make
          :owner owner
          :limit limit
          :tasks (make-hash-table :test #'equal)
          :waiting nil
          :waiting-tail nil
          :active-count 0
          :queued-count 0
          :next-sequence 0
          :lifecycle-handle nil
          :alive-p t
          :pumping-p nil)))
    (setf (appkit-task-queue--lifecycle-handle queue)
          (appkit-register-handle
           owner 'task-queue queue #'appkit-task-queue-cancel))
    queue))

(cl-defun appkit-task-queue-submit (queue key start &key finish)
  "Submit keyed asynchronous work to QUEUE.

KEY is compared with `equal' and is accepted only when no equal key is active
or waiting.  START receives one completion callback and may return either an
`appkit-handle' or a zero-argument cancellation function.  Processes and other
objects must be wrapped in such a function.  FINISH, when non-nil, receives
exactly the arguments passed to the current completion callback.

On normal completion, a returned `appkit-handle' is retired from its owner
without invoking its cancellation callback.  A returned function is simply
discarded.

Return non-nil when the task was accepted, and nil for a duplicate KEY.  A
synchronous START or FINISH error is re-signaled after its slot is released
and the queue has had an opportunity to advance."
  (appkit-task-queue--check queue)
  (unless (appkit-task-queue-live-p queue)
    (error "Cannot submit work to a dead appkit task queue"))
  (unless (functionp start)
    (signal 'wrong-type-argument (list 'functionp start)))
  (unless (or (null finish) (functionp finish))
    (signal 'wrong-type-argument (list 'functionp finish)))
  (let ((missing (make-symbol "missing")))
    (if (not (eq (gethash key (appkit-task-queue--tasks queue) missing)
                 missing))
        nil
      (let* ((sequence (appkit-task-queue--next-sequence queue))
             (task (appkit-task-queue--task-create
                    :key key
                    :start start
                    :finish finish
                    :sequence sequence
                    :state 'waiting
                    :token nil
                    :cancellation nil)))
        (setf (appkit-task-queue--next-sequence queue) (1+ sequence))
        (puthash key task (appkit-task-queue--tasks queue))
        (appkit-task-queue--enqueue queue task)
        (appkit-task-queue--pump queue)
        t))))

(defun appkit-task-queue-set-limit (queue limit)
  "Set live QUEUE's concurrency LIMIT and return LIMIT.

LIMIT must be positive.  Raising the limit starts newly eligible work
immediately.  Lowering it leaves active work alone and delays further starts
until the active count falls below LIMIT."
  (appkit-task-queue--check queue)
  (unless (appkit-task-queue-live-p queue)
    (error "Cannot resize a dead appkit task queue"))
  (unless (and (integerp limit) (> limit 0))
    (error "Appkit task queue limit must be positive: %S" limit))
  (let ((old-limit (appkit-task-queue--limit queue)))
    (setf (appkit-task-queue--limit queue) limit)
    (when (> limit old-limit)
      (appkit-task-queue--pump queue)))
  limit)

(defun appkit-task-queue-cancel-keys (queue keys)
  "Atomically cancel pending tasks in QUEUE identified by KEYS.

KEYS is a list whose elements use the queue's `equal' key comparison.  All
matching waiting and active tasks are retired before active cancellations run,
then the queue is pumped once.  Duplicate keys count only once.  Return the
number of tasks retired."
  (appkit-task-queue--check queue)
  (let ((missing (make-symbol "missing"))
        (owns-pump-guard (not (appkit-task-queue--pumping-p queue)))
        cancellations
        (count 0)
        first-error
        pump-error
        cancellation-phase-complete-p)
    ;; Suppress pumps triggered reentrantly by cancellation code until every
    ;; requested key has been removed.  An enclosing pump already provides
    ;; the same guard when this function is called from START or FINISH.
    (when owns-pump-guard
      (setf (appkit-task-queue--pumping-p queue) t))
    (unwind-protect
        (progn
          ;; Phase one is mutation-only: no cancellation callback can start
          ;; work that another key in this same batch is about to discard.
          (dolist (key keys)
            (let ((task
                   (gethash key (appkit-task-queue--tasks queue) missing)))
              (unless (eq task missing)
                (pcase (appkit-task-queue--task-state task)
                  ('waiting
                   (appkit-task-queue--remove-waiting queue task)
                   (remhash (appkit-task-queue--task-key task)
                            (appkit-task-queue--tasks queue))
                   (setf (appkit-task-queue--task-state task) 'cancelled)
                   (setq count (1+ count)))
                  ('active
                   (push (appkit-task-queue--task-cancellation task)
                         cancellations)
                   (appkit-task-queue--retire-active
                    queue task 'cancelled)
                   (setq count (1+ count)))))))
          ;; Phase two performs every active cancellation even when one fails
          ;; or transfers control nonlocally.
          (setq cancellations (nreverse cancellations))
          (appkit--run-cleanup-items
           cancellations #'appkit-task-queue--cancel-object
           (lambda (err)
             (if first-error
                 (appkit-task-queue--warn-secondary-error
                  "Additional task cancellation failed" err)
               (setq first-error err))))
          (setq cancellation-phase-complete-p t))
      (when owns-pump-guard
        (setf (appkit-task-queue--pumping-p queue) nil))
      ;; If a cancellation used an arbitrary throw, `--run-cleanup-items'
      ;; already finished its siblings.  Pump during unwinding because the
      ;; normal phase-three form below will not be reached.
      (unless cancellation-phase-complete-p
        (condition-case err
            (appkit-task-queue--pump queue)
          (error
           (appkit-task-queue--warn-secondary-error
            "Task queue pump failed during cancellation cleanup" err))
          (quit
           (appkit-task-queue--warn-secondary-error
            "Task queue pump quit during cancellation cleanup" err)))))
    ;; Phase three fills only the slots left after the entire batch mutation.
    (condition-case err
        (appkit-task-queue--pump queue)
      (error (setq pump-error err))
      (quit (setq pump-error err)))
    (when pump-error
      (if first-error
          (appkit-task-queue--warn-secondary-error
           "Task queue pump failed after cancellation failure" pump-error)
        (setq first-error pump-error)))
    (when first-error
      (appkit-task-queue--resignal first-error))
    count))

(defun appkit-task-queue-cancel-key (queue key)
  "Cancel the active or waiting task in QUEUE identified by KEY.

Return non-nil when an equal pending key existed.  Active task cancellation
supports `appkit-handle' values and zero-argument functions."
  (> (appkit-task-queue-cancel-keys queue (list key)) 0))

(defun appkit-task-queue-cancel (queue)
  "Close QUEUE, cancel active work, and discard waiting work.

Cancellation is idempotent.  Completion callbacks from discarded task tokens
become inert.  Return non-nil only when this call closed the queue."
  (appkit-task-queue--check queue)
  (when (appkit-task-queue--alive-p queue)
    (let (cancellations first-error lifecycle-handle)
      ;; Invalidate every token before invoking cancellation code, which may
      ;; synchronously call a completion callback.
      (setf (appkit-task-queue--alive-p queue) nil)
      (maphash
       (lambda (_key task)
         (when (eq (appkit-task-queue--task-state task) 'active)
           (push (appkit-task-queue--task-cancellation task) cancellations))
         (setf (appkit-task-queue--task-state task) 'cancelled
               (appkit-task-queue--task-token task) nil
               (appkit-task-queue--task-cancellation task) nil))
       (appkit-task-queue--tasks queue))
      (clrhash (appkit-task-queue--tasks queue))
      (setf (appkit-task-queue--waiting queue) nil
            (appkit-task-queue--waiting-tail queue) nil
            (appkit-task-queue--active-count queue) 0
            (appkit-task-queue--queued-count queue) 0)
      (setq lifecycle-handle
            (appkit-task-queue--lifecycle-handle queue))
      (setf (appkit-task-queue--lifecycle-handle queue) nil)
      (when (appkit-handle-p lifecycle-handle)
        (setq cancellations
              (append cancellations (list lifecycle-handle))))
      (appkit--run-cleanup-items
       cancellations #'appkit-task-queue--cancel-object
       (lambda (err)
         (if first-error
             (appkit-task-queue--warn-secondary-error
              "Additional task queue shutdown failure" err)
           (setq first-error err))))
      (when first-error
        (appkit-task-queue--resignal first-error))
      t)))

(provide 'appkit-task-queue)

;;; appkit-task-queue.el ends here
