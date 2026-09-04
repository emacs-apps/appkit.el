;;; appkit-task-queue-test.el --- Tests for bounded task queues -*- lexical-binding: t; -*-

(require 'ert)

(require 'appkit-task-queue)
(require 'appkit-test-helper)

(cl-defstruct (appkit-task-queue-test--harness
               (:constructor appkit-task-queue-test--harness-create)
               (:copier nil))
  starts
  finishes
  callbacks
  max-active)

(defun appkit-task-queue-test--make-harness ()
  "Create an empty controlled-task test harness."
  (appkit-task-queue-test--harness-create
   :starts nil
   :finishes nil
   :callbacks (make-hash-table :test #'equal)
   :max-active 0))

(cl-defun appkit-task-queue-test--submit
    (queue harness key &key cancellation finish)
  "Submit controlled KEY to QUEUE and record events in HARNESS."
  (let ((task-key key))
    (appkit-task-queue-submit
     queue task-key
     (lambda (complete)
       (setf (appkit-task-queue-test--harness-starts harness)
             (append (appkit-task-queue-test--harness-starts harness)
                     (list task-key))
             (appkit-task-queue-test--harness-max-active harness)
             (max (appkit-task-queue-test--harness-max-active harness)
                  (appkit-task-queue-active-count queue)))
       (puthash task-key complete
                (appkit-task-queue-test--harness-callbacks harness))
       cancellation)
     :finish
     (or finish
         (lambda (&rest arguments)
           (setf (appkit-task-queue-test--harness-finishes harness)
                 (append
                  (appkit-task-queue-test--harness-finishes harness)
                  (list (cons task-key arguments)))))))))

(defun appkit-task-queue-test--complete (harness key &rest arguments)
  "Complete the controlled task for KEY in HARNESS with ARGUMENTS."
  (apply (gethash key
                  (appkit-task-queue-test--harness-callbacks harness))
         arguments))

(ert-deftest appkit-task-queue-preserves-fifo-and-limit ()
  (appkit-test-with-surface
    (let* ((queue (appkit-task-queue-create (appkit-current-surface) 2))
           (harness (appkit-task-queue-test--make-harness)))
      (dotimes (key 4)
        (should (appkit-task-queue-test--submit queue harness key)))
      (should (equal (appkit-task-queue-test--harness-starts harness)
                     '(0 1)))
      (should (= (appkit-task-queue-active-count queue) 2))
      (should (= (appkit-task-queue-queued-count queue) 2))
      (appkit-task-queue-test--complete harness 0 'zero)
      (should (equal (appkit-task-queue-test--harness-starts harness)
                     '(0 1 2)))
      (appkit-task-queue-test--complete harness 1 'one)
      (should (equal (appkit-task-queue-test--harness-starts harness)
                     '(0 1 2 3)))
      (appkit-task-queue-test--complete harness 2 'two)
      (appkit-task-queue-test--complete harness 3 'three)
      (should (equal (appkit-task-queue-test--harness-finishes harness)
                     '((0 zero) (1 one) (2 two) (3 three))))
      (should (= (appkit-task-queue-test--harness-max-active harness) 2))
      (should-not (appkit-task-queue-pending-p queue)))))

(ert-deftest appkit-task-queue-drains-thirty-two-tasks-at-limit-four ()
  (appkit-test-with-surface
    (let* ((queue (appkit-task-queue-create (appkit-current-surface) 4))
           (harness (appkit-task-queue-test--make-harness)))
      (dotimes (key 32)
        (appkit-task-queue-test--submit queue harness key))
      (should (equal (appkit-task-queue-test--harness-starts harness)
                     '(0 1 2 3)))
      (dotimes (key 32)
        (appkit-task-queue-test--complete harness key key))
      (should (equal (appkit-task-queue-test--harness-starts harness)
                     (number-sequence 0 31)))
      (should (equal
               (appkit-task-queue-test--harness-finishes harness)
               (mapcar (lambda (key) (list key key))
                       (number-sequence 0 31))))
      (should (= (appkit-task-queue-test--harness-max-active harness) 4))
      (should (= (appkit-task-queue-total-count queue) 0)))))

(ert-deftest appkit-task-queue-deduplicates-active-and-waiting-keys ()
  (appkit-test-with-surface
    (let* ((queue (appkit-task-queue-create (appkit-current-surface) 1))
           (harness (appkit-task-queue-test--make-harness)))
      (should (appkit-task-queue-test--submit queue harness '(active 1)))
      (should (appkit-task-queue-test--submit queue harness '(waiting 1)))
      (should-not
       (appkit-task-queue-test--submit queue harness (list 'active 1)))
      (should-not
       (appkit-task-queue-test--submit queue harness (list 'waiting 1)))
      (should (= (appkit-task-queue-total-count queue) 2))
      (should (appkit-task-queue-pending-p queue '(active 1)))
      (should (appkit-task-queue-pending-p queue '(waiting 1))))))

(ert-deftest appkit-task-queue-supports-synchronous-completion ()
  (appkit-test-with-surface
    (let ((queue (appkit-task-queue-create (appkit-current-surface) 1))
          (finishes 0)
          result
          (late-cancellations 0))
      (should
       (appkit-task-queue-submit
        queue 'sync
        (lambda (complete)
          (funcall complete 'ok 42)
          (funcall complete 'duplicate)
          (lambda () (setq late-cancellations (1+ late-cancellations))))
        :finish
        (lambda (&rest arguments)
          (setq finishes (1+ finishes)
                result arguments))))
      (should (= finishes 1))
      (should (equal result '(ok 42)))
      (should (= late-cancellations 0))
      (should-not (appkit-task-queue-pending-p queue))
      (appkit-task-queue-cancel queue)
      (should (= late-cancellations 0)))))

(ert-deftest appkit-task-queue-ignores-duplicate-and-stale-completions ()
  (appkit-test-with-surface
    (let* ((queue (appkit-task-queue-create (appkit-current-surface) 1))
           (harness (appkit-task-queue-test--make-harness))
           old-callback new-callback)
      (appkit-task-queue-test--submit queue harness 'same)
      (setq old-callback
            (gethash 'same
                     (appkit-task-queue-test--harness-callbacks harness)))
      (funcall old-callback 'first)
      (funcall old-callback 'duplicate)
      (appkit-task-queue-test--submit queue harness 'same)
      (setq new-callback
            (gethash 'same
                     (appkit-task-queue-test--harness-callbacks harness)))
      (funcall old-callback 'stale)
      (should (= (appkit-task-queue-active-count queue) 1))
      (funcall new-callback 'second)
      (funcall new-callback 'duplicate)
      (should (equal (appkit-task-queue-test--harness-finishes harness)
                     '((same first) (same second)))))))

(ert-deftest appkit-task-queue-finish-error-releases-and-pumps ()
  (appkit-test-with-surface
    (let* ((queue (appkit-task-queue-create (appkit-current-surface) 1))
           (harness (appkit-task-queue-test--make-harness))
           (finish-runs 0)
           first-callback)
      (appkit-task-queue-test--submit
       queue harness 'first
       :finish (lambda (&rest _arguments)
                 (setq finish-runs (1+ finish-runs))
                 (error "finisher failed")))
      (setq first-callback
            (gethash 'first
                     (appkit-task-queue-test--harness-callbacks harness)))
      (appkit-task-queue-test--submit queue harness 'second)
      (should-error (funcall first-callback 'done) :type 'error)
      (should (= finish-runs 1))
      (should (equal (appkit-task-queue-test--harness-starts harness)
                     '(first second)))
      (should (= (appkit-task-queue-active-count queue) 1))
      (should-not (funcall first-callback 'stale))
      (should (= finish-runs 1))
      (appkit-task-queue-test--complete harness 'second))))

(ert-deftest appkit-task-queue-finish-quit-releases-and-pumps ()
  (appkit-test-with-surface
    (let* ((queue (appkit-task-queue-create (appkit-current-surface) 1))
           (harness (appkit-task-queue-test--make-harness))
           first-callback)
      (appkit-task-queue-test--submit
       queue harness 'first
       :finish (lambda (&rest _arguments) (signal 'quit nil)))
      (setq first-callback
            (gethash 'first
                     (appkit-task-queue-test--harness-callbacks harness)))
      (appkit-task-queue-test--submit queue harness 'second)
      (should
       (eq 'quit
           (condition-case nil
               (progn (funcall first-callback 'done) nil)
             (quit 'quit))))
      (should (equal (appkit-task-queue-test--harness-starts harness)
                     '(first second)))
      (should (= (appkit-task-queue-active-count queue) 1))
      (appkit-task-queue-test--complete harness 'second)
      (should-not (appkit-task-queue-pending-p queue)))))

(ert-deftest appkit-task-queue-warns-when-pump-also-fails-after-finish ()
  (appkit-test-with-surface
    (let ((queue (appkit-task-queue-create (appkit-current-surface) 1))
          callback
          primary-error
          warnings)
      (appkit-task-queue-submit
       queue 'finisher
       (lambda (complete) (setq callback complete))
       :finish (lambda (&rest _arguments) (error "primary finisher failed")))
      (appkit-task-queue-submit
       queue 'starter
       (lambda (_complete) (error "secondary starter failed")))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (type message &optional level _buffer-name)
                   (push (list type message level) warnings))))
        (setq primary-error
              (should-error (funcall callback) :type 'error)))
      (should (string-match-p "primary finisher failed"
                              (error-message-string primary-error)))
      (should (= (length warnings) 1))
      (should (eq (caar warnings) 'appkit-task-queue))
      (should (string-match-p "secondary starter failed"
                              (cadar warnings)))
      (should-not (appkit-task-queue-pending-p queue)))))

(ert-deftest appkit-task-queue-starter-error-releases-and-pumps ()
  (appkit-test-with-surface
    (let* ((queue (appkit-task-queue-create (appkit-current-surface) 1))
           (harness (appkit-task-queue-test--make-harness))
           (bad-starts 0))
      (appkit-task-queue-test--submit queue harness 'blocker)
      (appkit-task-queue-submit
       queue 'bad
       (lambda (_complete)
         (setq bad-starts (1+ bad-starts))
         (error "starter failed")))
      (appkit-task-queue-test--submit queue harness 'after)
      (should-error
       (appkit-task-queue-test--complete harness 'blocker)
       :type 'error)
      (should (= bad-starts 1))
      (should-not (appkit-task-queue-pending-p queue 'bad))
      (should (equal (appkit-task-queue-test--harness-starts harness)
                     '(blocker after)))
      (should (= (appkit-task-queue-active-count queue) 1))
      (appkit-task-queue-test--complete harness 'after)
      (should-not (appkit-task-queue-pending-p queue)))))

(ert-deftest appkit-task-queue-starter-quit-releases-slot ()
  (appkit-test-with-surface
    (let* ((queue (appkit-task-queue-create (appkit-current-surface) 1))
           (harness (appkit-task-queue-test--make-harness)))
      (should
       (eq 'quit
           (condition-case nil
               (progn
                 (appkit-task-queue-submit
                  queue 'quit (lambda (_complete) (signal 'quit nil)))
                 nil)
             (quit 'quit))))
      (should-not (appkit-task-queue-pending-p queue 'quit))
      (should (= (appkit-task-queue-active-count queue) 0))
      (appkit-task-queue-test--submit queue harness 'next)
      (should (equal (appkit-task-queue-test--harness-starts harness)
                     '(next)))
      (should (= (appkit-task-queue-active-count queue) 1))
      (appkit-task-queue-test--complete harness 'next)
      (should-not (appkit-task-queue-pending-p queue)))))

(ert-deftest appkit-task-queue-cancels-waiting-and-active-tasks ()
  (appkit-test-with-surface
    (let* ((queue (appkit-task-queue-create (appkit-current-surface) 1))
           (harness (appkit-task-queue-test--make-harness))
           (active-cancellations 0)
           active-callback)
      (appkit-task-queue-test--submit
       queue harness 'active
       :cancellation
       (lambda ()
         (setq active-cancellations (1+ active-cancellations))))
      (setq active-callback
            (gethash 'active
                     (appkit-task-queue-test--harness-callbacks harness)))
      (appkit-task-queue-test--submit queue harness 'discarded)
      (should (appkit-task-queue-cancel-key queue 'discarded))
      (should-not (appkit-task-queue-cancel-key queue 'missing))
      (appkit-task-queue-test--submit queue harness 'next)
      (should (appkit-task-queue-cancel-key queue 'active))
      (should (= active-cancellations 1))
      (should (equal (appkit-task-queue-test--harness-starts harness)
                     '(active next)))
      (should-not (funcall active-callback 'stale))
      (should-not
       (assoc 'active
              (appkit-task-queue-test--harness-finishes harness)))
      (appkit-task-queue-test--complete harness 'next))))

(ert-deftest appkit-task-queue-cancel-keys-mutates-before-cancel-and-pumps-once ()
  (appkit-test-with-surface
    (let* ((queue (appkit-task-queue-create (appkit-current-surface) 2))
           (harness (appkit-task-queue-test--make-harness))
           (targets '(drop-waiting-1 drop-active
                      drop-waiting-2 drop-active))
           (cancellations 0)
           pending-at-cancel
           drop-callback
           (real-pump (symbol-function 'appkit-task-queue--pump))
           (pump-calls 0))
      (appkit-task-queue-test--submit
       queue harness 'drop-active
       :cancellation
       (lambda ()
         (setq cancellations (1+ cancellations)
               pending-at-cancel
               (mapcar (lambda (key)
                         (appkit-task-queue-pending-p queue key))
                       targets))))
      (setq drop-callback
            (gethash 'drop-active
                     (appkit-task-queue-test--harness-callbacks harness)))
      (appkit-task-queue-test--submit queue harness 'keep-active)
      (appkit-task-queue-test--submit queue harness 'drop-waiting-1)
      (appkit-task-queue-test--submit queue harness 'keep-waiting)
      (appkit-task-queue-test--submit queue harness 'drop-waiting-2)
      (cl-letf (((symbol-function 'appkit-task-queue--pump)
                 (lambda (target-queue)
                   (setq pump-calls (1+ pump-calls))
                   (funcall real-pump target-queue))))
        (should (= (appkit-task-queue-cancel-keys queue targets) 3)))
      (should (= pump-calls 1))
      (should (= cancellations 1))
      (should (equal pending-at-cancel '(nil nil nil nil)))
      (should (equal (appkit-task-queue-test--harness-starts harness)
                     '(drop-active keep-active keep-waiting)))
      (should-not (appkit-task-queue-pending-p queue 'drop-active))
      (should-not (appkit-task-queue-pending-p queue 'drop-waiting-1))
      (should-not (appkit-task-queue-pending-p queue 'drop-waiting-2))
      (should-not (funcall drop-callback 'stale))
      (should (= (appkit-task-queue-active-count queue) 2))
      (should (= (appkit-task-queue-queued-count queue) 0)))))

(ert-deftest appkit-task-queue-cancel-keys-finishes-after-cancellation-quit ()
  (appkit-test-with-surface
    (let* ((queue (appkit-task-queue-create (appkit-current-surface) 2))
           (harness (appkit-task-queue-test--make-harness))
           (first-cancellations 0)
           (second-cancellations 0))
      (appkit-task-queue-test--submit
       queue harness 'first
       :cancellation
       (lambda ()
         (cl-incf first-cancellations)
         (signal 'quit nil)))
      (appkit-task-queue-test--submit
       queue harness 'second
       :cancellation (lambda () (cl-incf second-cancellations)))
      (appkit-task-queue-test--submit queue harness 'next)
      (should
       (eq 'quit
           (condition-case nil
               (progn
                 (appkit-task-queue-cancel-keys queue '(first second))
                 nil)
             (quit 'quit))))
      (should (= first-cancellations 1))
      (should (= second-cancellations 1))
      (should (equal (appkit-task-queue-test--harness-starts harness)
                     '(first second next)))
      (should (= (appkit-task-queue-active-count queue) 1))
      (appkit-task-queue-test--complete harness 'next)
      (should-not (appkit-task-queue-pending-p queue)))))

(ert-deftest appkit-task-queue-cancel-keys-finishes-after-cancellation-throw ()
  (appkit-test-with-surface
    (let* ((queue (appkit-task-queue-create (appkit-current-surface) 2))
           (harness (appkit-task-queue-test--make-harness))
           (throwing-cancellations 0)
           (sibling-cancellations 0))
      (appkit-task-queue-test--submit
       queue harness 'throws
       :cancellation
       (lambda ()
         (cl-incf throwing-cancellations)
         (throw 'escape 'escaped)))
      (appkit-task-queue-test--submit
       queue harness 'sibling
       :cancellation (lambda () (cl-incf sibling-cancellations)))
      (appkit-task-queue-test--submit queue harness 'next)
      (should
       (eq 'escaped
           (catch 'escape
             (appkit-task-queue-cancel-keys queue '(throws sibling))
             'returned)))
      (should (= throwing-cancellations 1))
      (should (= sibling-cancellations 1))
      (should (equal (appkit-task-queue-test--harness-starts harness)
                     '(throws sibling next)))
      (should (= (appkit-task-queue-active-count queue) 1))
      (appkit-task-queue-test--complete harness 'next)
      (should-not (appkit-task-queue-pending-p queue)))))

(ert-deftest appkit-task-queue-cancel-closes-all-work-exactly-once ()
  (appkit-test-with-surface
    (let* ((queue (appkit-task-queue-create (appkit-current-surface) 2))
           (harness (appkit-task-queue-test--make-harness))
           (cancellations 0)
           callbacks)
      (dotimes (key 3)
        (appkit-task-queue-test--submit
         queue harness key
         :cancellation
         (lambda () (setq cancellations (1+ cancellations)))))
      (setq callbacks
            (mapcar
             (lambda (key)
               (gethash key
                        (appkit-task-queue-test--harness-callbacks harness)))
             '(0 1)))
      (should (appkit-task-queue-cancel queue))
      (should-not (appkit-task-queue-cancel queue))
      (should-not (appkit-task-queue-live-p queue))
      (should (= cancellations 2))
      (should (= (appkit-task-queue-total-count queue) 0))
      (dolist (callback callbacks)
        (should-not (funcall callback 'stale)))
      (should-not (appkit-task-queue-test--harness-finishes harness)))))

(ert-deftest appkit-task-queue-cancel-retires-everything-after-quit ()
  (appkit-test-with-surface
    (let* ((view (appkit-current-surface))
           (queue (appkit-task-queue-create view 2))
           (harness (appkit-task-queue-test--make-harness))
           (quit-cancellations 0)
           (other-cancellations 0)
           (lifecycle-handle
            (appkit-task-queue--lifecycle-handle queue)))
      (appkit-task-queue-test--submit
       queue harness 'quits
       :cancellation
       (lambda ()
         (cl-incf quit-cancellations)
         (signal 'quit nil)))
      (appkit-task-queue-test--submit
       queue harness 'other
       :cancellation (lambda () (cl-incf other-cancellations)))
      (should
       (eq 'quit
           (condition-case nil
               (progn (appkit-task-queue-cancel queue) nil)
             (quit 'quit))))
      (should (= quit-cancellations 1))
      (should (= other-cancellations 1))
      (should-not (appkit-task-queue-live-p queue))
      (should-not (appkit-task-queue-pending-p queue))
      (should-not (appkit-handle-alive-p lifecycle-handle)))))

(ert-deftest appkit-task-queue-cancel-retires-everything-after-throw ()
  (appkit-test-with-surface
    (let* ((view (appkit-current-surface))
           (queue (appkit-task-queue-create view 2))
           (harness (appkit-task-queue-test--make-harness))
           (throwing-cancellations 0)
           (sibling-cancellations 0)
           (lifecycle-handle
            (appkit-task-queue--lifecycle-handle queue)))
      (appkit-task-queue-test--submit
       queue harness 'throws
       :cancellation
       (lambda ()
         (cl-incf throwing-cancellations)
         (throw 'escape 'escaped)))
      (appkit-task-queue-test--submit
       queue harness 'sibling
       :cancellation (lambda () (cl-incf sibling-cancellations)))
      (should
       (eq 'escaped
           (catch 'escape
             (appkit-task-queue-cancel queue)
             'returned)))
      (should (= throwing-cancellations 1))
      (should (= sibling-cancellations 1))
      (should-not (appkit-task-queue-live-p queue))
      (should-not (appkit-task-queue-pending-p queue))
      (should-not (appkit-handle-alive-p lifecycle-handle)))))

(ert-deftest appkit-task-queue-cancels-handle-returned-by-starter ()
  (appkit-test-with-surface
    (let ((queue (appkit-task-queue-create (appkit-current-surface) 1))
          (cancellations 0))
      (appkit-task-queue-submit
       queue 'handle
       (lambda (_complete)
         (appkit-register-handle
          (appkit-current-surface) 'function
          (lambda () (setq cancellations (1+ cancellations))))))
      (should (appkit-task-queue-cancel-key queue 'handle))
      (should (= cancellations 1)))))

(ert-deftest appkit-task-queue-retires-handle-on-asynchronous-completion ()
  (appkit-test-with-surface
    (let* ((view (appkit-current-surface))
           (queue (appkit-task-queue-create view 1))
           (cancellations 0)
           (finishes 0)
           callback
           handle)
      (appkit-task-queue-submit
       queue 'handle
       (lambda (complete)
         (setq callback complete
               handle
               (appkit-register-handle
                view 'function
                (lambda () (setq cancellations (1+ cancellations)))))
         handle)
       :finish (lambda (&rest _arguments) (setq finishes (1+ finishes))))
      (should (appkit-handle-alive-p handle))
      (funcall callback 'done)
      (should (= finishes 1))
      (should-not (appkit-handle-alive-p handle))
      (should (= cancellations 0))
      (appkit-surface-stop view)
      (should (= cancellations 0)))))

(ert-deftest appkit-task-queue-retires-late-handle-on-sync-completion ()
  (appkit-test-with-surface
    (let* ((view (appkit-current-surface))
           (queue (appkit-task-queue-create view 1))
           (cancellations 0)
           handle)
      (appkit-task-queue-submit
       queue 'handle
       (lambda (complete)
         (setq handle
               (appkit-register-handle
                view 'function
                (lambda () (setq cancellations (1+ cancellations)))))
         (funcall complete 'done)
         handle))
      (should-not (appkit-handle-alive-p handle))
      (should (= cancellations 0))
      (appkit-surface-stop view)
      (should (= cancellations 0)))))

(ert-deftest appkit-task-queue-retires-late-handle-on-sync-finisher-error ()
  (appkit-test-with-surface
    (let* ((view (appkit-current-surface))
           (queue (appkit-task-queue-create view 1))
           (cancellations 0)
           handle)
      (should-error
       (appkit-task-queue-submit
        queue 'handle
        (lambda (complete)
          (setq handle
                (appkit-register-handle
                 view 'function
                 (lambda () (setq cancellations (1+ cancellations)))))
          (funcall complete 'done)
          handle)
        :finish (lambda (&rest _arguments) (error "finisher failed")))
       :type 'error)
      (should (appkit-handle-p handle))
      (should-not (appkit-handle-alive-p handle))
      (should (= cancellations 0))
      (should-not (appkit-task-queue-pending-p queue))
      (appkit-surface-stop view)
      (should (= cancellations 0)))))

(ert-deftest appkit-task-queue-cancels-late-closure-after-reentrant-cancel ()
  (appkit-test-with-surface
    (let ((queue (appkit-task-queue-create (appkit-current-surface) 1))
          (cancellations 0))
      (appkit-task-queue-submit
       queue 'late
       (lambda (_complete)
         (appkit-task-queue-cancel-key queue 'late)
         (lambda () (setq cancellations (1+ cancellations)))))
      (should (= cancellations 1))
      (should-not (appkit-task-queue-pending-p queue)))))

(ert-deftest appkit-task-queue-cancels-late-handle-after-owner-kill ()
  (appkit-test-with-surface
    (let ((queue (appkit-task-queue-create (appkit-current-surface) 1))
          (cancellations 0))
      (appkit-task-queue-submit
       queue 'late
       (lambda (_complete)
         (let ((handle
                (appkit-register-handle
                 (appkit-current-surface) 'function
                 (lambda () (setq cancellations (1+ cancellations))))))
           (appkit-surface-stop (appkit-current-surface))
           handle)))
      (should (= cancellations 1))
      (should-not (appkit-task-queue-live-p queue))
      (should (= (appkit-task-queue-total-count queue) 0)))))

(ert-deftest appkit-task-queue-view-kill-cleans-up-and-stales-callback ()
  (appkit-test-with-surface
    (let* ((view (appkit-current-surface))
           (queue (appkit-task-queue-create view 1))
           (harness (appkit-task-queue-test--make-harness))
           (cancellations 0)
           callback)
      (appkit-task-queue-test--submit
       queue harness 'active
       :cancellation
       (lambda () (setq cancellations (1+ cancellations))))
      (setq callback
            (gethash 'active
                     (appkit-task-queue-test--harness-callbacks harness)))
      (appkit-task-queue-test--submit queue harness 'waiting)
      (appkit-surface-stop view)
      (should-not (appkit-task-queue-live-p queue))
      (should (= cancellations 1))
      (should (= (appkit-task-queue-total-count queue) 0))
      (should-not (funcall callback 'stale))
      (should-not (appkit-task-queue-test--harness-finishes harness)))))

(ert-deftest appkit-task-queue-app-owner-cleans-up-on-stop ()
  (let* ((app (appkit-app-start appkit-test--app-type :identity 'queue-owner))
         (queue (appkit-task-queue-create app 1))
         (cancellations 0)
         finishes
         callback)
    (appkit-task-queue-submit
     queue 'active
     (lambda (complete)
       (setq callback complete)
       (lambda () (setq cancellations (1+ cancellations))))
     :finish (lambda (&rest arguments) (setq finishes arguments)))
    (appkit-app-close app)
    (should-not (appkit-task-queue-live-p queue))
    (should (= cancellations 1))
    (should (= (appkit-task-queue-total-count queue) 0))
    (should-not (funcall callback 'stale))
    (should-not finishes)))

(ert-deftest appkit-task-queue-updates-live-concurrency-limit ()
  (appkit-test-with-surface
    (let* ((queue (appkit-task-queue-create (appkit-current-surface) 1))
           (harness (appkit-task-queue-test--make-harness)))
      (dotimes (key 4)
        (appkit-task-queue-test--submit queue harness key))
      (should (equal (appkit-task-queue-test--harness-starts harness) '(0)))
      (should (= (appkit-task-queue-set-limit queue 3) 3))
      (should (equal (appkit-task-queue-test--harness-starts harness)
                     '(0 1 2)))
      (should (= (appkit-task-queue-set-limit queue 1) 1))
      (should (= (appkit-task-queue-active-count queue) 3))
      (appkit-task-queue-test--complete harness 0)
      (appkit-task-queue-test--complete harness 1)
      (should (equal (appkit-task-queue-test--harness-starts harness)
                     '(0 1 2)))
      (appkit-task-queue-test--complete harness 2)
      (should (equal (appkit-task-queue-test--harness-starts harness)
                     '(0 1 2 3)))
      (appkit-task-queue-test--complete harness 3)
      (should-not (appkit-task-queue-pending-p queue)))))

(ert-deftest appkit-task-queue-enumerates-and-cancels-nil-key ()
  (appkit-test-with-surface
    (let* ((queue (appkit-task-queue-create (appkit-current-surface) 1))
           (harness (appkit-task-queue-test--make-harness)))
      (appkit-task-queue-test--submit queue harness 'first)
      (appkit-task-queue-test--submit queue harness nil)
      (appkit-task-queue-test--submit queue harness 'third)
      (should (equal (appkit-task-queue-pending-keys queue)
                     '(first nil third)))
      (should (appkit-task-queue-pending-p queue nil))
      (should (appkit-task-queue-cancel-key queue nil))
      (should-not (appkit-task-queue-pending-p queue nil))
      (should (equal (appkit-task-queue-pending-keys queue)
                     '(first third))))))

(provide 'appkit-task-queue-test)

;;; appkit-task-queue-test.el ends here
