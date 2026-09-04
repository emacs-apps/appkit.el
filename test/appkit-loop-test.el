;;; appkit-loop-test.el --- Tests for serialized runtime loops -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(require 'appkit-loop)
(require 'appkit-routing)

(defmacro appkit-loop-test--with-loop (binding &rest body)
  "Create the loop described by BINDING and evaluate BODY with cleanup."
  (declare (indent 1) (debug (sexp body)))
  (let ((loop (car binding))
        (arguments (cdr binding)))
    `(let ((,loop (appkit-loop-create ,@arguments)))
       (unwind-protect
           (progn ,@body)
         (appkit-loop-stop ,loop)))))

(ert-deftest appkit-loop-preserves-fifo-and-commits-revisions ()
  (let (seen)
    (appkit-loop-test--with-loop
        (loop
         :model nil
         :update
         (lambda (model message)
           (setq seen (append seen (list message)))
           (appkit-loop-accept (append model (list message)))))
      (should (eq (appkit-loop-post loop 'first) 'enqueued))
      (should (eq (appkit-loop-post loop 'second) 'enqueued))
      (should (eq (appkit-loop-post loop 'third) 'enqueued))
      (should (= (appkit-loop-run-pass loop) 3))
      (should (equal seen '(first second third)))
      (should (equal (appkit-loop-model loop) '(first second third)))
      (should (= (appkit-loop-revision loop) 3))
      (should (= (appkit-loop-pending-count loop) 0)))))

(ert-deftest appkit-loop-freezes-pass-cutoff ()
  (let (loop seen)
    (setq loop
          (appkit-loop-create
           :model nil
           :message-limit 8
           :update
           (lambda (model message)
             (setq seen (append seen (list message)))
             (when (eq message 'first)
               (should (eq (appkit-loop-post loop 'later) 'enqueued)))
             (appkit-loop-accept (cons message model)))))
    (unwind-protect
        (progn
          (appkit-loop-post loop 'first)
          (appkit-loop-post loop 'second)
          (should (= (appkit-loop-run-pass loop) 2))
          (should (equal seen '(first second)))
          (should (= (appkit-loop-pending-count loop) 1))
          (should (= (appkit-loop-run-pass loop) 1))
          (should (equal seen '(first second later))))
      (appkit-loop-stop loop))))

(ert-deftest appkit-loop-enforces-message-limit ()
  (let (seen)
    (appkit-loop-test--with-loop
        (loop
         :model nil
         :message-limit 2
         :update
         (lambda (model message)
           (setq seen (append seen (list message)))
           (appkit-loop-accept model)))
      (dolist (message '(one two three))
        (appkit-loop-post loop message))
      (should (= (appkit-loop-run-pass loop) 2))
      (should (equal seen '(one two)))
      (should (= (appkit-loop-pending-count loop) 1))
      (should (= (appkit-loop-run-pass loop) 1))
      (should (equal seen '(one two three))))))

(ert-deftest appkit-loop-bounds-ordinary-post-admission ()
  (appkit-loop-test--with-loop
      (loop
       :model nil
       :mailbox-capacity 2
       :update (lambda (model _message) (appkit-loop-accept model)))
    (should (eq (appkit-loop-post loop 'one) 'enqueued))
    (should (eq (appkit-loop-post loop 'two) 'enqueued))
    (should (eq (appkit-loop-post loop 'three) 'full))
    (should (= (appkit-loop-pending-count loop) 2))
    (appkit-loop-run-pass loop)
    (should (= (appkit-loop-revision loop) 2))))

(ert-deftest appkit-loop-send-uses-reserve-and-waits-behind-backlog ()
  (let (seen)
    (appkit-loop-test--with-loop
        (loop
         :model nil
         :mailbox-capacity 2
         :send-reserve 1
         :message-limit 1
         :update
         (lambda (model message)
           (setq seen (append seen (list message)))
           (appkit-loop-accept model)))
      (appkit-loop-post loop 'one)
      (appkit-loop-post loop 'two)
      (let ((ticket (appkit-loop-send loop 'barrier)))
        (should (appkit-loop-ticket-p ticket))
        (should (eq (appkit-loop-ticket-state ticket) 'accepted))
        (should (= (appkit-loop-ticket-revision ticket) 3)))
      (should (equal seen '(one two barrier)))
      (should (= (appkit-loop-pending-count loop) 0)))))

(ert-deftest appkit-loop-send-rejects-reentrancy ()
  (let (loop nested-result cross-result direct-condition)
    (let* ((target
            (appkit-loop-create
             :model nil
             :update
             (lambda (model _message) (appkit-loop-accept model))))
           (address (appkit-routing--address target)))
      (setq loop
            (appkit-loop-create
             :model nil
             :update
             (lambda (model message)
               (when (eq message 'outer)
                 (setq nested-result (appkit-loop-send loop 'inner)
                       cross-result (appkit-loop-send target 'inner)
                       direct-condition
                       (condition-case condition
                           (progn
                             (appkit-routing--post address 'inner)
                             nil)
                         (error condition))))
               (appkit-loop-accept model))))
      (unwind-protect
          (let ((ticket (appkit-loop-send loop 'outer)))
            (should (eq nested-result 'reentrant-send))
            (should (eq cross-result 'reentrant-send))
            (should direct-condition)
            (should (eq (appkit-loop-ticket-state ticket) 'accepted))
            (should (= (appkit-loop-revision loop) 1))
            (should (= (appkit-loop-revision target) 0)))
        (appkit-loop-stop loop)
        (appkit-loop-stop target)))))

(ert-deftest appkit-loop-rejection-does-not-commit ()
  (appkit-loop-test--with-loop
      (loop
       :model 'initial
       :update
       (lambda (_model message)
         (if (eq message 'reject)
             (appkit-loop-reject 'not-allowed)
           (appkit-loop-accept 'changed))))
    (let ((ticket (appkit-loop-send loop 'reject)))
      (should (eq (appkit-loop-ticket-state ticket) 'rejected))
      (should (eq (appkit-loop-ticket-outcome ticket) 'not-allowed)))
    (should (eq (appkit-loop-model loop) 'initial))
    (should (= (appkit-loop-revision loop) 0))
    (let ((ticket (appkit-loop-send loop 'accept)))
      (should (eq (appkit-loop-ticket-state ticket) 'accepted)))
    (should (eq (appkit-loop-model loop) 'changed))
    (should (= (appkit-loop-revision loop) 1))))

(ert-deftest appkit-loop-update-error-faults-and-closes-admission ()
  (appkit-loop-test--with-loop
      (loop
       :model 'trusted
       :update (lambda (_model _message) (error "broken transition")))
    (let ((condition
           (should-error (appkit-loop-send loop 'break) :type 'error)))
      (should (string-match-p "broken transition"
                              (error-message-string condition))))
    (should (eq (appkit-loop-status loop) 'faulted))
    (should (eq (appkit-loop-model loop) 'trusted))
    (should (= (appkit-loop-revision loop) 0))
    (should (= (appkit-loop-pending-count loop) 0))
    (should (eq (appkit-loop-post loop 'late) 'faulted))
    (should (eq (appkit-loop-send loop 'late) 'faulted))))

(ert-deftest appkit-loop-arbitrary-nonlocal-exit-still-faults ()
  (appkit-loop-test--with-loop
      (loop
       :model nil
       :update (lambda (_model _message) (throw 'escape 'escaped)))
    (appkit-loop-post loop 'break)
    (should (eq (catch 'escape (appkit-loop-run-pass loop)) 'escaped))
    (should (eq (appkit-loop-status loop) 'faulted))
    (should (= (appkit-loop-pending-count loop) 0))
    (should (string-match-p
             "exited nonlocally"
             (error-message-string
              (appkit-loop-fault-condition (appkit-loop-fault loop)))))))

(ert-deftest appkit-loop-post-schedules-a-pass ()
  (appkit-loop-test--with-loop
      (loop
       :model nil
       :update
       (lambda (model message)
         (appkit-loop-accept (cons message model))))
    (appkit-loop-post loop 'scheduled)
    (with-timeout (1 (ert-fail "Scheduled Appkit pass did not run"))
      (while (= (appkit-loop-revision loop) 0)
        (accept-process-output nil 0.01)))
    (should (equal (appkit-loop-model loop) '(scheduled)))
    (should (= (appkit-loop-pending-count loop) 0))))

(ert-deftest appkit-loop-stop-is-idempotent-and-closes-admission ()
  (let ((loop
         (appkit-loop-create
          :model nil
          :update (lambda (model _message) (appkit-loop-accept model)))))
    (appkit-loop-post loop 'pending)
    (should (appkit-loop-stop loop))
    (should (eq (appkit-loop-status loop) 'stopped))
    (should (= (appkit-loop-incarnation loop) 2))
    (should (= (appkit-loop-pending-count loop) 0))
    (should (eq (appkit-loop-post loop 'late) 'stopped))
    (should (eq (appkit-loop-send loop 'late) 'stopped))
    (should-not (appkit-loop-stop loop))))

(ert-deftest appkit-loop-rejects-mutation-from-worker-thread ()
  (appkit-loop-test--with-loop
      (loop
       :model nil
       :update (lambda (model _message) (appkit-loop-accept model)))
    (let ((condition
           (thread-join
            (make-thread
             (lambda ()
               (condition-case err
                   (progn (appkit-loop-post loop 'foreign) nil)
                 (error err)))))))
      (should condition)
      (should (string-match-p "main Emacs thread"
                              (error-message-string condition)))
      (should (= (appkit-loop-pending-count loop) 0)))))


(ert-deftest appkit-loop-after-pass-sees-final-committed-state-once ()
  (let (passes models)
    (appkit-loop-test--with-loop
        (loop
         :model nil
         :update
         (lambda (model message)
           (if (eq message 'reject)
               (appkit-loop-reject 'ignored)
             (appkit-loop-accept (append model (list message)))))
         :after-pass
         (lambda (current pass)
           (push pass passes)
           (push (copy-sequence (appkit-loop-model current)) models)))
      (dolist (message '(first reject second))
        (appkit-loop-post loop message))
      (should (= (appkit-loop-run-pass loop) 3))
      (should (= (length passes) 1))
      (let ((pass (car passes)))
        (should (= (appkit-loop-pass-processed pass) 3))
        (should (= (appkit-loop-pass-accepted pass) 2))
        (should (= (appkit-loop-pass-start-revision pass) 0))
        (should (= (appkit-loop-pass-end-revision pass) 2)))
      (should (equal models '((first second)))))))

(ert-deftest appkit-loop-after-pass-message-waits-for-next-pass ()
  (let (loop after-runs)
    (setq loop
          (appkit-loop-create
           :model nil
           :update
           (lambda (model message)
             (appkit-loop-accept (append model (list message))))
           :after-pass
           (lambda (_current _pass)
             (setq after-runs (1+ (or after-runs 0)))
             (when (= after-runs 1)
               (appkit-loop-post loop 'later)))))
    (unwind-protect
        (progn
          (appkit-loop-post loop 'first)
          (should (= (appkit-loop-run-pass loop) 1))
          (should (equal (appkit-loop-model loop) '(first)))
          (should (= (appkit-loop-pending-count loop) 1))
          (should (= (appkit-loop-run-pass loop) 1))
          (should (equal (appkit-loop-model loop) '(first later)))
          (should (= after-runs 2)))
      (appkit-loop-stop loop))))

(ert-deftest appkit-loop-after-pass-failure-faults-accepted-send ()
  (appkit-loop-test--with-loop
      (loop
       :model nil
       :update
       (lambda (model message)
         (appkit-loop-accept (cons message model)))
       :after-pass
       (lambda (_current _pass) (error "after-pass failed")))
    (let ((condition
           (should-error (appkit-loop-send loop 'commit) :type 'error)))
      (should (string-match-p "after-pass failed"
                              (error-message-string condition))))
    (should (eq (appkit-loop-status loop) 'faulted))
    (should (= (appkit-loop-revision loop) 1))
    (should (equal (appkit-loop-model loop) '(commit)))))

(ert-deftest appkit-loop-after-pass-nonlocal-exit-still-faults ()
  (appkit-loop-test--with-loop
      (loop
       :model nil
       :update
       (lambda (model message)
         (appkit-loop-accept (cons message model)))
       :after-pass
       (lambda (_current _pass) (throw 'escape 'escaped)))
    (should (eq (catch 'escape (appkit-loop-send loop 'commit)) 'escaped))
    (should (eq (appkit-loop-status loop) 'faulted))
    (should (= (appkit-loop-revision loop) 1))
    (should (string-match-p
             "after-pass exited nonlocally"
             (error-message-string
              (appkit-loop-fault-condition (appkit-loop-fault loop)))))))

(ert-deftest appkit-loop-fault-hook-runs-after-admission-closes ()
  (let (observed)
    (appkit-loop-test--with-loop
        (loop
         :model nil
         :update (lambda (_model _message) (error "update failed"))
         :on-fault
         (lambda (current fault)
           (setq observed
                 (list (appkit-loop-status current)
                       (appkit-loop-post current 'late)
                       (appkit-loop-fault-condition fault)))))
      (should-error (appkit-loop-send loop 'fail) :type 'error)
      (should (equal observed
                     '(faulted faulted (error "update failed")))))))

(ert-deftest appkit-loop-fault-cleanup-does-not-mask-primary ()
  (cl-letf (((symbol-function 'display-warning) #'ignore))
    (appkit-loop-test--with-loop
        (loop
         :model nil
         :update (lambda (_model _message) (error "primary"))
         :on-fault (lambda (_current _fault) (error "cleanup")))
      (let ((condition
             (should-error (appkit-loop-send loop 'fail) :type 'error)))
        (should (equal (error-message-string condition) "primary"))))))

(ert-deftest appkit-routing-fences-exact-addresses-and-reply-routes ()
  (let* ((loop
          (appkit-loop-create
           :owner-identity 'target
           :model nil
           :update
           (lambda (model message)
             (appkit-loop-accept (append model (list message))))))
         (address (appkit-routing--address loop))
         (route (appkit-routing--reply-route address 'request-1)))
    (unwind-protect
        (progn
          (should (eq (appkit-routing--post address 'direct) 'enqueued))
          (should (eq (appkit-routing--post route 'reply) 'enqueued))
          (should (= (appkit-loop-run-pass loop) 2))
          (should (equal (appkit-loop-model loop) '(direct reply)))
          (appkit-loop-stop loop)
          (let ((sequence (appkit-loop--next-sequence loop)))
            (should (eq (appkit-routing--post address 'late) 'stale))
            (should (eq (appkit-routing--post route 'late) 'stale))
            (should (= (appkit-loop--next-sequence loop) sequence)))
          (should
           (eq (appkit-routing--post
                (appkit-routing--address loop) 'late)
               'stopped)))
      (appkit-loop-stop loop))))

(provide 'appkit-loop-test)

;;; appkit-loop-test.el ends here
