;;; appkit-effect-test.el --- Tests for managed finite Effects -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(require 'appkit-effect)

(cl-defstruct (appkit-effect-test--harness
               (:constructor appkit-effect-test--harness-create)
               (:copier nil))
  loop
  runtime
  messages)

(cl-defun appkit-effect-test--make-harness
    (&key (mailbox-capacity 16) (message-limit 16) (max-active 8)
          client-update)
  "Create a loop and Effect runtime controlled by CLIENT-UPDATE."
  (let (runtime loop harness)
    (setq loop
          (appkit-loop-create
           :model nil
           :mailbox-capacity mailbox-capacity
           :message-limit message-limit
           :update
           (lambda (model input)
             (appkit-effect-runtime-dispatch
              runtime
              (or client-update
                  (lambda (current message)
                    (setf (appkit-effect-test--harness-messages harness)
                          (append
                           (appkit-effect-test--harness-messages harness)
                           (list message)))
                    (appkit-loop-accept (cons message current))))
              model input))))
    (setq runtime (appkit-effect-runtime-create loop max-active)
          harness
          (appkit-effect-test--harness-create
           :loop loop :runtime runtime :messages nil))
    harness))

(defun appkit-effect-test--cleanup (harness)
  "Stop HARNESS without allowing expected cleanup failures to escape."
  (when (appkit-effect-test--harness-p harness)
    (condition-case nil
        (appkit-effect-runtime-stop
         (appkit-effect-test--harness-runtime harness))
      ((error quit) nil))
    (appkit-loop-stop (appkit-effect-test--harness-loop harness))))

(defmacro appkit-effect-test--with-harness (binding &rest body)
  "Create the Effect harness described by BINDING and evaluate BODY."
  (declare (indent 1) (debug (sexp body)))
  (let ((harness (car binding))
        (arguments (cdr binding)))
    `(let ((,harness (appkit-effect-test--make-harness ,@arguments)))
       (unwind-protect
           (progn ,@body)
         (appkit-effect-test--cleanup ,harness)))))

(cl-defun appkit-effect-test--spec
    (key start &key (input key) observe observation-policy pending-limit
         success failure (cancellation-requirement 'logical))
  "Create a test Effect under KEY using START and mapper overrides."
  (appkit-effect-create
   :key key
   :input input
   :start start
   :observe observe
   :observation-policy observation-policy
   :observation-pending-limit pending-limit
   :success (or success (lambda (owned &rest payload)
                          (list 'success owned payload)))
   :failure (or failure (lambda (owned &rest payload)
                          (list 'failure owned payload)))
   :cancellation-requirement cancellation-requirement))

(ert-deftest appkit-effect-synchronous-settlement-is-deferred ()
  (let ((mapped 0))
    (appkit-effect-test--with-harness (harness)
      (let* ((runtime (appkit-effect-test--harness-runtime harness))
             (loop (appkit-effect-test--harness-loop harness))
             (spec
              (appkit-effect-test--spec
               'sync
               (lambda (_context _input _observe resolve _reject)
                 (funcall resolve 'ready)
                 nil)
               :success
               (lambda (input value)
                 (setq mapped (1+ mapped))
                 (list 'done input value)))))
        (appkit-effect-runtime-start runtime spec)
        (should (= mapped 0))
        (should (= (appkit-effect-runtime-count runtime) 1))
        (should (= (appkit-loop-pending-count loop) 1))
        (appkit-loop-run-pass loop)
        (should (= mapped 1))
        (should (equal (appkit-effect-test--harness-messages harness)
                       '((done sync ready))))
        (should (= (appkit-effect-runtime-count runtime) 0))
        (should (= (appkit-loop-revision loop) 1))))))

(ert-deftest appkit-effect-only-first-settlement-wins ()
  (let (resolve reject)
    (appkit-effect-test--with-harness (harness)
      (let* ((runtime (appkit-effect-test--harness-runtime harness))
             (loop (appkit-effect-test--harness-loop harness)))
        (appkit-effect-runtime-start
         runtime
         (appkit-effect-test--spec
          'once
          (lambda (_context _input _observe resolve-gate reject-gate)
            (setq resolve resolve-gate
                  reject reject-gate)
            (appkit-cancellation-create :kind 'logical))))
        (should (funcall resolve 'first))
        (should-not (funcall reject 'second))
        (should-not (funcall resolve 'duplicate))
        (appkit-loop-run-pass loop)
        (should (equal (appkit-effect-test--harness-messages harness)
                       '((success once (first)))))
        (should (= (appkit-effect-runtime-count runtime) 0))))))

(ert-deftest appkit-effect-replacement-revokes-old-callback-before-cancel ()
  (let ((callbacks (make-hash-table :test #'eq))
        cancelled)
    (appkit-effect-test--with-harness (harness)
      (let* ((runtime (appkit-effect-test--harness-runtime harness))
             (loop (appkit-effect-test--harness-loop harness))
             (starter
              (lambda (_context input _observe resolve _reject)
                (puthash input resolve callbacks)
                (appkit-cancellation-create
                 :kind 'logical
                 :cancel
                 (lambda ()
                   (push (list input
                               (appkit-effect-runtime-current-p runtime 'same))
                         cancelled))))))
        (appkit-effect-runtime-start
         runtime (appkit-effect-test--spec 'same starter :input 'old))
        (appkit-effect-runtime-start
         runtime (appkit-effect-test--spec 'same starter :input 'new))
        (should (equal cancelled '((old nil))))
        (should-not (funcall (gethash 'old callbacks) 'stale))
        (should (funcall (gethash 'new callbacks) 'current))
        (appkit-loop-run-pass loop)
        (should (equal (appkit-effect-test--harness-messages harness)
                       '((success new (current)))))))))

(ert-deftest appkit-effect-delivers-observations-before-settlement ()
  (let (observe resolve)
    (appkit-effect-test--with-harness (harness)
      (let* ((runtime (appkit-effect-test--harness-runtime harness))
             (loop (appkit-effect-test--harness-loop harness)))
        (appkit-effect-runtime-start
         runtime
         (appkit-effect-test--spec
          'progress
          (lambda (_context _input observe-gate resolve-gate _reject)
            (setq observe observe-gate
                  resolve resolve-gate)
            (appkit-cancellation-create :kind 'logical))
          :observe (lambda (_input value) (list 'progress value))
          :observation-policy 'lossless
          :pending-limit 4
          :success (lambda (_input value) (list 'done value))))
        (funcall observe 1)
        (funcall observe 2)
        (funcall resolve 'complete)
        (appkit-loop-run-pass loop)
        (should (equal (appkit-effect-test--harness-messages harness)
                       '((progress 1))))
        (appkit-loop-run-pass loop)
        (appkit-loop-run-pass loop)
        (should (equal (appkit-effect-test--harness-messages harness)
                       '((progress 1) (progress 2) (done complete))))
        (should (= (appkit-effect-runtime-count runtime) 0))))))

(ert-deftest appkit-effect-latest-observation-coalesces-in-place ()
  (let (observe resolve)
    (appkit-effect-test--with-harness (harness)
      (let* ((runtime (appkit-effect-test--harness-runtime harness))
             (loop (appkit-effect-test--harness-loop harness)))
        (appkit-effect-runtime-start
         runtime
         (appkit-effect-test--spec
          'latest
          (lambda (_context _input observe-gate resolve-gate _reject)
            (setq observe observe-gate
                  resolve resolve-gate)
            (appkit-cancellation-create :kind 'logical))
          :observe (lambda (_input value) (list 'progress value))
          :observation-policy 'latest))
        (funcall observe 1)
        (funcall observe 2)
        (funcall observe 3)
        (funcall resolve 'done)
        (appkit-loop-run-pass loop)
        (appkit-loop-run-pass loop)
        (should (equal (appkit-effect-test--harness-messages harness)
                       '((progress 3) (success latest (done)))))))))

(ert-deftest appkit-effect-coalesces-observation-by-first-payload-key ()
  (let (observe resolve)
    (appkit-effect-test--with-harness (harness)
      (let* ((runtime (appkit-effect-test--harness-runtime harness))
             (loop (appkit-effect-test--harness-loop harness)))
        (appkit-effect-runtime-start
         runtime
         (appkit-effect-test--spec
          'keyed
          (lambda (_context _input observe-gate resolve-gate _reject)
            (setq observe observe-gate
                  resolve resolve-gate)
            (appkit-cancellation-create :kind 'logical))
          :observe (lambda (_input key value) (list key value))
          :observation-policy 'coalesce-by-key
          :pending-limit 2))
        (funcall observe 'a 1)
        (funcall observe 'b 2)
        (funcall observe 'a 3)
        (funcall resolve 'done)
        (dotimes (_ 3) (appkit-loop-run-pass loop))
        (should (equal (appkit-effect-test--harness-messages harness)
                       '((b 2) (a 3) (success keyed (done)))))))))

(ert-deftest appkit-effect-lossless-overflow-settles-after-admitted-progress ()
  (let (observe cancelled)
    (appkit-effect-test--with-harness (harness)
      (let* ((runtime (appkit-effect-test--harness-runtime harness))
             (loop (appkit-effect-test--harness-loop harness)))
        (appkit-effect-runtime-start
         runtime
         (appkit-effect-test--spec
          'overflow
          (lambda (_context _input observe-gate _resolve _reject)
            (setq observe observe-gate)
            (appkit-cancellation-create
             :kind 'transport
             :cancel (lambda () (setq cancelled t))))
          :observe (lambda (_input value) (list 'progress value))
          :observation-policy 'lossless
          :pending-limit 2
          :cancellation-requirement 'transport))
        (should (funcall observe 1))
        (should (funcall observe 2))
        (should-not (funcall observe 3))
        (should cancelled)
        (dotimes (_ 3) (appkit-loop-run-pass loop))
        (should (equal (appkit-effect-test--harness-messages harness)
                       '((progress 1)
                         (progress 2)
                         (failure overflow (observation-overflow)))))
        (should (= (appkit-effect-runtime-count runtime) 0))))))

(ert-deftest appkit-effect-retries-wake-after-mailbox-capacity ()
  (appkit-effect-test--with-harness
      (harness :mailbox-capacity 1 :message-limit 1)
    (let* ((runtime (appkit-effect-test--harness-runtime harness))
           (loop (appkit-effect-test--harness-loop harness)))
      (appkit-loop-post loop 'blocker)
      (appkit-effect-runtime-start
       runtime
       (appkit-effect-test--spec
        'retry
        (lambda (_context _input _observe resolve _reject)
          (funcall resolve 'ready)
          nil)))
      (should (= (appkit-loop-pending-count loop) 1))
      (appkit-loop-run-pass loop)
      (with-timeout (1 (ert-fail "Effect wake retry did not complete"))
        (while (= (appkit-effect-runtime-count runtime) 1)
          (accept-process-output nil 0.01)))
      (should (equal (appkit-effect-test--harness-messages harness)
                     '(blocker (success retry (ready)))))
      (should (= (appkit-loop-pending-count loop) 0)))))

(ert-deftest appkit-effect-cancel-makes-queued-delivery-stale ()
  (let (resolve cancelled)
    (appkit-effect-test--with-harness (harness)
      (let* ((runtime (appkit-effect-test--harness-runtime harness))
             (loop (appkit-effect-test--harness-loop harness)))
        (appkit-effect-runtime-start
         runtime
         (appkit-effect-test--spec
          'cancel
          (lambda (_context _input _observe resolve-gate _reject)
            (setq resolve resolve-gate)
            (appkit-cancellation-create
             :kind 'logical
             :cancel (lambda () (setq cancelled t))))))
        (funcall resolve 'queued)
        (should (appkit-effect-runtime-cancel runtime 'cancel))
        (should cancelled)
        (should (= (appkit-loop-run-pass loop) 1))
        (should-not (appkit-effect-test--harness-messages harness))
        (should (= (appkit-loop-revision loop) 0))))))

(ert-deftest appkit-effect-starter-failure-does-not-leave-slot ()
  (appkit-effect-test--with-harness (harness)
    (let ((runtime (appkit-effect-test--harness-runtime harness)))
      (should-error
       (appkit-effect-runtime-start
        runtime
        (appkit-effect-test--spec
         'broken
         (lambda (_context _input _observe _resolve _reject)
           (error "starter failed"))))
       :type 'error)
      (should (= (appkit-effect-runtime-count runtime) 0))
      (should-not (appkit-effect-runtime-current-p runtime 'broken)))))

(ert-deftest appkit-effect-active-starter-must-return-capability ()
  (appkit-effect-test--with-harness (harness)
    (let ((runtime (appkit-effect-test--harness-runtime harness)))
      (should-error
       (appkit-effect-runtime-start
        runtime
        (appkit-effect-test--spec
         'missing-capability
         (lambda (_context _input _observe _resolve _reject) nil)))
       :type 'error)
      (should (= (appkit-effect-runtime-count runtime) 0)))))

(ert-deftest appkit-effect-invalid-transport-capability-is-cleaned ()
  (let (cancelled)
    (appkit-effect-test--with-harness (harness)
      (let ((runtime (appkit-effect-test--harness-runtime harness)))
        (should-error
         (appkit-effect-runtime-start
          runtime
          (appkit-effect-test--spec
           'transport
           (lambda (_context _input _observe _resolve _reject)
             (appkit-cancellation-create
              :kind 'logical
              :cancel (lambda () (setq cancelled t))))
           :cancellation-requirement 'transport))
         :type 'error)
        (should cancelled)
        (should (= (appkit-effect-runtime-count runtime) 0))))))

(ert-deftest appkit-effect-mapper-failure-faults-owning-loop ()
  (appkit-effect-test--with-harness (harness)
    (let* ((runtime (appkit-effect-test--harness-runtime harness))
           (loop (appkit-effect-test--harness-loop harness)))
      (appkit-effect-runtime-start
       runtime
       (appkit-effect-test--spec
        'mapper
        (lambda (_context _input _observe resolve _reject)
          (funcall resolve 'ready)
          nil)
        :success (lambda (_input _value) (error "mapper failed"))))
      (appkit-loop-run-pass loop)
      (should (eq (appkit-loop-status loop) 'faulted))
      (should (= (appkit-effect-runtime-count runtime) 0))
      (should (string-match-p
               "mapper failed"
               (error-message-string
                (appkit-loop-fault-condition (appkit-loop-fault loop))))))))

(ert-deftest appkit-effect-enforces-active-instance-limit ()
  (appkit-effect-test--with-harness (harness :max-active 1)
    (let ((runtime (appkit-effect-test--harness-runtime harness))
          (starter
           (lambda (_context _input _observe _resolve _reject)
             (appkit-cancellation-create :kind 'logical))))
      (appkit-effect-runtime-start
       runtime (appkit-effect-test--spec 'one starter))
      (should-error
       (appkit-effect-runtime-start
        runtime (appkit-effect-test--spec 'two starter))
       :type 'error)
      (should (appkit-effect-runtime-current-p runtime 'one))
      (should-not (appkit-effect-runtime-current-p runtime 'two)))))

(ert-deftest appkit-effect-gates-reject-worker-thread-callback ()
  (let (resolve)
    (appkit-effect-test--with-harness (harness)
      (let ((runtime (appkit-effect-test--harness-runtime harness)))
        (appkit-effect-runtime-start
         runtime
         (appkit-effect-test--spec
          'thread
          (lambda (_context _input _observe resolve-gate _reject)
            (setq resolve resolve-gate)
            (appkit-cancellation-create :kind 'logical))))
        (let ((condition
               (thread-join
                (make-thread
                 (lambda ()
                   (condition-case err
                       (progn (funcall resolve 'foreign) nil)
                     (error err)))))))
          (should condition)
          (should (string-match-p "main Emacs thread"
                                  (error-message-string condition)))
          (should (appkit-effect-runtime-current-p runtime 'thread))
          (should (= (appkit-loop-pending-count
                      (appkit-effect-test--harness-loop harness))
                     0)))))))

(ert-deftest appkit-effect-stop-cleans-all-instances-after-one-error ()
  (let (cancelled)
    (appkit-effect-test--with-harness (harness)
      (let* ((runtime (appkit-effect-test--harness-runtime harness))
             (starter
              (lambda (_context input _observe _resolve _reject)
                (appkit-cancellation-create
                 :kind 'transport
                 :cancel
                 (if (eq input 'bad)
                     (lambda ()
                       (push 'bad cancelled)
                       (error "cancel failed"))
                   (lambda () (push 'good cancelled)))))))
        (appkit-effect-runtime-start
         runtime
         (appkit-effect-test--spec
          'bad starter :input 'bad
          :cancellation-requirement 'transport))
        (appkit-effect-runtime-start
         runtime
         (appkit-effect-test--spec
          'good starter :input 'good
          :cancellation-requirement 'transport))
        (should-error (appkit-effect-runtime-stop runtime) :type 'error)
        (should (= (length cancelled) 2))
        (should (memq 'bad cancelled))
        (should (memq 'good cancelled))
        (should (= (appkit-effect-runtime-count runtime) 0))
        (should-not (appkit-effect-runtime-live-p runtime))))))

(provide 'appkit-effect-test)

;;; appkit-effect-test.el ends here
