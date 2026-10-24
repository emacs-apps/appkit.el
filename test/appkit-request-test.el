;;; appkit-request-test.el --- Routed request authority tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit-app)
(require 'appkit-surface)

(defmacro appkit-request-test--with-runtime (&rest body)
  "Run BODY with a controlled Surface request and canonical App commit."
  (declare (indent 0) (debug t))
  `(let ((gates (make-hash-table :test #'eq)) cancelled (reject-commit nil) app surface buffer)
     (unwind-protect
         (progn
           (setq app
                 (appkit-app-start
                  (appkit-app-type-create
                   :name 'request-test
                   :init (lambda (_context input)
                           (appkit-next :model input :render appkit-render-none))
                   :update
                   (lambda (context model message)
                     (if reject-commit
                         (appkit-next-reject reject-commit)
                       (appkit-next
                      :model (cons message model) :render appkit-render-none
                      :commands
                      (list (appkit-command-post-message
                             :target (appkit-transition-context-reply-route context)
                             :message (list 'committed message) :delivery 'report))))))))
           (setq surface
                 (appkit-open-generated-surface
                  (appkit-surface-type-create
                   :name 'request-test :mode #'special-mode
                   :init (lambda (_context input)
                           (appkit-next :model input :render appkit-render-none))
                   :update
                   (lambda (context model message)
                     (pcase message
                       ('noop (appkit-next :model model :render appkit-render-none))
                       (`(start ,label)
                        (appkit-next
                         :model (list 'loading label) :render appkit-render-none
                         :commands
                         (list
                          (appkit-command-start-effect
                           (appkit-effect-create
                            :key 'history :lifetime 'request :input label
                            :start
                            (lambda (_context input _observe resolve _reject)
                              (puthash input resolve gates)
                              (appkit-cancellation-create
                               :kind 'transport :cancel (lambda () (push input cancelled))))
                            :cancellation-requirement 'transport
                            :success (lambda (input value) (list 'loaded input value))
                            :failure (lambda (input error) (list 'failed input error)))))))
                       (`(loaded ,label ,value)
                        (appkit-next
                         :model model :render appkit-render-none
                         :commands
                         (list (appkit-command-post-message
                                :target (appkit-transition-context-parent-address context)
                                :message (list label value) :delivery 'report))))
                       ((or 'cancel `(committed ,_) `(failed . ,_)
                            (pred appkit-delivery-report-p))
                        (appkit-next
                         :model message :render appkit-render-none
                         :commands (list (appkit-command-cancel-effect 'history))))))
                   :renderer-factory
                   (lambda (_surface)
                     (appkit-generated-renderer-create
                      :mount (lambda (&rest _)) :merge (lambda (_left right) right)
                      :render (lambda (&rest _)) :recover (lambda (&rest _))
                      :unmount (lambda (&rest _)))))
                  :app app :identity 'history))
           (setq buffer (appkit-surface-buffer surface))
           ,@body)
       (when app (appkit-app-close app))
       (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun appkit-request-test--settle (surface gates label value)
  "Settle LABEL and let SURFACE enqueue its canonical commit."
  (funcall (gethash label gates) value)
  (appkit-loop-run-pass (appkit-surface-loop surface)))

(ert-deftest appkit-request-authority-survives-settlement-until-reply ()
  (appkit-request-test--with-runtime
    (appkit-surface-send surface '(start first))
    (let* ((runtime (appkit-surface-effect-runtime surface))
           (identity (appkit-effect-runtime-request runtime 'history)))
      (should identity)
      (appkit-request-test--settle surface gates 'first 42)
      (should (eq identity (appkit-effect-runtime-request runtime 'history)))
      (should-not (funcall (gethash 'first gates) 'duplicate))
      (should (= 1 (appkit-effect-runtime-count runtime)))
      (appkit-loop-run-pass (appkit-app-loop app))
      (should (equal (appkit-app-model app) '((first 42))))
      (appkit-loop-run-pass (appkit-surface-loop surface))
      (should (equal (appkit-surface-model surface) '(committed (first 42))))
      (should-not (appkit-effect-runtime-request runtime 'history))
      (should (= 0 (appkit-effect-runtime-count runtime)))
      (should-not cancelled))))

(ert-deftest appkit-request-cancel-discards-already-queued-canonical-commit ()
  (appkit-request-test--with-runtime
    (appkit-surface-send surface '(start first))
    (appkit-request-test--settle surface gates 'first 42)
    (appkit-surface-send surface 'cancel)
    (appkit-loop-run-pass (appkit-app-loop app))
    (should-not (appkit-app-model app))
    (should (eq (appkit-surface-model surface) 'cancel))))

(ert-deftest appkit-request-replacement-fences-old-reply-in-same-pass ()
  (appkit-request-test--with-runtime
    (appkit-surface-send surface '(start first))
    (appkit-request-test--settle surface gates 'first 42)
    ;; Both data messages are frozen into one Surface pass: replacement first,
    ;; then the old App reply.  Physical Effect replacement happens afterward.
    (appkit-surface-post surface '(start second))
    (appkit-loop-run-pass (appkit-app-loop app))
    (should (= 2 (appkit-loop-run-pass (appkit-surface-loop surface))))
    (should (equal (appkit-surface-model surface) '(loading second)))
    (should (appkit-effect-runtime-request
             (appkit-surface-effect-runtime surface) 'history))
    (appkit-request-test--settle surface gates 'second 43)
    (appkit-loop-run-pass (appkit-app-loop app))
    (appkit-loop-run-pass (appkit-surface-loop surface))
    (should (equal (appkit-surface-model surface) '(committed (second 43))))))

(ert-deftest appkit-request-cancel-fences-old-reply-in-same-pass ()
  (appkit-request-test--with-runtime
    (appkit-surface-send surface '(start first))
    (appkit-request-test--settle surface gates 'first 42)
    (appkit-surface-post surface 'cancel)
    (appkit-loop-run-pass (appkit-app-loop app))
    (should (= 2 (appkit-loop-run-pass (appkit-surface-loop surface))))
    (should (eq (appkit-surface-model surface) 'cancel))
    (should (= 0 (appkit-effect-runtime-count
                  (appkit-surface-effect-runtime surface))))))

(ert-deftest appkit-request-closed-owner-revokes-queued-commit ()
  (appkit-request-test--with-runtime
    (appkit-surface-send surface '(start first))
    (appkit-request-test--settle surface gates 'first 42)
    (appkit-surface-stop surface)
    (appkit-loop-run-pass (appkit-app-loop app))
    (should-not (appkit-app-model app))
    (should (= 0 (appkit-effect-runtime-count
                  (appkit-surface-effect-runtime surface))))))

(ert-deftest appkit-request-cancel-before-settlement-cancels-transport-once ()
  (appkit-request-test--with-runtime
    (appkit-surface-send surface '(start first))
    (appkit-surface-send surface 'cancel)
    (appkit-surface-send surface 'cancel)
    (should (equal cancelled '(first)))
    (should-not (funcall (gethash 'first gates) 42))
    (should-not (appkit-app-model app))))

(ert-deftest appkit-request-replacement-discards-post-staged-earlier-in-pass ()
  (appkit-request-test--with-runtime
    (appkit-surface-send surface '(start first))
    (funcall (gethash 'first gates) 42)
    (appkit-surface-post surface '(start second))
    ;; The old result is control-lane work; its commit post is staged before
    ;; the newer data-lane start logically revokes it within the same pass.
    (appkit-loop-run-pass (appkit-surface-loop surface))
    (appkit-loop-run-pass (appkit-app-loop app))
    (should-not (appkit-app-model app))
    (should (equal (appkit-surface-model surface) '(loading second)))))

(ert-deftest appkit-request-routing-failure-is-a-scoped-terminal-report ()
  (appkit-request-test--with-runtime
    (appkit-surface-send surface '(start first))
    (cl-letf (((symbol-function 'appkit-routing--admit)
               (lambda (&rest _arguments) 'full)))
      (appkit-request-test--settle surface gates 'first 42))
    (appkit-loop-run-pass (appkit-surface-loop surface))
    (should (appkit-delivery-report-p (appkit-surface-model surface)))
    (should (eq (appkit-delivery-report-outcome (appkit-surface-model surface)) 'full))
    (should (= 0 (appkit-effect-runtime-count
                  (appkit-surface-effect-runtime surface))))))

(ert-deftest appkit-request-retained-slots-obey-capacity-and-release ()
  (let (runtime loop)
    (unwind-protect
        (progn
          (setq loop
                (appkit-loop-create
                 :model nil
                 :update (lambda (model message)
                           (appkit-effect-runtime-dispatch
                            runtime (lambda (current _message) (appkit-loop-accept current))
                            model message)))
                runtime (appkit-effect-runtime-create loop 1))
          (cl-flet ((spec (key)
                      (appkit-effect-create
                       :key key :lifetime 'request
                       :start (lambda (_context _input _observe resolve _reject)
                                (funcall resolve 'done) nil)
                       :success (lambda (_input result) result)
                       :failure (lambda (_input error) error))))
            (appkit-effect-runtime-start runtime (spec 'one))
            (appkit-loop-run-pass loop)
            (should (= 1 (appkit-effect-runtime-count runtime)))
            (should-error (appkit-effect-runtime-start runtime (spec 'two)))
            (appkit-effect-runtime-cancel runtime 'one)
            (appkit-effect-runtime-start runtime (spec 'two))
            (appkit-loop-run-pass loop)
            (should (appkit-effect-runtime-request runtime 'two))))
      (when runtime (appkit-effect-runtime-stop runtime))
      (when loop (appkit-loop-stop loop)))))

(ert-deftest appkit-request-full-reply-mailbox-reports-to-origin-control-lane ()
  (appkit-request-test--with-runtime
    (appkit-surface-send surface '(start first))
    (appkit-request-test--settle surface gates 'first 42)
    (let ((loop (appkit-surface-loop surface)))
      (setf (appkit-loop--lane-capacity (appkit-loop--data-lane loop)) 1)
      (appkit-surface-post surface 'noop)
      (appkit-loop-run-pass (appkit-app-loop app))
      ;; The App has committed; its reply cannot enter the full data lane.
      (should (equal (appkit-app-model app) '((first 42))))
      (should (= 2 (appkit-loop-pending-count loop)))
      (appkit-loop-run-pass loop)
      (should (appkit-delivery-report-p (appkit-surface-model surface)))
      (should (eq (appkit-delivery-report-outcome (appkit-surface-model surface)) 'full))
      (should (eq (appkit-delivery-report-correlation (appkit-surface-model surface)) 'history))
      (should (= 0 (appkit-effect-runtime-count
                    (appkit-surface-effect-runtime surface)))))))

(ert-deftest appkit-request-rejected-canonical-commit-reports-to-origin ()
  (appkit-request-test--with-runtime
    (setq reject-commit 'invalid-page)
    (appkit-surface-send surface '(start first))
    (appkit-request-test--settle surface gates 'first 42)
    (appkit-loop-run-pass (appkit-app-loop app))
    (should-not (appkit-app-model app))
    (appkit-loop-run-pass (appkit-surface-loop surface))
    (let ((report (appkit-surface-model surface)))
      (should (appkit-delivery-report-p report))
      (should (eq (appkit-delivery-report-outcome report) 'rejected))
      (should (eq (appkit-delivery-report-reason report) 'invalid-page))
      (should (eq (appkit-delivery-report-correlation report) 'history)))
    (should (= 0 (appkit-effect-runtime-count
                  (appkit-surface-effect-runtime surface))))))

(provide 'appkit-request-test)
;;; appkit-request-test.el ends here
