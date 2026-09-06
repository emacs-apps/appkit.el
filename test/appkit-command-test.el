;;; appkit-command-test.el --- Tests for closed AppKit commands -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'appkit-command)

(defun appkit-command-test--effect (key)
  "Create an inert logical Effect under KEY."
  (appkit-effect-create
   :key key
   :start (lambda (_context _input _observe _resolve _reject)
            (appkit-cancellation-create :kind 'logical))
   :success #'ignore
   :failure #'ignore))

(ert-deftest appkit-next-requires-explicit-render-disposition ()
  (should-error (appkit-next :model 'model) :type 'error)
  (let ((next (appkit-next :model 'model :render nil)))
    (should (appkit-next-p next))
    (should-not (appkit-next-render next))))

(ert-deftest appkit-command-fold-keeps-only-final-keyed-deltas ()
  (let* ((batch (appkit-command--batch-create 3))
         (first-a
          (appkit-command-start-effect
           (appkit-command-test--effect 'a)))
         (start-b
          (appkit-command-start-effect
           (appkit-command-test--effect 'b)))
         (cancel-a (appkit-command-cancel-effect 'a)))
    (appkit-command--batch-add batch (list first-a start-b) 2)
    (appkit-command--batch-add batch (list cancel-a) 2)
    (let ((work (appkit-command--batch-drain batch)))
      (should-not (appkit-command-work-posts work))
      (should
       (equal (appkit-command-work-effects work)
              (list start-b cancel-a)))
      (should-not (appkit-command-work-source-intents work)))
    (let ((work (appkit-command--batch-drain batch)))
      (should-not (appkit-command-work-posts work))
      (should-not (appkit-command-work-effects work))
      (should-not (appkit-command-work-source-intents work)))))

(ert-deftest appkit-command-source-intents-preserve-fifo ()
  (let* ((batch (appkit-command--batch-create 2))
         (first
          (appkit-command-source-intent
           :key 'stream :expected-identity 'one :payload 'first
           :result-mapper #'ignore))
         (second
          (appkit-command-source-intent
           :key 'stream :expected-identity 'one :payload 'second
           :result-mapper #'ignore)))
    (appkit-command--batch-add batch (list first second) 2)
    (let ((work (appkit-command--batch-drain batch)))
      (should
       (equal (appkit-command-work-source-intents work)
              (list first second)))))
  (should-error
   (appkit-command-source-intent
    :key 'stream :expected-identity 'one :payload nil
    :result-mapper (lambda (&rest _arguments)))
   :type 'error))

(ert-deftest appkit-command-enforces-folded-key-boundary ()
  (let ((batch (appkit-command--batch-create 1)))
    (appkit-command--batch-add
     batch (list (appkit-command-cancel-effect 'same)) 1)
    (appkit-command--batch-add
     batch
     (list
      (appkit-command-start-effect
       (appkit-command-test--effect 'same)))
     1)
    (should-error
     (appkit-command--batch-add
      batch (list (appkit-command-cancel-effect 'other)) 1)
     :type 'error)))

(ert-deftest appkit-command-bounds-circular-command-lists ()
  (let* ((commands (list (appkit-command-cancel-effect 'same)))
         (batch (appkit-command--batch-create 1)))
    (setcdr commands commands)
    (should-error (appkit-command--batch-add batch commands 1) :type 'error)))

(ert-deftest appkit-command-default-capacities-have-hard-boundaries ()
  (let* ((command (appkit-command-cancel-effect 'same))
         (at-limit
          (make-list appkit-command-default-per-next-limit command)))
    (appkit-command--batch-add
     (appkit-command--batch-create appkit-command-default-folded-limit)
     at-limit
     appkit-command-default-per-next-limit)
    (should-error
     (appkit-command--batch-add
      (appkit-command--batch-create appkit-command-default-folded-limit)
      (cons command at-limit)
      appkit-command-default-per-next-limit)
     :type 'error))
  (let ((batch
         (appkit-command--batch-create
          appkit-command-default-folded-limit)))
    (dotimes (key appkit-command-default-folded-limit)
      (appkit-command--batch-add
       batch (list (appkit-command-cancel-effect key))
       appkit-command-default-per-next-limit))
    (should-error
     (appkit-command--batch-add
      batch
      (list
       (appkit-command-cancel-effect
        appkit-command-default-folded-limit))
      appkit-command-default-per-next-limit)
     :type 'error)))

(ert-deftest appkit-next-rejection-is-not-a-loop-result ()
  (let ((result (appkit-next-reject 'not-allowed)))
    (should (appkit-next-rejected-p result))
    (should (eq (appkit-next-rejected-reason result) 'not-allowed))
    (should-not (appkit-loop-rejected-p result))))

(provide 'appkit-command-test)

;;; appkit-command-test.el ends here
