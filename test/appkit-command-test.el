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
  (let* ((batch (appkit-command--batch-create))
         (first-a
          (appkit-command-start-effect
           (appkit-command-test--effect 'a)))
         (start-b
          (appkit-command-start-effect
           (appkit-command-test--effect 'b)))
         (cancel-a (appkit-command-cancel-effect 'a)))
    (appkit-command--batch-add batch (list first-a start-b))
    (appkit-command--batch-add batch (list cancel-a))
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
  (let* ((batch (appkit-command--batch-create))
         (first
          (appkit-command-source-intent
           :key 'stream
           :expected-identity 'one
           :payload 'first
           :result-mapper #'ignore))
         (second
          (appkit-command-source-intent
           :key 'stream
           :expected-identity 'one
           :payload 'second
           :result-mapper #'ignore)))
    (appkit-command--batch-add batch (list first second))
    (let ((work (appkit-command--batch-drain batch)))
      (should
       (equal (appkit-command-work-source-intents work)
              (list first second)))))
  (should-error
   (appkit-command-source-intent
    :key 'stream
    :expected-identity 'one
    :payload nil
    :result-mapper (lambda (&rest _arguments)))
   :type 'error))

(ert-deftest appkit-command-folds-large-batches-in-final-keyed-order ()
  (let* ((batch (appkit-command--batch-create))
         (starts (cl-loop for key below 256
                          collect (appkit-command-start-effect
                                   (appkit-command-test--effect key))))
         (cancels (cl-loop for key downfrom 255 to 0
                           collect (appkit-command-cancel-effect key))))
    (appkit-command--batch-add batch (append starts cancels))
    (should (equal (appkit-command-work-effects
                    (appkit-command--batch-drain batch))
                   cancels)))
  (let ((batch (appkit-command--batch-create)))
    (dotimes (key 256)
      (appkit-command--batch-add
       batch (list (appkit-command-cancel-effect key))))
    (should (= (length (appkit-command-work-effects
                        (appkit-command--batch-drain batch)))
               256))))

(ert-deftest appkit-command-rejects-malformed-lists-before-folding ()
  (let* ((command (appkit-command-cancel-effect 'same))
         (circular (list command))
         (batch (appkit-command--batch-create)))
    (setcdr circular circular)
    (dolist (commands (list circular (cons command 'tail) 'not-a-list))
      (should-error (appkit-command--batch-add batch commands) :type 'error)
      (let ((work (appkit-command--batch-drain batch)))
        (should-not (appkit-command-work-effects work))
        (should-not (appkit-command-work-posts work))
        (should-not (appkit-command-work-source-intents work))))))



(ert-deftest appkit-next-rejection-is-not-a-loop-result ()
  (let ((result (appkit-next-reject 'not-allowed)))
    (should (appkit-next-rejected-p result))
    (should (eq (appkit-next-rejected-reason result) 'not-allowed))
    (should-not (appkit-loop-rejected-p result))))

(provide 'appkit-command-test)

;;; appkit-command-test.el ends here
