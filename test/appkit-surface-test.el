;;; appkit-surface-test.el --- Tests for generated Surface runtime -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'appkit-surface)

(define-derived-mode appkit-surface-test-mode special-mode "Appkit-Surface")

(defun appkit-surface-test--renderer
    (record &optional render-function recover-function mount-function)
  "Create a test renderer reporting lifecycle events through RECORD."
  (appkit-generated-renderer-create
   :mount
   (or mount-function
       (lambda (_surface model) (funcall record (list 'mount model))))
   :merge
   (lambda (left right)
     (append (if (listp left) left (list left))
             (if (listp right) right (list right))))
   :render
   (or render-function
       (lambda (_surface model request)
         (funcall record (list 'render model request))
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (format "%S" model)))))
   :recover
   (or recover-function
       (lambda (_surface model condition)
         (funcall record
                  (list 'recover model (error-message-string condition)))
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (format "recovered:%S" model)))))
   :unmount
   (lambda (_surface) (funcall record '(unmount)))))

(cl-defun appkit-surface-test--type
    (&key init update renderer-factory)
  "Create a generated test Surface type."
  (appkit-surface-type-create
   :name 'test-generated
   :mode #'appkit-surface-test-mode
   :init init
   :update update
   :renderer-factory renderer-factory))

(defun appkit-surface-test--cleanup (surface)
  "Stop SURFACE and kill its test buffer."
  (when (appkit-surface-p surface)
    (let ((buffer (appkit-surface-buffer surface)))
      (condition-case nil
          (appkit-surface-stop surface)
        ((error quit) nil))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest appkit-surface-initializes-mode-before-attachment ()
  (let (events init-mode init-owner surface)
    (unwind-protect
        (progn
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-test--type
                  :init
                  (lambda (_surface input)
                    (setq init-mode major-mode
                          init-owner (appkit-current-surface))
                    (appkit-surface-next input 'initial))
                  :update
                  (lambda (_surface model _message)
                    (appkit-surface-next model appkit-render-none))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-surface-test--renderer
                     (lambda (event) (push event events)))))
                 :input 'ready))
          (should (eq init-mode 'appkit-surface-test-mode))
          (should-not init-owner)
          (should (appkit-surface-live-p surface))
          (with-current-buffer (appkit-surface-buffer surface)
            (should (eq (appkit-current-surface) surface))
            (should (equal (buffer-string) "ready")))
          (should (equal (nreverse events)
                         '((mount ready) (render ready initial)))))
      (appkit-surface-test--cleanup surface))))

(ert-deftest appkit-surface-merges-one-render-per-loop-pass ()
  (let (events surface)
    (unwind-protect
        (progn
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-test--type
                  :init (lambda (_surface _input)
                          (appkit-surface-next nil appkit-render-none))
                  :update
                  (lambda (_surface model message)
                    (appkit-surface-next
                     (append model (list message)) (list message)))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-surface-test--renderer
                     (lambda (event) (push event events)))))))
          (appkit-surface-post surface 'first)
          (appkit-surface-post surface 'second)
          (should (= (appkit-loop-run-pass (appkit-surface-loop surface)) 2))
          (should (equal (appkit-surface-model surface) '(first second)))
          (should (equal (nreverse events)
                         '((mount nil)
                           (render (first second) (first second))))))
      (appkit-surface-test--cleanup surface))))

(ert-deftest appkit-surface-explicit-no-render-skips-presentation ()
  (let (events surface)
    (unwind-protect
        (progn
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-test--type
                  :init (lambda (_surface _input)
                          (appkit-surface-next 'initial appkit-render-none))
                  :update
                  (lambda (_surface _model _message)
                    (appkit-surface-next 'changed appkit-render-none))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-surface-test--renderer
                     (lambda (event) (push event events)))))))
          (let ((ticket (appkit-surface-send surface 'change)))
            (should (eq (appkit-loop-ticket-state ticket) 'accepted)))
          (should (eq (appkit-surface-model surface) 'changed))
          (should (equal events '((mount initial)))))
      (appkit-surface-test--cleanup surface))))

(ert-deftest appkit-surface-render-failure-recovers-from-committed-model ()
  (let (events surface)
    (unwind-protect
        (progn
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-test--type
                  :init (lambda (_surface _input)
                          (appkit-surface-next 'initial appkit-render-none))
                  :update
                  (lambda (_surface _model message)
                    (appkit-surface-next message 'render))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-surface-test--renderer
                     (lambda (event) (push event events))
                     (lambda (_surface _model _request)
                       (error "incremental render failed")))))))
          (let ((ticket (appkit-surface-send surface 'committed)))
            (should (eq (appkit-loop-ticket-state ticket) 'accepted)))
          (should (eq (appkit-surface-model surface) 'committed))
          (should (appkit-surface-renderer-valid-p surface))
          (with-current-buffer (appkit-surface-buffer surface)
            (should (equal (buffer-string) "recovered:committed")))
          (should (eq (caar events) 'recover)))
      (appkit-surface-test--cleanup surface))))

(ert-deftest appkit-surface-second-render-failure-faults-send ()
  (let (events surface)
    (unwind-protect
        (progn
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-test--type
                  :init (lambda (_surface _input)
                          (appkit-surface-next 'initial appkit-render-none))
                  :update
                  (lambda (_surface _model message)
                    (appkit-surface-next message 'render))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-surface-test--renderer
                     (lambda (event) (push event events))
                     (lambda (_surface _model _request)
                       (error "render failed"))
                     (lambda (_surface _model _condition)
                       (error "recover failed")))))))
          (let ((condition
                 (should-error
                  (appkit-surface-send surface 'committed)
                  :type 'error)))
            (should (string-match-p "render failed"
                                    (error-message-string condition)))
            (should (string-match-p "recover failed"
                                    (error-message-string condition))))
          (should (eq (appkit-surface-model surface) 'committed))
          (should (eq (appkit-surface-status surface) 'faulted))
          (should-not (appkit-surface-renderer-valid-p surface)))
      (appkit-surface-test--cleanup surface))))

(ert-deftest appkit-surface-domain-rejection-does-not-render ()
  (let (events surface)
    (unwind-protect
        (progn
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-test--type
                  :init (lambda (_surface _input)
                          (appkit-surface-next 'initial appkit-render-none))
                  :update
                  (lambda (_surface _model _message)
                    (appkit-loop-reject 'not-allowed))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-surface-test--renderer
                     (lambda (event) (push event events)))))))
          (let ((ticket (appkit-surface-send surface 'reject)))
            (should (eq (appkit-loop-ticket-state ticket) 'rejected))
            (should (eq (appkit-loop-ticket-outcome ticket) 'not-allowed)))
          (should (eq (appkit-surface-model surface) 'initial))
          (should (= (appkit-loop-revision (appkit-surface-loop surface)) 0))
          (should (equal events '((mount initial)))))
      (appkit-surface-test--cleanup surface))))

(ert-deftest appkit-surface-open-failure-removes-created-buffer ()
  (let ((name (generate-new-buffer-name " *appkit-surface-open-failure*"))
        unmounted)
    (should-error
     (appkit-open-generated-surface
      (appkit-surface-test--type
       :init (lambda (_surface _input)
               (appkit-surface-next nil appkit-render-none))
       :update (lambda (_surface model _message)
                 (appkit-surface-next model appkit-render-none))
       :renderer-factory
       (lambda (_surface)
         (appkit-generated-renderer-create
          :mount (lambda (_surface _model) (error "mount failed"))
          :merge (lambda (_left right) right)
          :render (lambda (&rest _arguments))
          :recover (lambda (&rest _arguments))
          :unmount (lambda (_surface) (setq unmounted t)))))
      :buffer-name name)
     :type 'error)
    (should unmounted)
    (should-not (get-buffer name))))

(ert-deftest appkit-surface-kill-buffer-stops-and-unmounts ()
  (let (events surface)
    (setq surface
          (appkit-open-generated-surface
           (appkit-surface-test--type
            :init (lambda (_surface _input)
                    (appkit-surface-next nil appkit-render-none))
            :update (lambda (_surface model _message)
                      (appkit-surface-next model appkit-render-none))
            :renderer-factory
            (lambda (_surface)
              (appkit-surface-test--renderer
               (lambda (event) (push event events)))))))
    (kill-buffer (appkit-surface-buffer surface))
    (should-not (appkit-surface-live-p surface))
    (should (eq (appkit-surface-status surface) 'stopped))
    (should (equal events '((unmount) (mount nil))))))

(ert-deftest appkit-surface-post-renders-through-real-timer ()
  (let (events surface)
    (unwind-protect
        (progn
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-test--type
                  :init (lambda (_surface _input)
                          (appkit-surface-next nil appkit-render-none))
                  :update
                  (lambda (_surface model message)
                    (appkit-surface-next (cons message model) 'full))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-surface-test--renderer
                     (lambda (event) (push event events)))))))
          (appkit-surface-post surface 'timer)
          (with-timeout (1 (ert-fail "Surface timer render did not run"))
            (while (= (appkit-loop-revision (appkit-surface-loop surface)) 0)
              (accept-process-output nil 0.01)))
          (with-current-buffer (appkit-surface-buffer surface)
            (should (equal (buffer-string) "(timer)"))))
      (appkit-surface-test--cleanup surface))))

(provide 'appkit-surface-test)

;;; appkit-surface-test.el ends here
