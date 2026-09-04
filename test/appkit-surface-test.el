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

(defmacro appkit-surface-test--with-surface (binding &rest body)
  "Open the Surface described by BINDING and evaluate BODY with cleanup."
  (declare (indent 1) (debug ((symbolp form &rest form) body)))
  (let ((surface (car binding))
        (type (cadr binding))
        (arguments (cddr binding)))
    `(let (,surface)
       (unwind-protect
           (progn
             (setq ,surface
                   (appkit-open-generated-surface ,type ,@arguments))
             ,@body)
         (appkit-surface-test--cleanup ,surface)))))

(cl-defun appkit-surface-test--effect
    (key record &key synchronous label cancel-error)
  "Create a test Effect under KEY reporting lifecycle through RECORD.

SYNCHRONOUS resolves during startup.  LABEL distinguishes instances sharing
KEY.  CANCEL-ERROR makes physical cleanup fail after recording the attempt."
  (let ((name (or label key)))
    (appkit-effect-create
     :key key
     :input key
     :start
     (lambda (_context _input _observe resolve _reject)
       (funcall record (list 'start name))
       (if synchronous
           (progn
             (funcall resolve 'ready)
             nil)
         (appkit-cancellation-create
          :kind 'logical
          :cancel
          (lambda ()
            (funcall record (list 'cancel name))
            (when cancel-error
              (error "%s cancellation failed" name))))))
     :success (lambda (owned value) (list 'effect-done owned value))
     :failure (lambda (owned reason) (list 'effect-failed owned reason)))))

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
                    (appkit-next :model input :render 'initial))
                  :update
                  (lambda (_surface model _message)
                    (appkit-next :model model :render appkit-render-none))
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
                          (appkit-next :model nil :render appkit-render-none))
                  :update
                  (lambda (_surface model message)
                    (appkit-next
                     :model (append model (list message))
                     :render (list message)))
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
                          (appkit-next :model 'initial :render appkit-render-none))
                  :update
                  (lambda (_surface _model _message)
                    (appkit-next :model 'changed :render appkit-render-none))
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
                          (appkit-next :model 'initial :render appkit-render-none))
                  :update
                  (lambda (_surface _model message)
                    (appkit-next :model message :render 'render))
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
                          (appkit-next :model 'initial :render appkit-render-none))
                  :update
                  (lambda (_surface _model message)
                    (appkit-next :model message :render 'render))
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
                          (appkit-next :model 'initial :render appkit-render-none))
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
               (appkit-next :model nil :render appkit-render-none))
       :update (lambda (_surface model _message)
                 (appkit-next :model model :render appkit-render-none))
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
                    (appkit-next :model nil :render appkit-render-none))
            :update (lambda (_surface model _message)
                      (appkit-next :model model :render appkit-render-none))
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
                          (appkit-next :model nil :render appkit-render-none))
                  :update
                  (lambda (_surface model message)
                    (appkit-next :model (cons message model) :render 'full))
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


(ert-deftest appkit-surface-starts-initial-effects-after-render ()
  (let* (events
         (record (lambda (event) (push event events)))
         (effect (appkit-surface-test--effect 'initial record)))
    (appkit-surface-test--with-surface
     (surface
      (appkit-surface-test--type
       :init
       (lambda (_surface _input)
         (appkit-next
          :model 'ready
          :render 'initial
          :commands (list (appkit-command-start-effect effect))))
       :update
       (lambda (_surface model _message)
         (appkit-next :model model :render appkit-render-none))
       :renderer-factory
       (lambda (_surface)
         (appkit-surface-test--renderer record))))
     (should (equal (nreverse events)
                    '((mount ready) (render ready initial)
                      (start initial))))
     (should (= (appkit-effect-runtime-count
                 (appkit-surface-effect-runtime surface))
                1)))))

(ert-deftest appkit-surface-folds-effect-commands-before-startup ()
  (let* (events
         (record (lambda (event) (push event events)))
         (old-a (appkit-surface-test--effect 'a record :label 'old-a))
         (effect-b (appkit-surface-test--effect 'b record :label 'b))
         (new-a (appkit-surface-test--effect 'a record :label 'new-a)))
    (appkit-surface-test--with-surface
     (surface
      (appkit-surface-test--type
       :init
       (lambda (_surface _input)
         (appkit-next :model nil :render appkit-render-none))
       :update (lambda (_surface _model next) next)
       :renderer-factory
       (lambda (_surface)
         (appkit-surface-test--renderer record))))
     (setq events nil)
     (dolist (next
              (list
               (appkit-next
                :model 'first :render '(first)
                :commands (list (appkit-command-start-effect old-a)))
               (appkit-next
                :model 'second :render '(second)
                :commands (list (appkit-command-start-effect effect-b)))
               (appkit-next
                :model 'third :render '(third)
                :commands (list (appkit-command-start-effect new-a)))))
       (appkit-surface-post surface next))
     (should (= (appkit-loop-run-pass (appkit-surface-loop surface)) 3))
     (should (equal (appkit-surface-model surface) 'third))
     (should (equal (nreverse events)
                    '((render third (first second third))
                      (start b) (start new-a))))
     (should (= (appkit-effect-runtime-count
                 (appkit-surface-effect-runtime surface))
                2)))))

(ert-deftest appkit-surface-defers-synchronous-effect-result ()
  (let* (events
         (record (lambda (event) (push event events)))
         (effect
          (appkit-surface-test--effect 'sync record :synchronous t)))
    (appkit-surface-test--with-surface
     (surface
      (appkit-surface-test--type
       :init
       (lambda (_surface _input)
         (appkit-next :model 'idle :render appkit-render-none))
       :update
       (lambda (_surface _model message)
         (pcase message
           ('begin
            (appkit-next
             :model 'starting
             :render 'starting
             :commands (list (appkit-command-start-effect effect))))
           (`(effect-done sync ready)
            (appkit-next :model 'done :render 'done))))
       :renderer-factory
       (lambda (_surface)
         (appkit-surface-test--renderer record))))
     (setq events nil)
     (let ((ticket (appkit-surface-send surface 'begin)))
       (should (eq (appkit-loop-ticket-state ticket) 'accepted)))
     (should (eq (appkit-surface-model surface) 'starting))
     (should (equal (nreverse events)
                    '((render starting starting) (start sync))))
     (should (= (appkit-loop-pending-count
                 (appkit-surface-loop surface))
                1))
     (setq events nil)
     (should (= (appkit-loop-run-pass (appkit-surface-loop surface)) 1))
     (should (eq (appkit-surface-model surface) 'done))
     (should (equal (nreverse events) '((render done done)))))))

(ert-deftest appkit-surface-cancels-effect-before-render ()
  (let* (events
         (record (lambda (event) (push event events)))
         (effect (appkit-surface-test--effect 'owned record)))
    (appkit-surface-test--with-surface
     (surface
      (appkit-surface-test--type
       :init
       (lambda (_surface _input)
         (appkit-next :model 'idle :render appkit-render-none))
       :update
       (lambda (_surface _model message)
         (pcase message
           ('start
            (appkit-next
             :model 'active :render 'active
             :commands (list (appkit-command-start-effect effect))))
           ('cancel
            (appkit-next
             :model 'cancelled :render 'cancelled
             :commands (list (appkit-command-cancel-effect 'owned))))))
       :renderer-factory
       (lambda (_surface)
         (appkit-surface-test--renderer record))))
     (appkit-surface-send surface 'start)
     (setq events nil)
     (appkit-surface-send surface 'cancel)
     (should (equal (nreverse events)
                    '((cancel owned) (render cancelled cancelled))))
     (should (= (appkit-effect-runtime-count
                 (appkit-surface-effect-runtime surface))
                0)))))

(ert-deftest appkit-surface-fault-revokes-active-effects ()
  (let* (events
         (record (lambda (event) (push event events)))
         (effect (appkit-surface-test--effect 'owned record)))
    (appkit-surface-test--with-surface
     (surface
      (appkit-surface-test--type
       :init
       (lambda (_surface _input)
         (appkit-next :model 'idle :render appkit-render-none))
       :update
       (lambda (_surface model message)
         (if (eq message 'start)
             (appkit-next
              :model 'active :render appkit-render-none
              :commands (list (appkit-command-start-effect effect)))
           (error "broken update from %S" model)))
       :renderer-factory
       (lambda (_surface)
         (appkit-surface-test--renderer record))))
     (appkit-surface-send surface 'start)
     (setq events nil)
     (should-error (appkit-surface-send surface 'boom) :type 'error)
     (should (eq (appkit-surface-status surface) 'faulted))
     (should (equal events '((cancel owned))))
     (should (= (appkit-effect-runtime-count
                 (appkit-surface-effect-runtime surface))
                0)))))


(ert-deftest appkit-surface-cancellation-failure-does-not-skip-cleanup ()
  (let* (events
         (record (lambda (event) (push event events)))
         (effect-a
          (appkit-surface-test--effect
           'a record :cancel-error t))
         (effect-b (appkit-surface-test--effect 'b record)))
    (appkit-surface-test--with-surface
     (surface
      (appkit-surface-test--type
       :init
       (lambda (_surface _input)
         (appkit-next
          :model 'active
          :render appkit-render-none
          :commands
          (list (appkit-command-start-effect effect-a)
                (appkit-command-start-effect effect-b))))
       :update
       (lambda (_surface _model _message)
         (appkit-next
          :model 'cancelled
          :render 'must-not-render
          :commands
          (list (appkit-command-cancel-effect 'a)
                (appkit-command-cancel-effect 'b))))
       :renderer-factory
       (lambda (_surface)
         (appkit-surface-test--renderer record))))
     (setq events nil)
     (should-error (appkit-surface-send surface 'cancel) :type 'error)
     (should (eq (appkit-surface-status surface) 'faulted))
     (should (equal (nreverse events) '((cancel a) (cancel b))))
     (should (= (appkit-effect-runtime-count
                 (appkit-surface-effect-runtime surface))
                0)))))
(ert-deftest appkit-surface-discards-folded-work-on-command-overflow ()
  (let ((events nil))
    (appkit-surface-test--with-surface
     (surface
      (appkit-surface-test--type
       :init
       (lambda (_surface _input)
         (appkit-next :model nil :render appkit-render-none))
       :update
       (lambda (_surface _model key)
         (appkit-next
          :model key
          :render key
          :commands (list (appkit-command-cancel-effect key))))
       :renderer-factory
       (lambda (_surface)
         (appkit-surface-test--renderer
          (lambda (event) (push event events)))))
      :folded-command-limit 1)
     (appkit-surface-post surface 'first)
     (appkit-surface-post surface 'second)
     (should (= (appkit-loop-run-pass (appkit-surface-loop surface)) 2))
     (should (eq (appkit-surface-status surface) 'faulted))
     (should (eq (appkit-surface-model surface) 'first))
     (should (equal events '((mount nil)))))))
(provide 'appkit-surface-test)

;;; appkit-surface-test.el ends here
