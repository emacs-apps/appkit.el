;;; appkit-app-test.el --- Tests for canonical App runtimes -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'appkit-app)
(require 'appkit-surface)

(ert-deftest appkit-app-runs-contextual-ui-free-transitions ()
  (let (contexts shutdown-app app)
    (unwind-protect
        (progn
          (setq app
                (appkit-app-start
                 (appkit-app-type-create
                  :name 'canonical-test
                  :init
                  (lambda (context input)
                    (push context contexts)
                    (appkit-next
                     :model input :render appkit-render-none))
                  :update
                  (lambda (context model message)
                    (push context contexts)
                    (appkit-next
                     :model (+ model message) :render appkit-render-none))
                  :shutdown (lambda (current) (setq shutdown-app current)))
                 :input 2))
          (let ((ticket (appkit-app-send app 3)))
            (should (eq (appkit-loop-ticket-state ticket) 'accepted)))
          (should (= (appkit-app-model app) 5))
          (should (= (length contexts) 2))
          (dolist (context contexts)
            (should (appkit-transition-context-p context))
            (should
             (appkit-runtime-address-p
              (appkit-transition-context-owner-address context)))
            (should-not (appkit-transition-context-parent-address context))
            (should-not (appkit-transition-context-app-read-view context)))
          (should (appkit-app-close app))
          (should (eq shutdown-app app))
          (should-not (appkit-app-close app)))
      (when (and app (not (eq (appkit-app-status app) 'stopped)))
        (appkit-app-close app)))))

(ert-deftest appkit-surface-pass-shares-one-app-read-view ()
  (let (app surface buffer init-context init-view mount-view update-views
            render-view)
    (unwind-protect
        (progn
          (setq app
                (appkit-app-start
                 (appkit-app-type-create
                  :name 'parent-test
                  :init
                  (lambda (_context input)
                    (appkit-next
                     :model input :render appkit-render-none))
                  :update
                  (lambda (_context model _message)
                    (appkit-next
                     :model model :render appkit-render-none)))
                 :input 'canonical))
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-type-create
                  :name 'child-test
                  :mode #'special-mode
                  :init
                  (lambda (context _input)
                    (setq init-context context
                          init-view
                          (appkit-transition-context-app-read-view context))
                    (appkit-next :model nil :render 'initial))
                  :update
                  (lambda (context model message)
                    (push (appkit-transition-context-app-read-view context)
                          update-views)
                    (appkit-next
                     :model (append model (list message))
                     :render message))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-generated-renderer-create
                     :mount
                     (lambda (_surface view _model) (setq mount-view view))
                     :merge (lambda (left right) (list left right))
                     :render
                     (lambda (_surface view _model _request)
                       (setq render-view view))
                     :recover (lambda (&rest _arguments))
                     :unmount (lambda (_surface)))))
                 :app app :identity 'primary))
          (setq buffer (appkit-surface-buffer surface))
          (should (= (appkit-app-surface-count app) 1))
          (should
           (appkit-runtime-address-p
            (appkit-transition-context-parent-address init-context)))
          (should (eq init-view mount-view))
          (should (eq (appkit-app-read-view-model init-view) 'canonical))
          (appkit-surface-post surface 'first)
          (appkit-surface-post surface 'second)
          (should (= (appkit-loop-run-pass (appkit-surface-loop surface)) 2))
          (should (= (length update-views) 2))
          (should (eq (car update-views) (cadr update-views)))
          (should (eq render-view (car update-views)))
          (should (eq (appkit-app-read-view-model render-view) 'canonical))
          (should (appkit-app-close app))
          (should (eq (appkit-surface-status surface) 'stopped))
          (should (= (appkit-app-surface-count app) 0)))
      (when (and app (not (eq (appkit-app-status app) 'stopped)))
        (appkit-app-close app))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest appkit-app-validates-ui-free-next-before-staging ()
  (let (app
        (staged-next-count 0))
    (unwind-protect
        (progn
          (setq app
                (appkit-app-start
                 (appkit-app-type-create
                  :name 'validate-before-stage
                  :init
                  (lambda (_context input)
                    (appkit-next
                     :model input :render appkit-render-none))
                  :update
                  (lambda (_context _model message)
                    (appkit-next
                     :model message
                     :render 'forbidden-ui
                     :commands
                     (list
                      (appkit-command-cancel-effect 'must-not-stage)))))
                 :input 'committed))
          (let ((original-batch-add
                 (symbol-function 'appkit-command--batch-add)))
            (cl-letf (((symbol-function 'appkit-command--batch-add)
                       (lambda (&rest arguments)
                         (setq staged-next-count (1+ staged-next-count))
                         (apply original-batch-add arguments))))
              (should-error (appkit-app-send app 'invalid)
                            :type 'error)))
          (should (= staged-next-count 0))
          (should (eq (appkit-app-model app) 'committed))
          (should (eq (appkit-app-status app) 'faulted)))
      (when (and app
                 (not (eq (appkit-app-status app) 'stopped)))
        (appkit-app-close app)))))

(ert-deftest appkit-app-startup-failure-never-becomes-live ()
  (let (failed-app
        (shutdown-count 0))
    (should-error
     (appkit-app-start
      (appkit-app-type-create
       :name 'failed-startup
       :init
       (lambda (_context input)
         (appkit-next
          :model input
          :render appkit-render-none
          :commands
          (list
           (appkit-command-start-effect
            (appkit-effect-create
             :key 'initial
             :input nil
             :start
             (lambda (&rest _arguments)
               (error "initial Effect failed"))
             :success (lambda (&rest _arguments) nil)
             :failure (lambda (&rest _arguments) nil))))))
       :update
       (lambda (_context model _message)
         (appkit-next :model model :render appkit-render-none))
       :shutdown
       (lambda (app)
         (setq failed-app app
               shutdown-count (1+ shutdown-count))))
      :input 'staged)
     :type 'error)
    (should (appkit-app-p failed-app))
    (should-not (appkit-app-live-p failed-app))
    (should (eq (appkit-app-status failed-app) 'stopped))
    (should (= shutdown-count 1))))

(ert-deftest appkit-app-close-owns-reentrant-surface-cleanup ()
  (let (app surface buffer app-address surface-address
            app-post-outcome surface-post-outcome
            (shutdown-count 0)
            (unmount-count 0))
    (unwind-protect
        (progn
          (setq app
                (appkit-app-start
                 (appkit-app-type-create
                  :name 'reentrant-close
                  :init
                  (lambda (_context input)
                    (appkit-next
                     :model input :render appkit-render-none))
                  :update
                  (lambda (_context model _message)
                    (appkit-next
                     :model model :render appkit-render-none))
                  :shutdown
                  (lambda (_app)
                    (setq shutdown-count (1+ shutdown-count)
                          app-post-outcome
                          (appkit-routing--post app-address 'too-late))))
                 :input nil)
                app-address
                (appkit-routing--address (appkit-app-loop app)))
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-type-create
                  :name 'reentrant-close-surface
                  :mode #'special-mode
                  :init
                  (lambda (_context input)
                    (appkit-next
                     :model input :render appkit-render-none))
                  :update
                  (lambda (_context model _message)
                    (appkit-next
                     :model model :render appkit-render-none))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-generated-renderer-create
                     :mount (lambda (&rest _arguments))
                     :merge (lambda (_left right) right)
                     :render (lambda (&rest _arguments))
                     :recover (lambda (&rest _arguments))
                     :unmount
                     (lambda (_surface)
                       (setq unmount-count (1+ unmount-count)
                             surface-post-outcome
                             (appkit-routing--post
                              surface-address 'too-late))
                       (appkit-app-close app)
                       (error "unmount cleanup failed")))))
                 :app app :identity 'owned)
                buffer (appkit-surface-buffer surface)
                surface-address
                (appkit-routing--address (appkit-surface-loop surface)))
          (should-error (appkit-app-close app) :type 'error)
          (should (= shutdown-count 1))
          (should (= unmount-count 1))
          (should (eq app-post-outcome 'stopped))
          (should (eq surface-post-outcome 'stopped))
          (should (eq (appkit-app-status app) 'stopped))
          (should (eq (appkit-surface-status surface) 'stopped))
          (should (= (appkit-app-surface-count app) 0))
          (should (buffer-live-p buffer))
          (with-current-buffer buffer
            (should-not (appkit-current-surface))))
      (when (and app
                 (not (eq (appkit-app-status app) 'stopped)))
        (condition-case nil
            (appkit-app-close app)
          ((error quit) nil)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest appkit-app-fault-discards-whole-pass-deferred-work ()
  (let* ((target
          (appkit-loop-create
           :model nil
           :update
           (lambda (model message)
             (appkit-loop-accept (cons message model)))))
         (target-address (appkit-routing--address target))
         app)
    (unwind-protect
        (progn
          (setq app
                (appkit-app-start
                 (appkit-app-type-create
                  :name 'faulted-pass
                  :init
                  (lambda (_context _input)
                    (appkit-next
                     :model nil :render appkit-render-none))
                  :update
                  (lambda (_context model message)
                    (pcase message
                      ('commit
                       (appkit-next
                        :model (cons 'committed model)
                        :render appkit-render-none
                        :commands
                        (list
                         (appkit-command-post-message
                          :target target-address
                          :message 'must-not-escape
                          :delivery 'report))))
                      ('fault
                       (error "fault after accepted transition")))))))
          (appkit-app-post app 'commit)
          (appkit-app-post app 'fault)
          (should (= (appkit-loop-run-pass (appkit-app-loop app)) 2))
          (should (eq (appkit-app-status app) 'faulted))
          (should (= (appkit-loop-revision (appkit-app-loop app)) 1))
          (should (equal (appkit-app-model app) '(committed)))
          (should (zerop (appkit-loop-pending-count target))))
      (when (and app (not (eq (appkit-app-status app) 'stopped)))
        (appkit-app-close app))
      (appkit-loop-stop target))))

(ert-deftest appkit-app-fault-immediately-fences-owned-surfaces ()
  (let (app surface resolve)
    (unwind-protect
        (progn
          (setq app
                (appkit-app-start
                 (appkit-app-type-create
                  :name 'fault-parent
                  :init
                  (lambda (_context input)
                    (appkit-next
                     :model input :render appkit-render-none))
                  :update
                  (lambda (&rest _arguments)
                    (error "parent transition fault")))
                 :input nil))
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-type-create
                  :name 'fault-child
                  :mode #'special-mode
                  :init
                  (lambda (_context input)
                    (appkit-next
                     :model input
                     :render appkit-render-none
                     :commands
                     (list
                      (appkit-command-start-effect
                       (appkit-effect-create
                        :key 'child-work
                        :input nil
                        :start
                        (lambda (_context _input _observe
                                 resolve-gate _reject)
                          (setq resolve resolve-gate)
                          (appkit-cancellation-create :kind 'logical))
                        :success (lambda (&rest _) 'late)
                        :failure (lambda (&rest _) 'failed))))))
                  :update
                  (lambda (_context model _message)
                    (appkit-next
                     :model model :render appkit-render-none))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-generated-renderer-create
                     :mount (lambda (&rest _arguments))
                     :merge (lambda (_left right) right)
                     :render (lambda (&rest _arguments))
                     :recover nil
                     :unmount (lambda (&rest _arguments)))))
                 :app app :identity 'fault-child))
          (appkit-app-post app 'fault)
          (appkit-loop-run-pass (appkit-app-loop app))
          (should (eq (appkit-app-status app) 'faulted))
          (should (eq (appkit-surface-status surface) 'faulted))
          (should-not (appkit-app-live-p app))
          (should-not (appkit-surface-live-p surface))
          (should-not (funcall resolve 'too-late))
          (should (= (appkit-app-surface-count app) 1))
          (with-current-buffer (appkit-surface-buffer surface)
            (should (eq (appkit-current-surface) surface))))
      (when (and app (not (eq (appkit-app-status app) 'stopped)))
        (appkit-app-close app))
      (when (and surface
                 (buffer-live-p (appkit-surface-buffer surface)))
        (kill-buffer (appkit-surface-buffer surface))))))

(provide 'appkit-app-test)

;;; appkit-app-test.el ends here
