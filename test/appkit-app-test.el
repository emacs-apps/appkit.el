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

(provide 'appkit-app-test)

;;; appkit-app-test.el ends here
