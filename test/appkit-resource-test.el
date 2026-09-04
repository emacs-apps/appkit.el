;;; appkit-resource-test.el --- Resource companion tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'appkit-app)
(require 'appkit-command)
(require 'appkit-projection)
(require 'appkit-resource)
(require 'appkit-surface)

(defconst appkit-resource-test--app-type
  (appkit-app-type-create
   :name 'appkit-resource-test
   :init (lambda (_context _input)
           (appkit-next :model nil :render appkit-render-none))
   :update (lambda (_context model _message)
             (appkit-next :model model :render appkit-render-none))
   :shutdown #'ignore))

(ert-deftest appkit-resource-completion-redraws-without-domain-revision ()
  (let (resolve cancel app surface buffer projects prints (starts 0))
    (let* ((loader
            (lambda (_context _input success _failure)
              (setq starts (1+ starts)
                    resolve success)
              (appkit-cancellation-create
               :kind 'transport
               :cancel (lambda () (setq cancel t)))))
           (surface-type
            (appkit-surface-type-create
            :name 'appkit-resource-test
            :mode #'special-mode
            :init
            (lambda (_context _input)
              (appkit-next
               :model 'resource-model
               :render (appkit-projection-change-create :full-p t)))
            :update
            (lambda (_context model message)
              (pcase message
                ('refresh
                 (appkit-next
                  :model model
                  :render (appkit-projection-change-create :full-p t)))
                (_ (appkit-next-reject 'unsupported))))
            :renderer-factory
            (lambda (_surface)
              (appkit-projection-renderer-create
               :project-all
               (lambda (_surface _app-read-view model)
                 (setq projects (1+ (or projects 0)))
                 (list
                  (appkit-projection-row-create
                   :key 'row
                   :payload model
                   :dependencies '(cover)
                   :resource-demands
                   (list
                    (appkit-resource-demand-create
                     :key 'cover
                     :input 'cover-input
                     :loader loader
                     :acquisition-identity '(test cover)
                     :sharing-policy 'app-private
                     :cache-policy 'while-interested)))))
               :printer
               (lambda (surface _app-read-view row)
                 (setq prints (1+ (or prints 0)))
                 (let ((state (appkit-resource-state surface 'cover)))
                   (insert
                    (if (and state
                             (eq (appkit-resource-state-status state) 'ready))
                        (appkit-resource-state-value state)
                      (format "%s" (appkit-projection-row-payload row))))))
               :anchor-property 'appkit-resource-test-key
               :no-separator-p t)))))
      (unwind-protect
          (progn
            (setq app
                  (appkit-app-start
                   appkit-resource-test--app-type
                   :identity (make-symbol "resource-app")))
            (setq surface
                  (appkit-open-generated-surface
                   surface-type :app app :identity 'resource))
            (setq buffer (appkit-surface-buffer surface))
            (should (functionp resolve))
            (should (= 1 projects))
            (should (= 1 prints))
            (should (= 0 (appkit-loop-revision (appkit-surface-loop surface))))
            (funcall resolve "ready-bytes")
            (should (= 1 (appkit-loop-run-pass (appkit-app-loop app))))
            (should (= 1 (appkit-loop-run-pass (appkit-surface-loop surface))))
            (should (= 1 projects))
            (should (= 2 prints))
            (should (= 0 (appkit-loop-revision (appkit-surface-loop surface))))
            (with-current-buffer buffer
              (should (equal "ready-bytes" (buffer-string))))
            (appkit-surface-send surface 'refresh)
            (should (= 1 starts))
            (should (= 2 projects))
            (should (= 2 prints))
            (appkit-surface-stop surface)
            (should-not cancel))
        (when (and buffer (buffer-live-p buffer)) (kill-buffer buffer))
        (when (and app (appkit-app-p app)
                   (not (eq (appkit-app-status app) 'stopped)))
          (appkit-app-close app))))))


(ert-deftest appkit-resource-detach-cancels-work-and-fences-late-output ()
  (let (resolve cancelled app surface buffer)
    (let ((surface-type
           (appkit-surface-type-create
            :name 'appkit-resource-cancellation-test
            :mode #'special-mode
            :init
            (lambda (_context _input)
              (appkit-next :model nil :render 'render))
            :update
            (lambda (_context _model _message)
              (appkit-next-reject 'unsupported))
            :renderer-factory
            (lambda (_surface)
              (appkit-generated-renderer-create
               :mount #'ignore
               :merge (lambda (_left right) right)
               :render
               (lambda (_surface _app-read-view _model _request)
                 (appkit-render-result-create
                  :resource-demands
                  (list
                   (appkit-resource-demand-create
                    :key 'late
                    :input 'late-input
                    :loader
                    (lambda (_context _input success _failure)
                      (setq resolve success)
                      (appkit-cancellation-create
                       :kind 'transport
                       :cancel (lambda () (setq cancelled t))))
                    :acquisition-identity '(test late)
                    :sharing-policy 'app-private
                    :cache-policy 'while-interested))
                  :resource-interest-update
                  (appkit-resource-interest-update-create
                   :mode 'replace
                   :entries
                   (list
                    (appkit-resource-interest-create
                     :key 'late :row-keys '(row))))))
               :recover nil
               :resource-request (lambda (_keys) 'render)
               :unmount #'ignore)))))
      (unwind-protect
          (progn
            (setq app
                  (appkit-app-start
                   appkit-resource-test--app-type
                   :identity (make-symbol "resource-cancel-app")))
            (setq surface
                  (appkit-open-generated-surface
                   surface-type :app app :identity 'resource-cancel))
            (setq buffer (appkit-surface-buffer surface))
            (should (functionp resolve))
            (appkit-surface-stop surface)
            (should cancelled)
            (should-not (funcall resolve "too-late"))
            (should (= 0 (appkit-loop-run-pass (appkit-app-loop app))))
            (should (eq (appkit-app-status app) 'running)))
        (when (and buffer (buffer-live-p buffer)) (kill-buffer buffer))
        (when (and app (appkit-app-p app)
                   (not (eq (appkit-app-status app) 'stopped)))
          (appkit-app-close app))))))

(ert-deftest appkit-resource-loader-signal-faults-owning-app ()
  (let (app surface buffer)
    (let ((surface-type
           (appkit-surface-type-create
            :name 'appkit-resource-fault-test
            :mode #'special-mode
            :init
            (lambda (_context _input)
              (appkit-next :model nil :render 'render))
            :update
            (lambda (_context _model _message)
              (appkit-next-reject 'unsupported))
            :renderer-factory
            (lambda (_surface)
              (appkit-generated-renderer-create
               :mount #'ignore
               :merge (lambda (_left right) right)
               :render
               (lambda (_surface _app-read-view _model _request)
                 (appkit-render-result-create
                  :resource-demands
                  (list
                   (appkit-resource-demand-create
                    :key 'broken
                    :input 'broken-input
                    :loader
                    (lambda (&rest _arguments)
                      (error "loader invariant broke"))
                    :acquisition-identity '(test broken)
                    :sharing-policy 'app-private
                    :cache-policy 'while-interested))
                  :resource-interest-update
                  (appkit-resource-interest-update-create
                   :mode 'replace
                   :entries
                   (list
                    (appkit-resource-interest-create
                     :key 'broken :row-keys '(row))))))
               :recover nil
               :resource-request (lambda (_keys) 'render)
               :unmount #'ignore)))))
      (unwind-protect
          (progn
            (setq app
                  (appkit-app-start
                   appkit-resource-test--app-type
                   :identity (make-symbol "resource-fault-app")))
            (setq surface
                  (appkit-open-generated-surface
                   surface-type :app app :identity 'resource-fault))
            (setq buffer (appkit-surface-buffer surface))
            (should (eq (appkit-app-status app) 'running))
            (should (= 1 (appkit-loop-run-pass (appkit-app-loop app))))
            (should (eq (appkit-app-status app) 'faulted))
            (should (eq (appkit-surface-status surface) 'faulted))
            (should (= 0 (appkit-loop-revision (appkit-app-loop app)))))
        (when (and buffer (buffer-live-p buffer)) (kill-buffer buffer))
        (when (and app (appkit-app-p app)
                   (not (eq (appkit-app-status app) 'stopped)))
          (appkit-app-close app))))))

(ert-deftest appkit-resource-limit-faults-before-starting-any-loader ()
  (let ((starts 0) app)
    (let* ((loader
           (lambda (_context _input _success _failure)
             (setq starts (1+ starts))
             (appkit-cancellation-create :kind 'transport :cancel #'ignore)))
          (surface-type
           (appkit-surface-type-create
            :name 'appkit-resource-limit-test
            :mode #'special-mode
            :init
            (lambda (_context _input)
              (appkit-next :model nil :render 'render))
            :update
            (lambda (_context _model _message)
              (appkit-next-reject 'unsupported))
            :renderer-factory
            (lambda (_surface)
              (appkit-generated-renderer-create
               :mount #'ignore
               :merge (lambda (_left right) right)
               :render
               (lambda (_surface _app-read-view _model _request)
                 (appkit-render-result-create
                  :resource-demands
                  (mapcar
                   (lambda (index)
                     (appkit-resource-demand-create
                      :key index :input index :loader loader
                      :acquisition-identity (list 'limit index)
                      :sharing-policy 'app-private
                      :cache-policy 'while-interested))
                   (number-sequence
                    0 appkit-resource-default-per-render-limit))))
               :recover nil
               :resource-request nil
               :unmount #'ignore)))))
      (unwind-protect
          (progn
            (setq app
                  (appkit-app-start
                   appkit-resource-test--app-type
                   :identity (make-symbol "resource-limit-app")))
            (should-error
             (appkit-open-generated-surface
              surface-type :app app :identity 'resource-limit)
             :type 'error)
            (should (= 0 starts))
            (should (= 0 (appkit-app-surface-count app)))
            (should (eq (appkit-app-status app) 'running)))
        (when (and app (appkit-app-p app)
                   (not (eq (appkit-app-status app) 'stopped)))
          (appkit-app-close app))))))
(provide 'appkit-resource-test)

;;; appkit-resource-test.el ends here
