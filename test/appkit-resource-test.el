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

(ert-deftest appkit-resource-projects-a-fifty-resource-page ()
  "A normal history page must fit within its App's resource capacity."
  (let* ((count 50)
         (loader
          (lambda (_context input success _failure)
            (funcall success (format "image-%d\n" input))
            (appkit-cancellation-create :kind 'transport :cancel #'ignore)))
         (surface-type
          (appkit-surface-type-create
           :name 'appkit-resource-page-test
           :mode #'special-mode
           :init (lambda (_context _input)
                   (appkit-next :model nil
                                :render (appkit-projection-change-create :full-p t)))
           :update (lambda (_context _model _message)
                     (appkit-next-reject 'unsupported))
           :renderer-factory
           (lambda (_surface)
             (appkit-projection-renderer-create
              :project-all
              (lambda (_surface _app _model)
                (mapcar
                 (lambda (index)
                   (appkit-projection-row-create
                    :key index :payload index :dependencies (list index)
                    :resource-demands
                    (list (appkit-resource-demand-create
                           :key index :input index :loader loader
                           :acquisition-identity (list 'page index)
                           :sharing-policy 'app-private :cache-policy 'while-interested))))
                 (number-sequence 0 (1- count))))
              :printer
              (lambda (surface _app row)
                (let ((state (appkit-resource-state surface (appkit-projection-row-key row))))
                  (insert (if (and state (eq (appkit-resource-state-status state) 'ready))
                              (appkit-resource-state-value state)
                            "pending\n"))))
              :anchor-property 'appkit-resource-page-key
              :no-separator-p t))))
         app surface buffer)
    (unwind-protect
        (progn
          (setq app (appkit-app-start appkit-resource-test--app-type
                                      :identity (make-symbol "resource-page-app")))
          (setq surface (appkit-open-generated-surface
                         surface-type :app app :identity 'page)
                buffer (appkit-surface-buffer surface))
          (appkit-loop-run-pass (appkit-app-loop app))
          (appkit-loop-run-pass (appkit-surface-loop surface))
          (with-current-buffer buffer
            (should (equal (mapconcat (lambda (index) (format "image-%d\n" index))
                                      (number-sequence 0 (1- count)) "")
                           (buffer-string)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when app (appkit-app-close app)))))

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
                     0 appkit-resource-default-entry-limit))))
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

(ert-deftest appkit-resource-retire-advances-queue-after-cancel-error ()
  (let* ((starts 0)
         (broker
          (appkit-resource--broker-create
           :acquisitions (make-hash-table :test #'equal)
           :active-count 1 :max-active 1 :queue-count 1 :max-queued 1))
         (active
          (appkit-resource--acquisition-create
           :broker broker :identity 'active :state 'active :leases nil
           :cancellation
           (appkit-cancellation-create
            :kind 'transport :cancel (lambda () (error "cancel failed")))))
         (queued
          (appkit-resource--acquisition-create
           :broker broker :identity 'queued :input 'queued :state 'new
           :queued-p t :leases nil :token 'queued-token
           :loader
           (lambda (_context _input _success _failure)
             (setq starts (1+ starts))
             (appkit-cancellation-create :kind 'transport :cancel #'ignore))))
         (queue (list queued)))
    (puthash 'active active (appkit-resource--broker-acquisitions broker))
    (puthash 'queued queued (appkit-resource--broker-acquisitions broker))
    (setf (appkit-resource--broker-queue-head broker) queue
          (appkit-resource--broker-queue-tail broker) queue)
    (should-error (appkit-resource--broker-retire active t) :type 'error)
    (should (= 1 starts))
    (should-not (gethash 'active (appkit-resource--broker-acquisitions broker)))
    (should (eq (appkit-resource--acquisition-state queued) 'active))
    (should (= 1 (appkit-resource--broker-active-count broker)))
    (should (= 0 (appkit-resource--broker-queue-count broker)))
    (appkit-resource--broker-retire queued t)))

(ert-deftest appkit-resource-uninterested-cleanup-continues-after-errors ()
  (let* ((cancellations 0)
         (broker
          (appkit-resource--broker-create
           :acquisitions (make-hash-table :test #'equal)
           :active-count 2 :max-active 2 :queue-count 0 :max-queued 1))
         (coordinator
          (appkit-resource--coordinator-create-internal
           :entries (make-hash-table :test #'equal)
           :interests (make-hash-table :test #'eq)
           :max-entries 2 :max-interests 2 :alive-p t)))
    (dotimes (key 2)
      (let* ((demand
              (appkit-resource-demand-create
               :key key :input key :loader #'ignore
               :acquisition-identity key :cache-policy 'while-interested))
             (acquisition
              (appkit-resource--acquisition-create
               :broker broker :identity key :state 'active
               :cancellation
               (appkit-cancellation-create
                :kind 'transport
                :cancel (lambda ()
                          (setq cancellations (1+ cancellations))
                          (error "cancel failed")))))
             (entry
              (appkit-resource--entry-create
               :coordinator coordinator :key key :demand demand
               :acquisition acquisition)))
        (setf (appkit-resource--acquisition-leases acquisition) (list entry))
        (puthash key acquisition (appkit-resource--broker-acquisitions broker))
        (puthash key entry (appkit-resource--coordinator-entries coordinator))))
    (cl-letf (((symbol-function 'display-warning) #'ignore))
      (should-error (appkit-resource--release-uninterested coordinator)
                    :type 'error))
    (should (= 2 cancellations))
    (should (= 0 (hash-table-count
                  (appkit-resource--coordinator-entries coordinator))))
    (should (= 0 (hash-table-count
                  (appkit-resource--broker-acquisitions broker))))
    (should (= 0 (appkit-resource--broker-active-count broker)))))

(ert-deftest appkit-resource-retired-completion-does-not-stop-current-work ()
  (let ((resolvers (make-hash-table :test #'eq)) app surface buffer)
    (let* ((loader
            (lambda (_context input success _failure)
              (puthash input
                       (lambda (value)
                         (remhash input resolvers)
                         (funcall success value))
                       resolvers)
              (appkit-cancellation-create
               :kind 'transport
               :cancel (lambda () (remhash input resolvers)))))
           (surface-type
            (appkit-surface-type-create
             :name 'appkit-resource-retired-completion-test
             :mode #'special-mode
             :init
             (lambda (_context _input)
               (appkit-next
                :model 'old
                :render (appkit-projection-change-create :full-p t)))
             :update
             (lambda (_context _model message)
               (pcase message
                 ('replace
                  (appkit-next
                   :model 'new
                   :render (appkit-projection-change-create :full-p t)))
                 (_ (appkit-next-reject 'unsupported))))
             :renderer-factory
             (lambda (_surface)
               (appkit-projection-renderer-create
                :project-all
                (lambda (_surface _app-read-view model)
                  (list
                   (appkit-projection-row-create
                    :key 'row :payload model :dependencies (list model)
                    :resource-demands
                    (list
                     (appkit-resource-demand-create
                      :key model :input model :loader loader
                      :acquisition-identity (list 'test model)
                      :sharing-policy 'app-private
                      :cache-policy 'while-interested)))))
                :printer
                (lambda (surface _app-read-view row)
                  (let ((state (appkit-resource-state
                                surface (appkit-projection-row-payload row))))
                    (insert
                     (if (and state
                              (eq (appkit-resource-state-status state) 'ready))
                         (appkit-resource-state-value state)
                       "loading"))))
                :anchor-property 'appkit-resource-test-key
                :no-separator-p t)))))
      (unwind-protect
          (progn
            (setq app (appkit-app-start
                       appkit-resource-test--app-type
                       :identity (make-symbol "resource-retirement-app")))
            (setq surface (appkit-open-generated-surface
                           surface-type :app app :identity 'resource-retirement)
                  buffer (appkit-surface-buffer surface))
            ;; The old completion already owns the wake when its interest ends.
            (funcall (gethash 'old resolvers) "retired")
            (appkit-surface-send surface 'replace)
            (funcall (gethash 'new resolvers) "current")
            (appkit-loop-run-pass (appkit-app-loop app))
            (appkit-loop-run-pass (appkit-surface-loop surface))
            (should (appkit-app-live-p app))
            (with-current-buffer buffer
              (should (equal "current" (buffer-string)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (when (appkit-app-live-p app) (appkit-app-close app))))))

(provide 'appkit-resource-test)

;;; appkit-resource-test.el ends here
