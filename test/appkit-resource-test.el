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
                   surface-type
                   :app app
                   :identity 'resource))
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
                     :key 'late
                     :row-keys '(row))))))
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
                   surface-type
                   :app app
                   :identity 'resource-cancel))
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
                     :key 'broken
                     :row-keys '(row))))))
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
                   surface-type
                   :app app
                   :identity 'resource-fault))
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
                   (appkit-next
                    :model nil
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
                    :key index
                    :payload index
                    :dependencies (list index)
                    :resource-demands
                    (list (appkit-resource-demand-create
                           :key index
                           :input index
                           :loader loader
                           :acquisition-identity (list 'page index)
                           :sharing-policy 'app-private
                           :cache-policy 'while-interested))))
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
                         surface-type
                         :app app
                         :identity 'page)
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
                       :key index
                       :input index
                       :loader loader
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
              surface-type
              :app app
              :identity 'resource-limit)
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
           :active-count 1
           :max-active 1
           :queue-count 1
           :max-queued 1))
         (active
          (appkit-resource--acquisition-create
           :broker broker
           :identity 'active
           :state 'active
           :leases nil
           :cancellation
           (appkit-cancellation-create
            :kind 'transport
            :cancel (lambda () (error "cancel failed")))))
         (queued
          (appkit-resource--acquisition-create
           :broker broker
           :identity 'queued
           :input 'queued
           :state 'new
           :queued-p t
           :leases nil
           :token 'queued-token
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
           :active-count 2
           :max-active 2
           :queue-count 0
           :max-queued 1))
         (coordinator
          (appkit-resource--coordinator-create-internal
           :entries (make-hash-table :test #'equal)
           :interests (make-hash-table :test #'eq)
           :max-entries 2
           :max-interests 2
           :alive-p t)))
    (dotimes (key 2)
      (let* ((demand
              (appkit-resource-demand-create
               :key key
               :input key
               :loader #'ignore
               :acquisition-identity key
               :cache-policy 'while-interested))
             (acquisition
              (appkit-resource--acquisition-create
               :broker broker
               :identity key
               :state 'active
               :cancellation
               (appkit-cancellation-create
                :kind 'transport
                :cancel (lambda ()
                          (setq cancellations (1+ cancellations))
                          (error "cancel failed")))))
             (entry
              (appkit-resource--entry-create
               :coordinator coordinator
               :key key
               :demand demand
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
                    :key 'row
                    :payload model
                    :dependencies (list model)
                    :resource-demands
                    (list
                     (appkit-resource-demand-create
                      :key model
                      :input model
                      :loader loader
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
                           surface-type
                           :app app
                           :identity 'resource-retirement)
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


(defun appkit-resource-test--retained-surface (loader)
  "Build a Surface whose model selects its owned resource."
  (appkit-surface-type-create
   :name 'appkit-resource-retained-test :mode #'special-mode
   :init (lambda (_context _input)
           (appkit-next :model 'one :render 'render))
   :update (lambda (_context _model message)
             (appkit-next :model message :render 'render))
   :renderer-factory
   (lambda (_surface)
     (appkit-generated-renderer-create
      :mount #'ignore :merge (lambda (_left right) right)
      :render
      (lambda (_surface _app-view model _request)
        (appkit-render-result-create
         :resource-demands
         (list (appkit-resource-demand-create
                :key 'cover :input model :loader loader
                :acquisition-identity (list 'retained model)
                :sharing-policy 'app-private :cache-policy 'while-interested))
         :resource-interest-update
         (appkit-resource-interest-update-create
          :mode 'replace
          :entries (list (appkit-resource-interest-create
                          :key 'cover :row-keys '(row))))))
      :recover nil :resource-request (lambda (_keys) 'render) :unmount #'ignore))))

(ert-deftest appkit-resource-retained-value-follows-last-interest-and-replacement ()
  (dolist (synchronous '(nil t))
    (let ((starts 0) (released 0) callbacks app surfaces)
      (let* ((value (lambda () (appkit-resource-value-create
                               :value "retained" :release (lambda () (cl-incf released)))))
             (type (appkit-resource-test--retained-surface
                    (lambda (_context _input success _failure)
                      (cl-incf starts)
                      (push success callbacks)
                      (when synchronous (funcall success (funcall value)))
                      (appkit-cancellation-create :cancel #'ignore)))))
        (unwind-protect
            (progn
              (setq app (appkit-app-start appkit-resource-test--app-type
                                         :identity (make-symbol "owned-resource")))
              (dotimes (index 2)
                (push (appkit-open-generated-surface type :app app :identity index) surfaces))
              (should (= starts 1))
              (unless synchronous (funcall (car callbacks) (funcall value)))
              (appkit-loop-run-pass (appkit-app-loop app))
              (dolist (surface surfaces) (appkit-loop-run-pass (appkit-surface-loop surface)))
              (appkit-surface-stop (car surfaces))
              (should (= released 0))
              (appkit-surface-send (cadr surfaces) 'two)
              (should (= starts 2))
              (should (= released 1))
              (unless synchronous (funcall (car callbacks) (funcall value)))
              (appkit-surface-stop (cadr surfaces))
              (should (= released 2))
              ;; A late value cannot be leaked after its final consumer left.
              (let ((late (funcall value)))
                (funcall (car callbacks) late)
                (funcall (car callbacks) late)
                (should (= released 3))))
          (dolist (surface surfaces)
            (when (appkit-surface-live-p surface) (appkit-surface-stop surface))
            (when (buffer-live-p (appkit-surface-buffer surface))
              (kill-buffer (appkit-surface-buffer surface))))
          (when app (appkit-app-close app)))))))



(ert-deftest appkit-resource-presentation-follows-the-last-visible-consumer ()
  "Repeated rows and two Surfaces share one image per size until both detach."
  (let ((starts 0) (decodes 0) released images app surfaces)
    (let* ((loader
            (lambda (_context _input success _failure)
              (cl-incf starts)
              (funcall success
                       (appkit-resource-presentation-create
                        'avatar-bytes (lambda (image) (push image released))))
              (appkit-cancellation-create :kind 'transport :cancel #'ignore)))
           (type
            (appkit-surface-type-create
             :name 'presentation-consumer :mode #'special-mode
             :init (lambda (_context _input) (appkit-next :model nil :render t))
             :update (lambda (_context model _message)
                       (appkit-next :model model :render t))
             :renderer-factory
             (lambda (_surface)
               (appkit-generated-renderer-create
                :mount #'ignore :unmount #'ignore
                :merge (lambda (_old new) new) :resource-request (lambda (_keys) t)
                :render
                (lambda (surface _view _model _change)
                  (when-let* ((state (appkit-resource-state surface 'avatar))
                              ((eq (appkit-resource-state-status state) 'ready)))
                    (dotimes (_ 10)
                      (dolist (size '(40 80))
                        (push
                         (appkit-resource-presentation-get
                          (appkit-resource-state-value state) size
                          (lambda (source)
                            (cl-incf decodes)
                            (list source size)))
                         images))))
                  (appkit-resource-rows-result
                   (list (list 'row
                               (appkit-resource-demand-create
                                :key 'avatar :input 'avatar :loader loader
                                :acquisition-identity 'avatar :sharing-policy 'app-private
                                :cache-policy 'while-interested)))
                   '(row))))))))
      (unwind-protect
          (progn
            (setq app (appkit-app-start appkit-resource-test--app-type
                                       :identity (make-symbol "presentation")))
            (dotimes (index 2)
              (push (appkit-open-generated-surface type :app app :identity index) surfaces))
            (appkit-loop-run-pass (appkit-app-loop app))
            (dolist (surface surfaces) (appkit-loop-run-pass (appkit-surface-loop surface)))
            (should (= starts 1))
            (should (= decodes 2))
            (should (= (length (delete-dups (copy-sequence images))) 2))
            (appkit-surface-stop (car surfaces))
            (should-not released)
            (appkit-surface-stop (cadr surfaces))
            (should (= (length released) 2)))
        (dolist (surface surfaces)
          (when (buffer-live-p (appkit-surface-buffer surface))
            (kill-buffer (appkit-surface-buffer surface))))
        (when app (appkit-app-close app))))))

(ert-deftest appkit-resource-presentation-retries-unavailable-decoders ()
  (let* ((wrapped (appkit-resource-presentation-create 'source #'ignore))
         (value (appkit-resource-value-value wrapped))
         (calls 0)
         (decode (lambda (_source) (when (> (cl-incf calls) 1) (list 'image)))))
    (should-not (appkit-resource-presentation-get value 40 decode))
    (let ((image (appkit-resource-presentation-get value 40 decode)))
      (should image)
      (should (eq image (appkit-resource-presentation-get value 40 decode)))
      (should (= calls 2)))))

(ert-deftest appkit-resource-rows-share-one-visible-budget ()
  (let* ((demand (lambda (key) (appkit-resource-demand-create :key key)))
         (base (appkit-render-result-create
                :resource-demands (list (funcall demand 'lease))
                :resource-interest-update
                (appkit-resource-interest-update-create
                 :mode 'replace
                 :entries (list (appkit-resource-interest-create :key 'lease :row-keys '(one))))))
         (result (appkit-resource-rows-result
                  (list (list 'hidden (funcall demand 'hidden))
                        (list 'one (funcall demand 'avatar) (funcall demand 'avatar))
                        (list 'two (funcall demand 'avatar) (funcall demand 'overflow)))
                  '(one two) :budget 2 :result base))
         (interests (appkit-resource-interest-update-entries
                     (appkit-render-result-resource-interest-update result))))
    (should (equal '(lease avatar)
                   (mapcar #'appkit-resource-demand-key (appkit-render-result-resource-demands result))))
    (should (= 2 (length interests)))
    (should (equal '(one two)
                   (appkit-resource-interest-row-keys
                    (seq-find (lambda (interest) (eq (appkit-resource-interest-key interest) 'avatar)) interests))))))
(provide 'appkit-resource-test)

;;; appkit-resource-test.el ends here
