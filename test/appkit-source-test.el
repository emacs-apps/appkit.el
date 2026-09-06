;;; appkit-source-test.el --- Declarative Source tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'appkit-app)
(require 'appkit-command)
(require 'appkit-surface)
(require 'appkit-source)

(defun appkit-source-test--tagged-event (input &rest payload)
  "Map tagged Source INPUT and PAYLOAD to a domain message."
  (list 'source-event (cons input payload)))

(defun appkit-source-test--event-message (_input value)
  "Map test Source VALUE to a domain message."
  (list 'source-event value))

(defun appkit-source-test--closed-message (_input &rest reason)
  "Map test Source REASON to a domain message."
  (list 'source-closed reason))

(defun appkit-source-test--intent-message (payload outcome)
  "Map outbound PAYLOAD and OUTCOME to a domain message."
  (list 'source-intent-result payload outcome))

(defun appkit-source-test--append (model value)
  "Return MODEL with VALUE appended to its event log."
  (let ((copy (copy-sequence model)))
    (plist-put copy :events
               (append (plist-get model :events) (list value)))))

(defun appkit-source-test--update (_context model message)
  "Reduce Source test MESSAGE into MODEL."
  (pcase message
    (`(source-event ,value)
     (appkit-next
      :model (appkit-source-test--append model (list 'event value))
      :render appkit-render-none))
    (`(source-closed ,reason)
     (appkit-next
      :model (appkit-source-test--append model (list 'closed reason))
      :render appkit-render-none))
    (`(source-intent-result ,payload ,outcome)
     (appkit-next
      :model (appkit-source-test--append
              model (list 'outbound payload outcome))
      :render appkit-render-none))
    (`(identity ,identity)
     (let ((copy (copy-sequence model)))
       (appkit-next
        :model (plist-put copy :identity identity)
        :render appkit-render-none)))
    (`(send ,identity ,payload)
     (appkit-next
      :model model :render appkit-render-none
      :commands
      (list
       (appkit-command-source-intent
        :key 'stream :expected-identity identity :payload payload
        :result-mapper #'appkit-source-test--intent-message))))
    ('tick
     (appkit-next :model model :render appkit-render-none))
    (_ (appkit-next-reject 'unsupported))))

(ert-deftest appkit-source-emits-closes-once-and-keeps-terminal-tombstone ()
  (let (emit close app (starts 0) (cancels 0))
    (let* ((starter
            (lambda (_context _input emit-gate close-gate)
              (setq starts (1+ starts)
                    emit emit-gate
                    close close-gate)
              (appkit-source-cancellation-create
               :kind 'logical
               :cancel (lambda () (setq cancels (1+ cancels))))))
           (sources
            (lambda (model)
              (list
               (appkit-source-spec-create
                :key 'stream
                :identity (plist-get model :identity)
                :input (list (plist-get model :identity))
                :start starter
                :event #'appkit-source-test--event-message
                :closed #'appkit-source-test--closed-message
                :emission-policy 'lossless
                :pending-limit 4
                :cancellation-requirement 'logical))))
           (type
            (appkit-app-type-create
             :name 'source-test
             :init
             (lambda (_context _input)
               (appkit-next
                :model '(:identity one :events nil)
                :render appkit-render-none))
             :update #'appkit-source-test--update
             :sources sources
             :shutdown #'ignore)))
      (unwind-protect
          (progn
            (setq app (appkit-app-start type))
            (should (= 1 starts))
            (should (funcall emit 'a))
            (should (funcall emit 'b))
            (should (= 1 (appkit-loop-run-pass (appkit-app-loop app))))
            (should (= 1 (appkit-loop-run-pass (appkit-app-loop app))))
            (should
             (equal (plist-get (appkit-app-model app) :events)
                    '((event a) (event b))))
            (appkit-app-send app 'tick)
            (should (= 1 starts))
            (should (funcall close 'remote))
            (should-not (funcall close 'duplicate))
            (should-not (funcall emit 'late))
            (should (= 1 (appkit-loop-run-pass (appkit-app-loop app))))
            (should
             (equal (plist-get (appkit-app-model app) :events)
                    '((event a) (event b) (closed (remote)))))
            (appkit-app-send app 'tick)
            (should (= 1 starts))
            (should (= 0 cancels)))
        (when (and app (not (eq (appkit-app-status app) 'stopped)))
          (appkit-app-close app))))))

(ert-deftest appkit-source-latest-and-coalesced-staging-stay-bounded ()
  (let (emit-latest emit-coalesced app)
    (let ((type
           (appkit-app-type-create
            :name 'source-coalescing-test
            :init
            (lambda (_context _input)
              (appkit-next
               :model '(:identity coalescing :events nil)
               :render appkit-render-none))
            :update #'appkit-source-test--update
            :sources
            (lambda (_model)
              (list
               (appkit-source-spec-create
                :key 'latest :identity 'latest :input 'latest
                :start
                (lambda (_context _input emit _close)
                  (setq emit-latest emit)
                  (appkit-source-cancellation-create :kind 'logical))
                :event #'appkit-source-test--tagged-event
                :closed #'appkit-source-test--closed-message
                :emission-policy 'latest
                :cancellation-requirement 'logical)
               (appkit-source-spec-create
                :key 'coalesced :identity 'coalesced :input 'coalesced
                :start
                (lambda (_context _input emit _close)
                  (setq emit-coalesced emit)
                  (appkit-source-cancellation-create :kind 'logical))
                :event #'appkit-source-test--tagged-event
                :closed #'appkit-source-test--closed-message
                :emission-policy 'coalesce-by-key :pending-limit 2
                :cancellation-requirement 'logical)))
            :shutdown #'ignore)))
      (unwind-protect
          (progn
            (setq app (appkit-app-start type))
            (funcall emit-latest 'old)
            (funcall emit-latest 'new)
            (funcall emit-coalesced 'first 'old)
            (funcall emit-coalesced 'second 'kept)
            (funcall emit-coalesced 'first 'new)
            (dotimes (_ 3)
              (should (= 1 (appkit-loop-run-pass (appkit-app-loop app)))))
            (should
             (equal (plist-get (appkit-app-model app) :events)
                    '((event (latest new))
                      (event (coalesced second kept))
                      (event (coalesced first new))))))
        (when (and app (not (eq (appkit-app-status app) 'stopped)))
          (appkit-app-close app))))))

(ert-deftest appkit-source-surface-starts-after-initial-render ()
  (let (emit surface buffer events)
    (let ((type
           (appkit-surface-type-create
            :name 'surface-source-test
            :mode #'special-mode
            :init
            (lambda (_context _input)
              (appkit-next :model 'surface :render 'initial))
            :update
            (lambda (_context model message)
              (pcase message
                (`(source-event ,value)
                 (push (list 'event value) events)
                 (appkit-next :model model :render appkit-render-none))
                (_ (appkit-next-reject 'unsupported))))
            :sources
            (lambda (_model)
              (list
               (appkit-source-spec-create
                :key 'surface-stream :identity 'surface-stream :input nil
                :start
                (lambda (_context _input emit-gate _close-gate)
                  (push 'source-start events)
                  (setq emit emit-gate)
                  (appkit-source-cancellation-create :kind 'logical))
                :event #'appkit-source-test--event-message
                :closed #'appkit-source-test--closed-message
                :emission-policy 'latest
                :cancellation-requirement 'logical)))
            :renderer-factory
            (lambda (_surface)
              (appkit-generated-renderer-create
               :mount #'ignore
               :merge (lambda (_left right) right)
               :render
               (lambda (&rest _arguments)
                 (push 'render events)
                 nil)
               :recover nil
               :resource-request nil
               :unmount #'ignore)))))
      (unwind-protect
          (progn
            (setq surface (appkit-open-generated-surface type)
                  buffer (appkit-surface-buffer surface))
            (should (equal (nreverse events) '(render source-start)))
            (setq events nil)
            (should (funcall emit 'surface-event))
            (should (= 1 (appkit-loop-run-pass
                          (appkit-surface-loop surface))))
            (should (equal events '((event surface-event)))))
        (when (and surface
                   (not (eq (appkit-surface-status surface) 'stopped)))
          (appkit-surface-stop surface))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest appkit-source-lossless-overflow-closes-visibly ()
  (let (emit app (cancelled nil))
    (let* ((starter
            (lambda (_context _input emit-gate _close-gate)
              (setq emit emit-gate)
              (appkit-source-cancellation-create
               :kind 'transport
               :cancel (lambda () (setq cancelled t)))))
           (type
            (appkit-app-type-create
             :name 'source-overflow-test
             :init
             (lambda (_context _input)
               (appkit-next
                :model '(:identity overflow :events nil)
                :render appkit-render-none))
             :update #'appkit-source-test--update
             :sources
             (lambda (_model)
               (list
                (appkit-source-spec-create
                 :key 'stream :identity 'overflow :input nil
                 :start starter
                 :event #'appkit-source-test--event-message
                 :closed #'appkit-source-test--closed-message
                 :emission-policy 'lossless :pending-limit 2
                 :cancellation-requirement 'transport)))
             :shutdown #'ignore)))
      (unwind-protect
          (progn
            (setq app (appkit-app-start type))
            (should (funcall emit 'one))
            (should (funcall emit 'two))
            (should (funcall emit 'overflow))
            (should cancelled)
            (dotimes (_ 3)
              (should (= 1 (appkit-loop-run-pass (appkit-app-loop app)))))
            (should
             (equal (plist-get (appkit-app-model app) :events)
                    '((event one) (event two) (closed (source-overflow)))))
            (should-not (funcall emit 'late)))
        (when (and app (not (eq (appkit-app-status app) 'stopped)))
          (appkit-app-close app))))))

(ert-deftest appkit-source-quiescent-replacement-delays-successor ()
  (let (emit quiesce app starts)
    (let* ((starter
            (lambda (_context input emit-gate _close-gate)
              (push input starts)
              (setq emit emit-gate)
              (appkit-source-cancellation-create
               :kind 'quiescent-transport
               :cancel (lambda (gate) (setq quiesce gate)))))
           (sources
            (lambda (model)
              (let ((identity (plist-get model :identity)))
                (list
                 (appkit-source-spec-create
                  :key 'stream :identity identity :input identity
                  :start starter
                  :event #'appkit-source-test--event-message
                  :closed #'appkit-source-test--closed-message
                  :emission-policy 'latest
                  :cancellation-requirement 'quiescent-transport)))))
           (type
            (appkit-app-type-create
             :name 'source-quiescence-test
             :init
             (lambda (_context _input)
               (appkit-next
                :model '(:identity old :events nil)
                :render appkit-render-none))
             :update #'appkit-source-test--update
             :sources sources
             :shutdown #'ignore)))
      (unwind-protect
          (progn
            (setq app (appkit-app-start type))
            (should (equal starts '(old)))
            (let ((old-emit emit))
              (appkit-app-send app '(identity new))
              (should (equal starts '(old)))
              (should (functionp quiesce))
              (should-not (funcall old-emit 'stale)))
            (should (funcall quiesce))
            (should (= 1 (appkit-loop-run-pass (appkit-app-loop app))))
            (should (equal starts '(new old)))
            (should (= 1 (appkit-loop-revision (appkit-app-loop app))))
            (should (funcall emit 'current))
            (should (= 1 (appkit-loop-run-pass (appkit-app-loop app))))
            (should
             (equal (plist-get (appkit-app-model app) :events)
                    '((event current)))))
        (when (and app (not (eq (appkit-app-status app) 'stopped)))
          (appkit-app-close app))))))

(ert-deftest appkit-source-outbound-results-enter-later-pass ()
  (let (resolve emit app adapter-inputs)
    (let* ((outbound
            (lambda (_context _input payload result-gate)
              (push payload adapter-inputs)
              (setq resolve result-gate)
              appkit-source-outbound-pending))
           (type
            (appkit-app-type-create
             :name 'source-outbound-test
             :init
             (lambda (_context _input)
               (appkit-next
                :model '(:identity live :events nil)
                :render appkit-render-none))
             :update #'appkit-source-test--update
             :sources
             (lambda (_model)
               (list
                (appkit-source-spec-create
                 :key 'stream :identity 'live :input 'owned
                 :start
                 (lambda (_context _input emit-gate _close-gate)
                   (setq emit emit-gate)
                   (appkit-source-cancellation-create :kind 'logical))
                 :event #'appkit-source-test--event-message
                 :closed #'appkit-source-test--closed-message
                 :outbound outbound
                 :emission-policy 'latest
                 :outbound-pending-limit 1
                 :cancellation-requirement 'logical)))
             :shutdown #'ignore)))
      (unwind-protect
          (progn
            (setq app (appkit-app-start type))
            (appkit-app-send app '(send live first))
            (should (equal adapter-inputs '(first)))
            (should-not (plist-get (appkit-app-model app) :events))
            (appkit-app-send app '(send live second))
            (should (equal adapter-inputs '(first)))
            (should (= 1 (appkit-loop-run-pass (appkit-app-loop app))))
            (should
             (equal (plist-get (appkit-app-model app) :events)
                    '((outbound second backpressured))))
            (should (funcall resolve 'accepted))
            (should (= 1 (appkit-loop-run-pass (appkit-app-loop app))))
            (should
             (equal (plist-get (appkit-app-model app) :events)
                    '((outbound second backpressured)
                      (outbound first accepted))))
            (appkit-app-send app '(send stale third))
            (dotimes (index 3)
              (funcall emit index)
              (should (= 1 (appkit-loop-run-pass (appkit-app-loop app)))))
            (should
             (member '(outbound third stale)
                     (plist-get (appkit-app-model app) :events))))
        (appkit-app-send app '(send live invalid))
        (should (funcall resolve 'invalid-outcome))
        (should (= 1 (appkit-loop-run-pass (appkit-app-loop app))))
        (should (eq (appkit-app-status app) 'faulted))
        (when (and app (not (eq (appkit-app-status app) 'stopped)))
          (appkit-app-close app))))))

(provide 'appkit-source-test)

;;; appkit-source-test.el ends here
