;;; appkit-topic-test.el --- App-to-Surface publish and subscribe tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'appkit-app)
(require 'appkit-surface)

(defun appkit-topic-test--app (&optional subscribed)
  "Start an App that publishes `(publish TOPIC PAYLOAD)' messages.
SUBSCRIBED is the App type's interest mapper.  Interest messages and requests
are recorded in the model."
  (appkit-app-start
   (appkit-app-type-create
    :name 'topic-test
    :init (lambda (_context _input) (appkit-next :model nil :render appkit-render-none))
    :update
    (lambda (context model message)
      (pcase message
        (`(publish ,topic ,payload)
         (appkit-next :model model :render appkit-render-none
                      :commands (list (appkit-command-publish topic payload))))
        (`(ask ,payload)
         ;; Reply to the asking Surface, then publish, in one transition.
         (appkit-next
          :model model :render appkit-render-none
          :commands
          (list (appkit-command-post-message
                 :target (appkit-transition-context-reply-route context)
                 :message (list 'reply payload) :delivery 'report)
                (appkit-command-publish 'feed payload))))
        (_ (appkit-next :model (append model (list message))
                        :render appkit-render-none))))
    :subscribed subscribed)))

(defun appkit-topic-test--surface (app identity subscriptions &optional record)
  "Open a Surface of APP at IDENTITY following SUBSCRIPTIONS of its model.
The model is a plist; `(set KEY VALUE)' messages change it.  RECORD receives
every other message with the pass's App read-view revision."
  (appkit-open-generated-surface
   (appkit-surface-type-create
    :name 'topic-test-surface
    :mode #'special-mode
    :init (lambda (_context input) (appkit-next :model input :render appkit-render-none))
    :update
    (lambda (context model message)
      (pcase message
        (`(set ,key ,value)
         (appkit-next :model (plist-put (copy-sequence model) key value)
                      :render appkit-render-none))
        ('publish
         (appkit-next :model model :render appkit-render-none
                      :commands (list (appkit-command-publish 'feed model))))
        ('ask
         (appkit-next
          :model model :render appkit-render-none
          :commands
          (list (appkit-command-post-message
                 :target (appkit-transition-context-parent-address context)
                 :message (list 'ask identity) :delivery 'report
                 :reply-correlation identity))))
        (_
         (when record
           (funcall record
                    (list message
                          (appkit-app-read-view-revision
                           (appkit-transition-context-app-read-view context)))))
         (appkit-next :model model :render appkit-render-none))))
    :subscriptions subscriptions
    :renderer-factory
    (lambda (_surface)
      (appkit-generated-renderer-create
       :mount #'ignore :merge (lambda (_left right) right)
       :render #'ignore :recover nil :unmount #'ignore)))
   :app app :identity identity :input (list :room identity)))

(defun appkit-topic-test--level (topic)
  "Return a level subscription to TOPIC delivering `(changed PAYLOAD)'."
  (appkit-subscription-create :topic topic
                              :notify (lambda (payload) (list 'changed payload))))

(defun appkit-topic-test--room (model)
  "Follow MODEL's current room."
  (list (appkit-topic-test--level (list 'room (plist-get model :room)))))

(defun appkit-topic-test--pass (&rest owners)
  "Run one pass of every App or Surface in OWNERS."
  (dolist (owner owners)
    (appkit-loop-run-pass (if (appkit-app-p owner)
                              (appkit-app-loop owner)
                            (appkit-surface-loop owner)))))

(defmacro appkit-topic-test--with-app (binding &rest body)
  "Bind (APP SUBSCRIBED) and evaluate BODY, closing APP and its buffers."
  (declare (indent 1))
  `(let ((,(car binding) (appkit-topic-test--app ,(cadr binding))))
     (unwind-protect (progn ,@body)
       (let ((buffers (mapcar #'appkit-surface-buffer
                              (appkit-app--surface-snapshot ,(car binding)))))
         (appkit-app-close ,(car binding))
         (mapc #'kill-buffer (seq-filter #'buffer-live-p buffers))))))

(ert-deftest appkit-topic-publication-reaches-only-subscribers-after-commit ()
  (appkit-topic-test--with-app (app nil)
    (let* (a-log b-log
           (a (appkit-topic-test--surface app 'a #'appkit-topic-test--room (lambda (entry) (push entry a-log))))
           (b (appkit-topic-test--surface app 'b #'appkit-topic-test--room (lambda (entry) (push entry b-log)))))
      (appkit-app-send app '(publish (room a) hello))
      (appkit-topic-test--pass a b)
      (should (equal a-log
                     (list (list '(changed hello)
                                 (appkit-loop-revision (appkit-app-loop app))))))
      (should-not b-log))))

(ert-deftest appkit-topic-level-subscription-keeps-only-the-latest-payload ()
  (appkit-topic-test--with-app (app nil)
    (let* (log (surface (appkit-topic-test--surface
                         app 'a #'appkit-topic-test--room (lambda (entry) (push entry log)))))
      (dolist (payload '(1 2 3))
        (appkit-app-send app (list 'publish '(room a) payload)))
      (appkit-topic-test--pass surface)
      (should (equal (mapcar #'car log) '((changed 3))))
      (appkit-app-send app '(publish (room a) 4))
      (appkit-topic-test--pass surface)
      (should (equal (mapcar #'car log) '((changed 4) (changed 3)))))))

(ert-deftest appkit-topic-lossless-subscription-orders-then-resyncs-on-overflow ()
  (appkit-topic-test--with-app (app nil)
    (let* (log
           (surface
            (appkit-topic-test--surface
             app 'a
             (lambda (_model)
               (list (appkit-subscription-create
                      :topic 'events :delivery 'lossless :pending-limit 2
                      :notify (lambda (payload) (list 'event payload))
                      :resync (lambda () '(resync)))))
             (lambda (entry) (push entry log)))))
      (appkit-app-send app '(publish events 1))
      (appkit-app-send app '(publish events 2))
      (appkit-topic-test--pass surface)
      (should (equal (reverse (mapcar #'car log)) '((event 1) (event 2))))
      (setq log nil)
      (dolist (payload '(3 4 5 6))
        (appkit-app-send app (list 'publish 'events payload)))
      (appkit-topic-test--pass surface)
      ;; The backlog beyond two is discarded; one resync replaces all of it.
      (should (equal (mapcar #'car log) '((resync))))
      (setq log nil)
      (appkit-app-send app '(publish events 7))
      (appkit-topic-test--pass surface)
      (should (equal (mapcar #'car log) '((event 7)))))))

(ert-deftest appkit-topic-subscriptions-follow-the-surface-model ()
  (appkit-topic-test--with-app (app nil)
    (let* (log (surface (appkit-topic-test--surface
                         app 'a #'appkit-topic-test--room (lambda (entry) (push entry log)))))
      (appkit-surface-post surface '(set :room b))
      (appkit-topic-test--pass surface)
      (appkit-app-send app '(publish (room a) ignored))
      (appkit-app-send app '(publish (room b) fresh))
      (appkit-topic-test--pass surface)
      (should (equal (mapcar #'car log) '((changed fresh))))
      ;; A notification still queued when its topic is dropped never arrives:
      ;; the first pass ends after 32 messages and reconciles to room c.
      (appkit-surface-post surface '(set :room c))
      (dotimes (_ 40) (appkit-surface-post surface 'filler))
      (appkit-app-send app '(publish (room b) late))
      (appkit-topic-test--pass surface)
      (appkit-topic-test--pass surface)
      (should-not (member '(changed late) (mapcar #'car log))))))

(ert-deftest appkit-topic-reports-first-and-last-subscriber-to-the-app ()
  (appkit-topic-test--with-app
      (app (lambda (topic subscribed-p) (list 'interest topic subscribed-p)))
    (let ((first (appkit-topic-test--surface app 'one
                                             (lambda (_) (list (appkit-topic-test--level 'feed))))))
      (appkit-topic-test--pass app)
      (should (equal (appkit-app-model app) '((interest feed t))))
      (let ((second (appkit-topic-test--surface
                     app 'two (lambda (_) (list (appkit-topic-test--level 'feed))))))
        (appkit-surface-stop first)
        (appkit-topic-test--pass app)
        (should (equal (appkit-app-model app) '((interest feed t))))
        (appkit-surface-stop second)
        ;; Losing and regaining the topic before the App runs is no change.
        (let ((third (appkit-topic-test--surface
                      app 'three (lambda (_) (list (appkit-topic-test--level 'feed))))))
          (appkit-topic-test--pass app)
          (should (equal (appkit-app-model app) '((interest feed t))))
          (appkit-surface-stop third)
          (appkit-topic-test--pass app)
          (should (equal (appkit-app-model app)
                         '((interest feed t) (interest feed nil)))))))))

(ert-deftest appkit-topic-publication-keeps-fifo-with-directed-replies ()
  (appkit-topic-test--with-app (app nil)
    (let* (log (surface (appkit-topic-test--surface
                         app 'a (lambda (_) (list (appkit-topic-test--level 'feed)))
                         (lambda (entry) (push entry log)))))
      (appkit-surface-send surface 'ask)
      (appkit-topic-test--pass app surface)
      (should (equal (reverse (mapcar #'car log)) '((reply a) (changed a)))))))

(ert-deftest appkit-topic-full-data-lane-overflows-into-one-control-wake ()
  (appkit-topic-test--with-app (app nil)
    (let* (log (surface (appkit-topic-test--surface
                         app 'a #'appkit-topic-test--room (lambda (entry) (push entry log))))
           (loop (appkit-surface-loop surface)))
      (while (eq (appkit-loop-post loop 'filler) 'enqueued))
      (appkit-app-send app '(publish (room a) 1))
      (appkit-app-send app '(publish (room a) 2))
      (should (= (appkit-loop--lane-count (appkit-loop--control-lane loop)) 1))
      (while (> (appkit-loop-pending-count loop) 0)
        (appkit-topic-test--pass surface))
      (should (equal (seq-remove (lambda (message) (eq message 'filler))
                                 (mapcar #'car log))
                     '((changed 2)))))))

(ert-deftest appkit-topic-only-apps-publish-and-subscriptions-need-an-app ()
  (appkit-topic-test--with-app (app nil)
    (let ((surface (appkit-topic-test--surface app 'a nil)))
      (should-error (appkit-surface-send surface 'publish))
      (should-not (appkit-surface-live-p surface))))
  (should-error
   (appkit-open-generated-surface
    (appkit-surface-type-create
     :name 'standalone :mode #'special-mode
     :init (lambda (_c _i) (appkit-next :model nil :render appkit-render-none))
     :update (lambda (_c model _m) (appkit-next :model model :render appkit-render-none))
     :subscriptions (lambda (_model) nil)
     :renderer-factory
     (lambda (_s) (appkit-generated-renderer-create
                   :mount #'ignore :merge (lambda (_l r) r) :render #'ignore
                   :unmount #'ignore))))))

(provide 'appkit-topic-test)

;;; appkit-topic-test.el ends here
