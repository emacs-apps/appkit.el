;;; appkit-core-test.el --- Tests for appkit lifecycle -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(require 'appkit-core)
(require 'appkit-test-helper)

(define-derived-mode appkit-test-mode special-mode "Appkit-Test")

(define-derived-mode appkit-test-alternate-mode special-mode
  "Appkit-Test-Alternate")

(ert-deftest appkit-attach-view-rejects-duplicate-global-fingerprint ()
  (let ((app-one (appkit-start-app 'appkit-test :id 'account))
        (app-two (appkit-start-app 'appkit-test :id 'account))
        (first-buffer (generate-new-buffer " *appkit-attach-one*"))
        (second-buffer (generate-new-buffer " *appkit-attach-two*"))
        first)
    (unwind-protect
        (progn
          (with-current-buffer first-buffer
            (appkit-test-mode)
            (setq first
                  (appkit-attach-view
                   :app app-one :id 'chat :mode 'appkit-test-mode)))
          (with-current-buffer second-buffer
            (appkit-test-mode)
            (should-error
             (appkit-attach-view
              :app app-two :id 'chat :mode 'appkit-test-mode)))
          (should (appkit-view-live-p first))
          (should-not (appkit-view-for-id app-two 'chat)))
      (appkit-stop-app app-one)
      (appkit-stop-app app-two)
      (dolist (buffer (list first-buffer second-buffer))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest appkit-attach-view-rejects-foreign-detached-fingerprint ()
  (let ((app (appkit-start-app 'appkit-test :id 'account))
        (buffer (generate-new-buffer " *appkit-attach-detached*"))
        first)
    (unwind-protect
        (with-current-buffer buffer
          (appkit-test-mode)
          (setq first
                (appkit-attach-view
                 :app app :id 'first :mode 'appkit-test-mode))
          (appkit-kill-view first)
          (should-error
           (appkit-attach-view
            :app app :id 'second :mode 'appkit-test-mode))
          (should (equal appkit--view-fingerprint
                         (appkit--view-fingerprint-for app 'first))))
      (appkit-stop-app app)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest appkit-indirect-buffer-alias-cannot-attach-or-kill-owner-view ()
  (let ((app (appkit-start-app 'appkit-test :id 'account))
        (base (generate-new-buffer " *appkit-indirect-base*"))
        clone view)
    (unwind-protect
        (progn
          (with-current-buffer base
            (appkit-test-mode)
            (setq view
                  (appkit-attach-view
                   :app app :id 'chat :mode 'appkit-test-mode))
            (setq clone
                  (clone-indirect-buffer " *appkit-indirect-clone*" nil)))
          (with-current-buffer clone
            (should (eq appkit--current-view view))
            (should-not (appkit-current-view))
            (should-error
             (appkit-attach-view
              :app app :id 'chat :mode 'appkit-test-mode)))
          (kill-buffer clone)
          (should (appkit-view-live-p view))
          (with-current-buffer base
            (should (eq (appkit-current-view) view))))
      (appkit-stop-app app)
      (dolist (buffer (list base clone))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest appkit-open-view-initializes-major-mode-once ()
  (let ((app (appkit-start-app 'appkit-test :id 'app))
        (mode-runs 0)
        first second)
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-test-mode)
                   (lambda ()
                     (setq mode-runs (1+ mode-runs))
                     (special-mode)
                     (setq-local major-mode 'appkit-test-mode))))
          (setq first
                (appkit-open-view
                 :app app :id 'chat :mode 'appkit-test-mode
                 :buffer-name " *appkit-open-view*"))
          (setq second
                (appkit-open-view
                 :app app :id 'chat :mode 'appkit-test-mode
                 :buffer-name " *appkit-open-view*"))
          (should (eq first second))
          (should (= mode-runs 1)))
      (when (appkit-view-p first) (appkit-kill-view first t))
      (appkit-stop-app app))))

(ert-deftest appkit-open-view-reuses-renamed-detached-buffer ()
  (let ((app (appkit-start-app 'appkit-test :id 'app))
        (buffer-name (generate-new-buffer-name " *appkit-detached*"))
        (renamed-name (generate-new-buffer-name
                       " *appkit-detached-renamed*"))
        (setup-runs 0)
        first second third buffer)
    (unwind-protect
        (progn
          (setq first
                (appkit-open-view
                 :app app :id 'chat :mode 'appkit-test-mode
                 :buffer-name buffer-name
                 :setup (lambda (_view) (setq setup-runs (1+ setup-runs)))))
          (setq buffer (appkit-view-buffer first))
          (with-current-buffer buffer
            (rename-buffer renamed-name))
          (appkit-kill-view first)
          (setq second
                (appkit-open-view
                 :app app :id 'chat :mode 'appkit-test-mode
                 :buffer-name buffer-name
                 :setup (lambda (_view) (setq setup-runs (1+ setup-runs)))))
          (should (eq (appkit-view-buffer second) buffer))
          (should-not (eq second first))
          (should (= setup-runs 2))
          (setq third
                (appkit-open-view
                 :app app :id 'chat :mode 'appkit-test-mode
                 :buffer-name buffer-name
                 :setup (lambda (_view) (setq setup-runs (1+ setup-runs)))))
          (should (eq third second))
          (should (= setup-runs 2)))
      (appkit-stop-app app)
      (dolist (candidate (list buffer
                               (get-buffer buffer-name)
                               (get-buffer renamed-name)))
        (when (buffer-live-p candidate)
          (kill-buffer candidate))))))

(ert-deftest appkit-open-view-reuses-detached-buffer-after-app-restart ()
  (let ((buffer-name (generate-new-buffer-name " *appkit-restart*"))
        (renamed-name (generate-new-buffer-name
                       " *appkit-restart-renamed*"))
        (setup-runs 0)
        app-one app-two first second buffer)
    (unwind-protect
        (progn
          (setq app-one
                (appkit-start-app 'appkit-test
                                  :id (copy-sequence "account")))
          (setq first
                (appkit-open-view
                 :app app-one :id (list 'chat "42")
                 :mode 'appkit-test-mode :buffer-name buffer-name
                 :setup (lambda (_view) (setq setup-runs (1+ setup-runs)))))
          (setq buffer (appkit-view-buffer first))
          (with-current-buffer buffer
            (rename-buffer renamed-name))
          (appkit-stop-app app-one)
          (setq app-two
                (appkit-start-app 'appkit-test
                                  :id (copy-sequence "account")))
          (setq second
                (appkit-open-view
                 :app app-two :id (list 'chat "42")
                 :mode 'appkit-test-mode :buffer-name buffer-name
                 :setup (lambda (_view) (setq setup-runs (1+ setup-runs)))))
          (should (eq (appkit-view-buffer second) buffer))
          (should (eq (appkit-view-app second) app-two))
          (should-not (eq second first))
          (should (= setup-runs 2)))
      (when (appkit-app-p app-one)
        (appkit-stop-app app-one))
      (when (appkit-app-p app-two)
        (appkit-stop-app app-two))
      (dolist (candidate (list buffer
                               (get-buffer buffer-name)
                               (get-buffer renamed-name)))
        (when (buffer-live-p candidate)
          (kill-buffer candidate))))))

(ert-deftest appkit-open-view-does-not-reuse-repurposed-buffer ()
  (let ((app (appkit-start-app 'appkit-test :id 'app))
        (buffer-name (generate-new-buffer-name " *appkit-repurpose*"))
        (renamed-name (generate-new-buffer-name
                       " *appkit-repurpose-renamed*"))
        first second old-buffer new-buffer)
    (unwind-protect
        (progn
          (setq first
                (appkit-open-view
                 :app app :id 'chat :mode 'appkit-test-mode
                 :buffer-name buffer-name))
          (setq old-buffer (appkit-view-buffer first))
          (with-current-buffer old-buffer
            (rename-buffer renamed-name)
            (fundamental-mode)
            (should-not appkit--view-fingerprint))
          (should-not (appkit-view-live-p first))
          (setq second
                (appkit-open-view
                 :app app :id 'chat :mode 'appkit-test-mode
                 :buffer-name buffer-name))
          (setq new-buffer (appkit-view-buffer second))
          (should-not (eq new-buffer old-buffer))
          (should (equal (buffer-name new-buffer) buffer-name)))
      (appkit-stop-app app)
      (dolist (candidate (list old-buffer new-buffer
                               (get-buffer buffer-name)
                               (get-buffer renamed-name)))
        (when (buffer-live-p candidate)
          (kill-buffer candidate))))))

(ert-deftest appkit-open-view-does-not-steal-live-fingerprint-collision ()
  (let ((buffer-name (generate-new-buffer-name " *appkit-collision*"))
        app-one app-two first buffer)
    (unwind-protect
        (progn
          (setq app-one (appkit-start-app 'appkit-test :id 'account)
                app-two (appkit-start-app 'appkit-test :id 'account))
          (setq first
                (appkit-open-view
                 :app app-one :id 'chat :mode 'appkit-test-mode
                 :buffer-name buffer-name))
          (setq buffer (appkit-view-buffer first))
          (should-error
           (appkit-open-view
            :app app-two :id 'chat :mode 'appkit-test-alternate-mode
            :buffer-name buffer-name))
          (should (appkit-view-live-p first))
          (with-current-buffer buffer
            (should (eq (appkit-current-view) first))
            (should (eq major-mode 'appkit-test-mode)))
          (should-not (appkit-view-for-id app-two 'chat)))
      (when (appkit-app-p app-one)
        (appkit-stop-app app-one))
      (when (appkit-app-p app-two)
        (appkit-stop-app app-two))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest appkit-open-view-disambiguates-live-foreign-fallback-name ()
  (let ((app (appkit-start-app 'appkit-test :id 'account))
        (buffer-name (generate-new-buffer-name " *appkit-shared-title*"))
        first second first-buffer second-buffer)
    (unwind-protect
        (progn
          (setq first
                (appkit-open-view
                 :app app :id 'first :mode 'appkit-test-mode
                 :buffer-name buffer-name))
          (setq second
                (appkit-open-view
                 :app app :id 'second :mode 'appkit-test-mode
                 :buffer-name buffer-name))
          (setq first-buffer (appkit-view-buffer first)
                second-buffer (appkit-view-buffer second))
          (should-not (eq first-buffer second-buffer))
          (should (equal (buffer-name first-buffer) buffer-name))
          (should (string-prefix-p buffer-name (buffer-name second-buffer)))
          (should (appkit-view-live-p first))
          (should (appkit-view-live-p second))
          (with-current-buffer first-buffer
            (should (eq (appkit-current-view) first)))
          (with-current-buffer second-buffer
            (should (eq (appkit-current-view) second))))
      (appkit-stop-app app)
      (dolist (buffer (list first-buffer second-buffer
                            (get-buffer buffer-name)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest appkit-open-view-rejects-live-fingerprint-with-different-name ()
  (let ((first-name (generate-new-buffer-name " *appkit-first-name*"))
        (second-name (generate-new-buffer-name " *appkit-second-name*"))
        app-one app-two first buffer)
    (unwind-protect
        (progn
          (setq app-one (appkit-start-app 'appkit-test :id 'account)
                app-two (appkit-start-app 'appkit-test :id 'account))
          (setq first
                (appkit-open-view
                 :app app-one :id 'chat :mode 'appkit-test-mode
                 :buffer-name first-name))
          (setq buffer (appkit-view-buffer first))
          (should-error
           (appkit-open-view
            :app app-two :id 'chat :mode 'appkit-test-mode
            :buffer-name second-name))
          (should (appkit-view-live-p first))
          (should-not (appkit-view-for-id app-two 'chat))
          (should-not (get-buffer second-name)))
      (when (appkit-app-p app-one) (appkit-stop-app app-one))
      (when (appkit-app-p app-two) (appkit-stop-app app-two))
      (dolist (candidate (list buffer (get-buffer first-name)
                               (get-buffer second-name)))
        (when (buffer-live-p candidate)
          (kill-buffer candidate))))))

(ert-deftest appkit-open-view-rejects-ambiguous-detached-fingerprint ()
  (let* ((app (appkit-start-app 'appkit-test :id 'account))
         (view-id 'chat)
         (fingerprint (appkit--view-fingerprint-for app view-id))
         (first (generate-new-buffer " *appkit-ambiguous-one*"))
         (second (generate-new-buffer " *appkit-ambiguous-two*")))
    (unwind-protect
        (progn
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (appkit-test-mode)
              (setq-local appkit--view-fingerprint fingerprint)))
          (should-error
           (appkit-open-view
            :app app :id view-id :mode 'appkit-test-mode
            :buffer-name " *appkit-ambiguous-fallback*"))
          (should-not (appkit-view-for-id app view-id)))
      (appkit-stop-app app)
      (dolist (buffer (list first second
                            (get-buffer " *appkit-ambiguous-fallback*")))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest appkit-open-view-rejects-live-plus-detached-fingerprint ()
  (let* ((app (appkit-start-app 'appkit-test :id 'account))
         (buffer-name (generate-new-buffer-name " *appkit-live-owner*"))
         (duplicate (generate-new-buffer " *appkit-detached-duplicate*"))
         view buffer)
    (unwind-protect
        (progn
          (setq view
                (appkit-open-view
                 :app app :id 'chat :mode 'appkit-test-mode
                 :buffer-name buffer-name))
          (setq buffer (appkit-view-buffer view))
          (with-current-buffer duplicate
            (appkit-test-mode)
            (setq-local appkit--view-fingerprint
                        (appkit--view-fingerprint-for app 'chat)))
          (should-error
           (appkit-open-view
            :app app :id 'chat :mode 'appkit-test-mode
            :buffer-name buffer-name))
          (should (appkit-view-live-p view)))
      (appkit-stop-app app)
      (dolist (candidate (list buffer duplicate (get-buffer buffer-name)))
        (when (buffer-live-p candidate)
          (kill-buffer candidate))))))

(ert-deftest appkit-open-view-does-not-steal-detached-foreign-fingerprint ()
  (let ((app (appkit-start-app 'appkit-test :id 'app))
        (buffer-name (generate-new-buffer-name " *appkit-detached-owner*"))
        first second first-buffer second-buffer)
    (unwind-protect
        (progn
          (setq first
                (appkit-open-view
                 :app app :id 'first :mode 'appkit-test-mode
                 :buffer-name buffer-name))
          (setq first-buffer (appkit-view-buffer first))
          (appkit-kill-view first)
          (setq second
                (appkit-open-view
                 :app app :id 'second :mode 'appkit-test-mode
                 :buffer-name buffer-name))
          (setq second-buffer (appkit-view-buffer second))
          (should-not (eq first-buffer second-buffer))
          (with-current-buffer first-buffer
            (should (equal appkit--view-fingerprint
                           (appkit--view-fingerprint-for app 'first))))
          (with-current-buffer second-buffer
            (should (equal appkit--view-fingerprint
                           (appkit--view-fingerprint-for app 'second)))))
      (appkit-stop-app app)
      (dolist (buffer (list first-buffer second-buffer
                            (get-buffer buffer-name)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest appkit-open-view-dead-app-fails-before-buffer-side-effects ()
  (let ((app (appkit-start-app 'appkit-test :id 'app))
        (buffer-name (generate-new-buffer-name " *appkit-dead-app*"))
        view buffer fingerprint)
    (unwind-protect
        (progn
          (setq view
                (appkit-open-view
                 :app app :id 'chat :mode 'appkit-test-mode
                 :buffer-name buffer-name))
          (setq buffer (appkit-view-buffer view))
          (appkit-stop-app app)
          (with-current-buffer buffer
            (setq fingerprint appkit--view-fingerprint)
            (should-error
             (appkit-open-view
              :app app :id 'chat :mode 'appkit-test-alternate-mode
              :buffer-name buffer-name))
            (should (eq major-mode 'appkit-test-mode))
            (should (equal appkit--view-fingerprint fingerprint))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest appkit-open-view-mode-replacement-runs-setup-for-new-view ()
  (let ((app (appkit-start-app 'appkit-test :id 'app))
        (buffer-name (generate-new-buffer-name " *appkit-mode-replace*"))
        (setup-runs 0)
        first second buffer)
    (unwind-protect
        (progn
          (setq first
                (appkit-open-view
                 :app app :id 'chat :mode 'appkit-test-mode
                 :buffer-name buffer-name
                 :setup (lambda (_view) (cl-incf setup-runs))))
          (setq buffer (appkit-view-buffer first))
          (setq second
                (appkit-open-view
                 :app app :id 'chat :mode 'appkit-test-alternate-mode
                 :buffer-name buffer-name
                 :setup (lambda (_view) (cl-incf setup-runs))))
          (should-not (eq first second))
          (should-not (appkit-view-live-p first))
          (should (appkit-view-live-p second))
          (should (= setup-runs 2))
          (with-current-buffer buffer
            (should (eq major-mode 'appkit-test-alternate-mode))))
      (appkit-stop-app app)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest appkit-view-for-id-reaps-stale-view-and-owned-handles ()
  (let ((app (appkit-start-app 'appkit-test :id 'app))
        (buffer-name (generate-new-buffer-name " *appkit-stale-view*"))
        cancelled view buffer)
    (unwind-protect
        (progn
          (setq view
                (appkit-open-view
                 :app app :id 'chat :mode 'appkit-test-mode
                 :buffer-name buffer-name))
          (setq buffer (appkit-view-buffer view))
          (appkit-register-handle
           view 'test 'owned (lambda (object) (setq cancelled object)))
          (with-current-buffer buffer
            (remove-hook 'kill-buffer-hook #'appkit--detach-current-view t))
          (kill-buffer buffer)
          (should (appkit-view-alive-p view))
          (should-not (appkit-view-for-id app 'chat))
          (should-not (appkit-view-alive-p view))
          (should (eq cancelled 'owned))
          (should-not (gethash 'chat (appkit-app-view-registry app))))
      (appkit-stop-app app)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest appkit-view-for-id-removes-wrong-id-alias-without-killing-view ()
  (let ((app (appkit-start-app 'appkit-test :id 'app))
        (buffer-name (generate-new-buffer-name " *appkit-wrong-alias*"))
        view buffer)
    (unwind-protect
        (progn
          (setq view
                (appkit-open-view
                 :app app :id 'right :mode 'appkit-test-mode
                 :buffer-name buffer-name))
          (setq buffer (appkit-view-buffer view))
          (puthash 'wrong view (appkit-app-view-registry app))
          (should-not (appkit-view-for-id app 'wrong))
          (should (appkit-view-live-p view))
          (should (eq view (appkit-view-for-id app 'right))))
      (appkit-stop-app app)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest appkit-view-kill-cancels-owned-handles ()
  (appkit-test-with-view
    (let ((view (appkit-current-view))
          cancelled)
      (appkit-register-handle
       view 'test 'owned (lambda (object) (setq cancelled object)))
      (appkit-kill-view view)
      (should (eq cancelled 'owned))
      (should-not (appkit-view-live-p view)))))

(ert-deftest appkit-retire-handle-forgets-owner-without-cancelling ()
  (let ((app (appkit-start-app 'appkit-test :id 'retire :shutdown #'ignore))
        cancellations
        handle)
    (unwind-protect
        (progn
          (setq handle
                (appkit-register-handle
                 app 'test 'owned
                 (lambda (object) (push object cancellations))))
          (should (memq handle (appkit-app-handles app)))
          (should (appkit-retire-handle handle))
          (should-not (appkit-handle-alive-p handle))
          (should-not (appkit-app-handles app))
          (should-not cancellations)
          (should-not (appkit-retire-handle handle))
          (appkit-stop-app app)
          (should-not cancellations))
      (when (appkit-app-live-p app)
        (appkit-stop-app app)))))

(ert-deftest appkit-view-kill-finishes-lifecycle-cleanup-after-handle-quit ()
  (appkit-test-with-view
    (let* ((view (appkit-current-view))
           (app (appkit-view-app view))
           (id (appkit-view-id view))
           (buffer (current-buffer))
           (quit-cancellations 0)
           (sibling-cancellations 0)
           sibling quitting)
      ;; Handles are stored newest first, so register the quitting handle last.
      (setq sibling
            (appkit-register-handle
             view 'function
             (lambda () (cl-incf sibling-cancellations))))
      (setq quitting
            (appkit-register-handle
             view 'function
             (lambda ()
               (cl-incf quit-cancellations)
               (signal 'quit nil))))
      (cl-letf (((symbol-function 'message) #'ignore))
        (appkit-kill-view view))
      (should (= quit-cancellations 1))
      (should (= sibling-cancellations 1))
      (should-not (appkit-handle-alive-p quitting))
      (should-not (appkit-handle-alive-p sibling))
      (should-not (appkit-view-handles view))
      (should-not (appkit-view-alive-p view))
      (should-not (gethash id (appkit-app-view-registry app)))
      (with-current-buffer buffer
        (should-not (appkit-current-view))))))

(ert-deftest appkit-view-kill-finishes-siblings-after-handle-throw ()
  (appkit-test-with-view
    (let* ((view (appkit-current-view))
           (app (appkit-view-app view))
           (id (appkit-view-id view))
           (sibling-cancellations 0))
      (appkit-register-handle
       view 'function (lambda () (cl-incf sibling-cancellations)))
      (appkit-register-handle
       view 'function (lambda () (throw 'escape 'escaped)))
      (should
       (eq 'escaped
           (catch 'escape
             (appkit-kill-view view)
             'returned)))
      (should (= sibling-cancellations 1))
      (should-not (appkit-view-handles view))
      (should-not (appkit-view-alive-p view))
      (should-not (gethash id (appkit-app-view-registry app)))
      (should-not (appkit-current-view)))))

(ert-deftest appkit-view-kill-cancels-thousands-of-owned-handles ()
  (appkit-test-with-view
    (let ((view (appkit-current-view))
          (cancellations 0))
      (dotimes (_ 2000)
        (appkit-register-handle
         view 'function (lambda () (cl-incf cancellations))))
      (appkit-kill-view view)
      (should (= cancellations 2000))
      (should-not (appkit-view-handles view))
      (should-not (appkit-view-alive-p view)))))

(ert-deftest appkit-stop-app-finishes-all-stages-after-view-handle-throw ()
  (let ((app-handle-cancellations 0)
        (shutdowns 0)
        app view buffer)
    (unwind-protect
        (progn
          (setq app
                (appkit-start-app
                 'appkit-test :id 'account
                 :shutdown (lambda (_app) (cl-incf shutdowns))))
          (setq buffer (generate-new-buffer " *appkit-stop-throw*"))
          (with-current-buffer buffer
            (appkit-test-mode)
            (setq view
                  (appkit-attach-view
                   :app app :id 'chat :mode 'appkit-test-mode)))
          (appkit-register-handle
           app 'function (lambda () (cl-incf app-handle-cancellations)))
          (appkit-register-handle
           view 'function (lambda () (throw 'escape 'escaped)))
          (should
           (eq 'escaped
               (catch 'escape
                 (appkit-stop-app app)
                 'returned)))
          (should-not (appkit-app-live-p app))
          (should-not (appkit-view-alive-p view))
          (should (= app-handle-cancellations 1))
          (should (= shutdowns 1))
          (should (= (hash-table-count (appkit-app-view-registry app)) 0)))
      (when (and (appkit-app-p app) (appkit-app-live-p app))
        (appkit-stop-app app))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest appkit-stop-app-does-not-kill-foreign-registry-alias ()
  (let ((app-one (appkit-start-app 'appkit-test :id 'one))
        (app-two (appkit-start-app 'appkit-test :id 'two))
        (buffer (generate-new-buffer " *appkit-foreign-alias*"))
        view)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (appkit-test-mode)
            (setq view
                  (appkit-attach-view
                   :app app-two :id 'chat :mode 'appkit-test-mode)))
          (puthash 'foreign view (appkit-app-view-registry app-one))
          (appkit-stop-app app-one)
          (should (appkit-view-live-p view))
          (should (eq view (appkit-view-for-id app-two 'chat)))
          (should (= (hash-table-count
                      (appkit-app-view-registry app-one))
                     0)))
      (when (appkit-app-live-p app-one) (appkit-stop-app app-one))
      (when (appkit-app-live-p app-two) (appkit-stop-app app-two))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest appkit-major-mode-change-detaches-view ()
  (appkit-test-with-view
    (let ((view (appkit-current-view)))
      (fundamental-mode)
      (should-not (appkit-view-live-p view))
      (should-not (appkit-current-view)))))

(ert-deftest appkit-view-event-acknowledgement-preserves-new-tail-events ()
  (appkit-test-with-view
    (let ((view (appkit-current-view)))
      (appkit-view-enqueue-event view 'first)
      (appkit-view-enqueue-event view 'second)
      (let ((snapshot (appkit-view-pending-events-snapshot view)))
        (appkit-view-enqueue-event view 'third)
        (appkit-view-acknowledge-events view (length snapshot)))
      (should (equal (appkit-view-pending-events view) '(third))))))

(ert-deftest appkit-app-events-unsubscribe-on-stop ()
  (let ((app (appkit-start-app 'appkit-test :id 'events))
        seen)
    (appkit-app-on app 'change (lambda (value) (push value seen)))
    (appkit-app-emit app 'change 1)
    (should (equal seen '(1)))
    (appkit-stop-app app)
    (appkit-app-emit app 'change 2)
    (should (equal seen '(1)))))

(ert-deftest appkit-view-operation-fences-synchronous-settlement ()
  (appkit-test-with-view
    (let* ((view (appkit-current-view))
           (cancelled nil)
           (operation
            (appkit-view-operation-begin
             view 'page :cancel-function
             (lambda (object) (push object cancelled)))))
      (should (appkit-view-operation-current-p operation))
      (should (appkit-view-operation-finish operation))
      (should-not (appkit-view-operation-current-p operation))
      (should (eq 'transport
                  (appkit-view-operation-bind operation 'transport)))
      (should-not cancelled)
      (should-not (appkit-view-operation-finish operation)))))

(ert-deftest appkit-view-operation-replacement-cancels-only-old-owner ()
  (appkit-test-with-view
    (let* ((view (appkit-current-view))
           cancelled
           (old
            (appkit-view-operation-begin
             view 'page :cancel-function
             (lambda (object) (push object cancelled)))))
      (appkit-view-operation-bind old 'old-transport)
      (let ((new
             (appkit-view-operation-begin
              view 'page :cancel-function
              (lambda (object) (push object cancelled)))))
        (should (equal cancelled '(old-transport)))
        (should-not (appkit-view-operation-current-p old))
        (should (appkit-view-operation-current-p new))
        (should-not (appkit-view-operation-finish old))
        (should (appkit-view-operation-finish new))))))

(ert-deftest appkit-view-operation-late-bind-honors-view-cancellation ()
  (let* ((app (appkit-start-app 'appkit-test :id 'operation))
         (view
          (appkit-open-view
           :app app :id 'operation :mode 'special-mode
           :buffer-name " *appkit-operation*"))
         (buffer (appkit-view-buffer view))
         cancelled
         (operation
          (appkit-view-operation-begin
           view 'load :cancel-function
           (lambda (object) (push object cancelled)))))
    (unwind-protect
        (progn
          (appkit-kill-view view)
          (should-not (appkit-view-operation-current-p operation))
          (appkit-view-operation-bind operation 'late-transport)
          (should (equal cancelled '(late-transport))))
      (appkit-stop-app app)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest appkit-view-operation-owns-transport-handles-as-children ()
  (appkit-test-with-view
    (let* ((view (appkit-current-view))
           (operation (appkit-view-operation-begin view 'page))
           cancelled
           (transport
            (appkit-register-handle
             operation 'test 'transport
             (lambda (object) (setq cancelled object)))))
      (should (appkit-owner-live-p operation))
      (should (memq transport (appkit-view-operation-handles operation)))
      (should-not (memq transport (appkit-view-handles view)))
      (appkit-view-operation-cancel view 'page)
      (should (eq cancelled 'transport))
      (should-not (appkit-owner-live-p operation))
      (should-not (appkit-view-operation-handles operation)))))

(ert-deftest appkit-view-operation-retires-opaque-child-before-finish ()
  (appkit-test-with-view
    (let* ((view (appkit-current-view))
           (operation
            (appkit-view-operation-begin
             view 'page :cancel-function
             (lambda (_object)
               (ert-fail "Normal settlement cancelled its opaque child")))))
      (appkit-view-operation-bind operation 'transport)
      (should (appkit-view-operation-retire operation 'transport))
      (should-not (appkit-view-operation-handles operation))
      (should (appkit-view-operation-finish operation))
      (should-not
       (appkit-view-operation-retire operation 'transport)))))

(ert-deftest appkit-view-operation-fences-and-cancels-replaced-view-state ()
  (appkit-test-with-view
    (let* ((view (appkit-current-view))
           (state (appkit-view-state view))
           (replacement (list :replacement t))
           (operation (appkit-view-operation-begin view 'page))
           cancelled)
      (appkit-register-handle
       operation 'test 'transport
       (lambda (object) (setq cancelled object)))
      (setf (appkit-view-state view) replacement)
      (should-not (appkit-view-operation-current-p operation))
      (should-not (appkit-view-operation-finish operation))
      (setf (appkit-view-state view) state)
      (should
       (eq view
           (appkit-open-view
            :app (appkit-view-app view)
            :id (appkit-view-id view)
            :mode (appkit-view-mode view)
            :buffer-name (buffer-name (appkit-view-buffer view))
            :state replacement
            :sync-function #'ignore
            :parts nil)))
      (should (eq (appkit-view-state view) replacement))
      (should (eq cancelled 'transport)))))


(ert-deftest appkit-cleanup-items-continue-after-error-and-quit ()
  (let (conditions visited)
    (appkit--run-cleanup-items
     '(first failing quitting last)
     (lambda (item)
       (push item visited)
       (pcase item
         ('failing (error "cleanup failed"))
         ('quitting (signal 'quit nil))))
     (lambda (condition) (push condition conditions)))
    (should (equal (nreverse visited) '(first failing quitting last)))
    (setq conditions (nreverse conditions))
    (should (equal (mapcar #'car conditions) '(error quit)))))

(ert-deftest appkit-cleanup-items-continue-during-throw ()
  (let (visited)
    (should
     (eq
      (catch 'escape
        (appkit--run-cleanup-items
         '(first escaping last)
         (lambda (item)
           (push item visited)
           (when (eq item 'escaping)
             (throw 'escape 'escaped)))
         #'ignore)
        'returned)
      'escaped))
    (should (equal (nreverse visited) '(first escaping last)))))

(provide 'appkit-core-test)

;;; appkit-core-test.el ends here
