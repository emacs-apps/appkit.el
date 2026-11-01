;;; appkit-operation-test.el --- Tests for keyed App operations -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'appkit-app)
(require 'appkit-operation)
(require 'appkit-surface)

(defvar appkit-operation-test--registry (make-hash-table :test #'eq))
(defvar appkit-operation-test--fetches nil
  "Pending fake fetches, newest first, as (STEP RESOLVE REJECT).")

(appkit-define-operation appkit-operation-test--registry probe (id)
  :fetch (progn (push (list id resolve reject) appkit-operation-test--fetches) nil)
  :commit (list id result))

(appkit-define-operation appkit-operation-test--registry save (id value)
  :write t :key (list 'save id)
  :fetch (progn (push (list value resolve reject) appkit-operation-test--fetches) nil))

(appkit-define-operation appkit-operation-test--registry merge (id value)
  :write t :key (list 'merge id)
  :coalesce (lambda (_new _queued) t)
  :fetch (progn (push (list value resolve reject) appkit-operation-test--fetches) nil))

(appkit-define-operation appkit-operation-test--registry batch (items)
  :steps (mapcar (lambda (item) (list 'batch (list item))) items)
  :fetch (progn (push (list (car items) resolve reject) appkit-operation-test--fetches) nil)
  :next (unless reason '((probe "after"))))

(defmacro appkit-operation-test-with-app (&rest body)
  "Run BODY with `app', an App holding operation entries as its model."
  (declare (indent 0) (debug t))
  `(let ((appkit-operation-test--fetches nil)
         (app (appkit-app-start
               (appkit-app-type-create
                :name 'appkit-operation-test
                :init (lambda (_context _input)
                        (appkit-next :model appkit-data-map-empty
                                     :render appkit-render-none))
                :update
                (lambda (context model message)
                  (if-let* ((result (appkit-operation-update
                                     appkit-operation-test--registry model context message)))
                      (appkit-next :model (car result) :render appkit-render-none
                                   :commands (cdr result))
                    (appkit-next :model model :render appkit-render-none))))
               :input nil)))
     (unwind-protect (progn ,@body)
       (appkit-app-close app))))

(defmacro appkit-operation-test-with-surface (binding &rest body)
  "Run BODY with a Surface named by BINDING attached to `app'."
  (declare (indent 1) (debug ((symbolp) body)))
  `(let ((,(car binding)
          (appkit-open-generated-surface
           (appkit-surface-type-create
            :name 'appkit-operation-test
            :mode #'fundamental-mode
            :init (lambda (_context _input)
                    (appkit-next :model nil :render appkit-render-none))
            :update
            (lambda (context model message)
              (or (appkit-operation-surface-update context model message)
                  (appkit-next :model (cons message model) :render appkit-render-none)))
            :renderer-factory
            (lambda (_surface)
              (appkit-generated-renderer-create :mount #'ignore :unmount #'ignore)))
           :app app :identity (make-symbol "operation-test"))))
     (unwind-protect (progn ,@body)
       (kill-buffer (appkit-surface-buffer ,(car binding))))))

(defun appkit-operation-test--submit (app request &optional refresh)
  "Submit REQUEST to APP and settle its passes."
  (appkit-operation-submit appkit-operation-test--registry (appkit-app-model app)
                           app request refresh)
  (appkit-operation-test--drain app))

(defun appkit-operation-test--drain (app &rest surfaces)
  "Drain APP and SURFACES until all routed messages have settled."
  (let ((loops (cons (appkit-app-loop app) (mapcar #'appkit-surface-loop surfaces))))
    (while (cl-some (lambda (loop) (> (appkit-loop-pending-count loop) 0)) loops)
      (dolist (loop loops)
        (when (> (appkit-loop-pending-count loop) 0)
          (appkit-loop-run-pass loop))))))

(defun appkit-operation-test--settle (app fetch &optional reason)
  "Resolve FETCH with its own step value, or reject it with REASON."
  (if reason (funcall (nth 2 fetch) nil reason) (funcall (nth 1 fetch) (car fetch)))
  (appkit-operation-test--drain app))

(defun appkit-operation-test--entry (app key)
  (appkit-data-map-get (appkit-app-model app) key))

(ert-deftest appkit-operation-reads-join-and-reuse-ready-data ()
  (appkit-operation-test-with-app
    (appkit-operation-test-with-surface (surface)
      (appkit-operation-test-with-surface (peer)
        (dolist (recipient (list (cons surface '(view 1)) (cons peer '(view 2))))
          (appkit-operation-submit appkit-operation-test--registry (appkit-app-model app)
                                   (car recipient) '(probe "x") nil (cdr recipient)))
        (appkit-operation-test--drain app surface peer)
        (should (= 1 (length appkit-operation-test--fetches)))
        (appkit-operation-test--settle app (car appkit-operation-test--fetches))
        (appkit-operation-test--drain app surface peer)
        (should (equal (appkit-surface-model surface)
                       '((appkit-operation-completed (view 1) (probe "x") ("x" "x") nil))))
        (should (equal (appkit-surface-model peer)
                       '((appkit-operation-completed (view 2) (probe "x") ("x" "x") nil))))
        ;; A ready value still returns through the requesting Surface's update.
        (appkit-operation-submit appkit-operation-test--registry (appkit-app-model app)
                                 peer '(probe "x") nil '(cached 3))
        (appkit-operation-test--drain app surface peer)
        (should (= 1 (length appkit-operation-test--fetches)))
        (should (equal (car (appkit-surface-model peer))
                       '(appkit-operation-completed (cached 3) (probe "x") ("x" "x") nil)))
        (should (= 1 (length (appkit-surface-model surface))))))))

(ert-deftest appkit-operation-refresh-revokes-the-replaced-fetch ()
  (appkit-operation-test-with-app
    (appkit-operation-test-with-surface (surface)
      (appkit-operation-test-with-surface (peer)
        (appkit-operation-submit appkit-operation-test--registry (appkit-app-model app)
                                 surface '(probe "x") nil 'old)
        (appkit-operation-test--drain app surface peer)
        (let ((old (car appkit-operation-test--fetches)))
          (appkit-operation-submit appkit-operation-test--registry (appkit-app-model app)
                                   peer '(probe "x") t 'replacement)
          (appkit-operation-test--drain app surface peer)
          (should (= 2 (length appkit-operation-test--fetches)))
          (should (equal (appkit-surface-model surface)
                         '((appkit-operation-completed old (probe "x") nil
                                                       "Request superseded"))))
          ;; A replaced fetch cannot commit or reply after its Effect is revoked.
          (funcall (nth 1 old) "stale")
          (appkit-operation-test--drain app surface peer)
          (should (plist-get (appkit-operation-test--entry app '(probe "x")) :pending))
          (should-not (appkit-surface-model peer))
          (appkit-operation-test--settle app (car appkit-operation-test--fetches) "down")
          (appkit-operation-test--drain app surface peer)
          (should (equal (appkit-surface-model peer)
                         '((appkit-operation-completed replacement (probe "x") nil "down"))))
          (should (= 1 (length (appkit-surface-model surface)))))))))

(ert-deftest appkit-operation-write-lane-refuses-or-coalesces ()
  (appkit-operation-test-with-app
    (appkit-operation-test--submit app '(save "a" 1))
    (should-error (appkit-operation-test--submit app '(save "a" 2)) :type 'user-error)
    (appkit-operation-test--settle app (car appkit-operation-test--fetches))
    (setq appkit-operation-test--fetches nil)
    ;; A coalescing lane keeps only the newest follow-up and runs it next.
    (appkit-operation-test--submit app '(merge "a" 1))
    (appkit-operation-test--submit app '(merge "a" 2))
    (appkit-operation-test--submit app '(merge "a" 3))
    (should (equal (mapcar #'car appkit-operation-test--fetches) '(1)))
    (appkit-operation-test--settle app (car appkit-operation-test--fetches))
    (should (equal (mapcar #'car appkit-operation-test--fetches) '(3 1)))))

(ert-deftest appkit-operation-steps-run-in-turn-then-follow-up ()
  (appkit-operation-test-with-app
    (appkit-operation-test--submit app '(batch (a b)))
    (should (equal (mapcar #'car appkit-operation-test--fetches) '(a)))
    (appkit-operation-test--settle app (car appkit-operation-test--fetches))
    (should (equal (mapcar #'car appkit-operation-test--fetches) '(b a)))
    (should (= 1 (plist-get (appkit-operation-test--entry app '(batch (a b))) :completed)))
    (appkit-operation-test--settle app (car appkit-operation-test--fetches))
    ;; Settlement starts the declared follow-up read.
    (should (equal (car (car appkit-operation-test--fetches)) "after"))))

(ert-deftest appkit-operation-failed-step-stops-and-keeps-old-value ()
  (appkit-operation-test-with-app
    (appkit-operation-test--submit app '(batch (a b)))
    (appkit-operation-test--settle app (car appkit-operation-test--fetches) "down")
    (let ((entry (appkit-operation-test--entry app '(batch (a b)))))
      (should (equal (plist-get entry :error) "down"))
      (should (= 0 (plist-get entry :completed)))
      (should-not (plist-get entry :pending)))
    (should (= 1 (length appkit-operation-test--fetches)))))





(ert-deftest appkit-operation-named-input-and-entry-snapshots-survive-settlement ()
  (appkit-operation-test-with-app
    (let* ((command (list 'probe "original"))
           (empty (appkit-app-model app)))
      (appkit-operation-test--submit app command)
      (let* ((pending (appkit-app-model app))
             (entry (appkit-data-map-get pending '(probe "original")))
             (request (plist-get entry :request))
             (other (appkit-data-with request ((:input) :id "other"))))
        (setcar (cdr command) "changed-command")
        (should (appkit-operation-request-p request))
        (should (equal "original" (appkit-operation-input request :id)))
        (should (equal '(probe "other") (appkit-operation-key appkit-operation-test--registry other)))
        (should (= 0 (appkit-data-map-count empty)))
        (appkit-operation-test--settle app (car appkit-operation-test--fetches))
        (let ((ready (appkit-app-model app)))
          (should (plist-get entry :pending))
          (should-not (plist-get entry :ready))
          (should (equal '("original" "original")
                         (plist-get (appkit-data-map-get ready '(probe "original")) :value)))
          (appkit-app-send app '(appkit-operation-forget (probe)))
          (appkit-operation-test--drain app)
          (should (= 0 (appkit-data-map-count (appkit-app-model app))))
          (should (= 1 (appkit-data-map-count ready))))))))

(ert-deftest appkit-operation-declared-input-rejects-missing-unknown-and-duplicate-fields ()
  (let* ((registry appkit-operation-test--registry)
         (request (appkit-operation-request-create registry 'probe :id "named")))
    (should (equal '(probe "named") (appkit-operation-key registry request)))
    (should-error (appkit-operation-request-create registry 'probe))
    (should-error (appkit-operation-request-create registry 'probe :other "named"))
    (should-error (appkit-operation-request-create registry 'probe :id "one" :id "two"))
    (should-error (appkit-operation-key registry (appkit-data-with request ((:input) :unknown t))))
    (should-error (appkit-operation-key registry '(probe "one" "extra")))))

(provide 'appkit-operation-test)
;;; appkit-operation-test.el ends here
