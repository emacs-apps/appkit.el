;;; appkit-operation-test.el --- Tests for keyed App operations -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'appkit-app)
(require 'appkit-operation)

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
                        (appkit-next :model (make-hash-table :test #'equal)
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

(defun appkit-operation-test--submit (app request &optional refresh)
  "Submit REQUEST to APP and settle its passes."
  (appkit-operation-submit appkit-operation-test--registry (appkit-app-model app)
                           app request refresh)
  (appkit-operation-test--drain app))

(defun appkit-operation-test--drain (app)
  (while (> (appkit-loop-pending-count (appkit-app-loop app)) 0)
    (appkit-loop-run-pass (appkit-app-loop app))))

(defun appkit-operation-test--settle (app fetch &optional reason)
  "Resolve FETCH with its own step value, or reject it with REASON."
  (if reason (funcall (nth 2 fetch) nil reason) (funcall (nth 1 fetch) (car fetch)))
  (appkit-operation-test--drain app))

(defun appkit-operation-test--entry (app key)
  (gethash key (appkit-app-model app)))

(ert-deftest appkit-operation-reads-join-and-reuse-ready-data ()
  (appkit-operation-test-with-app
    (appkit-operation-test--submit app '(probe "x"))
    (appkit-operation-test--submit app '(probe "x"))
    (should (= 1 (length appkit-operation-test--fetches)))
    (appkit-operation-test--settle app (car appkit-operation-test--fetches))
    (should (equal (plist-get (appkit-operation-test--entry app '(probe "x")) :value)
                   '("x" "x")))
    (appkit-operation-test--submit app '(probe "x"))
    (should (= 1 (length appkit-operation-test--fetches)))))

(ert-deftest appkit-operation-refresh-revokes-the-replaced-fetch ()
  (appkit-operation-test-with-app
    (appkit-operation-test--submit app '(probe "x"))
    (let ((old (car appkit-operation-test--fetches)))
      (appkit-operation-test--submit app '(probe "x") t)
      (should (= 2 (length appkit-operation-test--fetches)))
      ;; The replaced Effect is revoked, so its late result never commits.
      (funcall (nth 1 old) "stale")
      (appkit-operation-test--drain app)
      (should (plist-get (appkit-operation-test--entry app '(probe "x")) :pending))
      (appkit-operation-test--settle app (car appkit-operation-test--fetches))
      (should-not (plist-get (appkit-operation-test--entry app '(probe "x")) :pending)))))

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

(provide 'appkit-operation-test)

;;; appkit-operation-test.el ends here
