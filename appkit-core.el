;;; appkit-core.el --- App and view lifecycle runtime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Runtime ownership for app sessions, buffer views, lifecycle handles, and a
;; small app-local event bus.  Major modes remain initialization functions;
;; attaching or reusing a view never invokes a live buffer's mode again.

;;; Code:

(require 'cl-lib)
(require 'appkit-cleanup)

(defgroup appkit nil
  "Runtime primitives for stateful Emacs buffer applications."
  :group 'applications)

(defcustom appkit-debug nil
  "When non-nil, validate runtime ownership and mutation invariants."
  :type 'boolean
  :group 'appkit)

(defcustom appkit-strict-boundaries nil
  "When non-nil, signal errors for invalid runtime boundaries."
  :type 'boolean
  :group 'appkit)

(cl-defstruct (appkit-handle
               (:constructor appkit-handle--create))
  type
  object
  cancel
  owner
  alive-p)

(cl-defstruct (appkit-app
               (:constructor appkit-app--create))
  id
  kind
  state
  transport
  event-bus
  resource-store
  request-table
  view-registry
  handles
  shutdown
  alive-p)

(cl-defstruct (appkit-view
               (:constructor appkit-view--create))
  id
  app
  buffer
  mode
  state
  engine
  request-table
  invalidations
  pending-events
  handles
  position-policy
  sync-function
  parts
  update-depth
  alive-p)

(cl-defstruct (appkit-view-operation
               (:constructor appkit-view-operation--create))
  "One keyed asynchronous operation owned by an Appkit view."
  view
  view-state
  key
  cancel-function
  handle
  handles
  state)

(defvar appkit--app-kinds (make-hash-table :test #'eq)
  "Registered app kind definitions keyed by symbol.")

(defvar-local appkit--current-view nil
  "Appkit view attached to the current buffer.")

(defvar-local appkit--view-fingerprint nil
  "Persistent identity of the Appkit view owned by this buffer.

The value survives view detachment so a later app session can recover a
renamed buffer.  Changing major mode clears it with the other buffer-local
state because repurposed buffers no longer belong to the former view.")

(defun appkit-register-app-kind (kind options)
  "Register app KIND with evaluated OPTIONS and return KIND."
  (unless (symbolp kind)
    (error "Appkit app kind must be a symbol: %S" kind))
  (puthash kind (copy-sequence options) appkit--app-kinds)
  kind)

(defmacro appkit-define-app-kind (kind &rest options)
  "Define app KIND using evaluated OPTIONS."
  (declare (indent 1))
  `(appkit-register-app-kind ',kind (list ,@options)))

(defun appkit-app-kind-options (kind)
  "Return registered options for app KIND, or nil."
  (gethash kind appkit--app-kinds))

(defun appkit-app-kind-registered-p (kind)
  "Return non-nil when app KIND has been registered."
  (let ((missing (make-symbol "missing")))
    (not (eq (gethash kind appkit--app-kinds missing) missing))))

(cl-defun appkit-start-app (kind &key id state transport shutdown)
  "Create and return a live app session of KIND.

ID identifies one concrete account or backend session.  STATE and TRANSPORT
  remain application-owned values.  SHUTDOWN overrides the app-kind callback."
  (let ((kind-options (appkit-app-kind-options kind)))
    (unless (appkit-app-kind-registered-p kind)
      (error "Appkit app kind is not registered: %S" kind))
    (appkit-app--create
     :id id
     :kind kind
     :state state
     :transport transport
     :event-bus (make-hash-table :test #'equal)
     :resource-store (make-hash-table :test #'equal)
     :request-table (make-hash-table :test #'equal)
     :view-registry (make-hash-table :test #'equal)
     :handles nil
     :shutdown (or shutdown (plist-get kind-options :shutdown))
     :alive-p t)))

(defun appkit-app-live-p (app)
  "Return non-nil when APP is a live app session."
  (and (appkit-app-p app) (appkit-app-alive-p app)))

(defun appkit-current-view ()
  "Return the appkit view attached to the current buffer, or nil."
  (and (appkit-view-p appkit--current-view)
       (eq (current-buffer) (appkit-view-buffer appkit--current-view))
       appkit--current-view))

(defun appkit-view-live-p (view)
  "Return non-nil when VIEW and its owner buffer/app are live."
  (and (appkit-view-p view)
       (appkit-view-alive-p view)
       (buffer-live-p (appkit-view-buffer view))
       (appkit-app-live-p (appkit-view-app view))))

(defun appkit-owner-live-p (owner)
  "Return non-nil when OWNER is a live Appkit app, view, or operation."
  (cond
   ((appkit-app-p owner) (appkit-app-live-p owner))
   ((appkit-view-p owner) (appkit-view-live-p owner))
   ((appkit-view-operation-p owner)
    (appkit-view-operation-current-p owner))
   (t nil)))

(defun appkit-owner-view (owner)
  "Return the Appkit view containing lifecycle OWNER, or nil."
  (cond
   ((appkit-view-p owner) owner)
   ((appkit-view-operation-p owner)
    (appkit-view-operation-view owner))))

(defun appkit-owner-app (owner)
  "Return the Appkit application containing lifecycle OWNER, or nil."
  (cond
   ((appkit-app-p owner) owner)
   ((appkit-owner-view owner)
    (appkit-view-app (appkit-owner-view owner)))))

(defun appkit--owner-handles (owner)
  "Return lifecycle handle list belonging to OWNER."
  (cond
   ((appkit-app-p owner) (appkit-app-handles owner))
   ((appkit-view-p owner) (appkit-view-handles owner))
   ((appkit-view-operation-p owner)
    (appkit-view-operation-handles owner))
   (t (error "Appkit handle owner is invalid: %S" owner))))

(defun appkit--set-owner-handles (owner handles)
  "Set OWNER lifecycle HANDLES."
  (cond
   ((appkit-app-p owner) (setf (appkit-app-handles owner) handles))
   ((appkit-view-p owner) (setf (appkit-view-handles owner) handles))
   ((appkit-view-operation-p owner)
    (setf (appkit-view-operation-handles owner) handles))
   (t (error "Appkit handle owner is invalid: %S" owner))))

(defun appkit--default-handle-cancel (type object)
  "Cancel lifecycle OBJECT according to TYPE."
  (pcase type
    ('timer
     (when (timerp object) (cancel-timer object)))
    ('process
     (when (processp object)
       (set-process-filter object nil)
       (set-process-sentinel object nil)
       (when (process-live-p object) (delete-process object))))
    ('hook
     (pcase-let ((`(,hook ,function ,local ,buffer) object))
       (if (buffer-live-p buffer)
           (with-current-buffer buffer
             (remove-hook hook function local))
         (remove-hook hook function local))))
    ('event-subscription
     (when (functionp object) (funcall object)))
    ('function
     (when (functionp object) (funcall object)))))

(cl-defun appkit-register-handle (owner type object &optional cancel-function)
  "Register lifecycle OBJECT of TYPE under OWNER.

CANCEL-FUNCTION, when non-nil, receives OBJECT.  OWNER may be a live Appkit
application, view, or keyed view operation."
  (unless (appkit-owner-live-p owner)
    (error "Cannot register handle under dead appkit owner"))
  (let ((handle (appkit-handle--create
                 :type type
                 :object object
                 :cancel cancel-function
                 :owner owner
                 :alive-p t)))
    (appkit--set-owner-handles owner
                               (cons handle (appkit--owner-handles owner)))
    handle))

(defun appkit-retire-handle (handle)
  "Retire lifecycle HANDLE without invoking its cancellation side effect."
  (when (and (appkit-handle-p handle) (appkit-handle-alive-p handle))
    (setf (appkit-handle-alive-p handle) nil)
    (let ((owner (appkit-handle-owner handle)))
      (when (or (appkit-app-p owner)
                (appkit-view-p owner)
                (appkit-view-operation-p owner))
        (appkit--set-owner-handles
         owner (delq handle (appkit--owner-handles owner)))))
    t))

(defun appkit-cancel-handle (handle)
  "Cancel HANDLE exactly once."
  (when (and (appkit-handle-p handle) (appkit-handle-alive-p handle))
    (setf (appkit-handle-alive-p handle) nil)
    (unwind-protect
        (let ((cancel (appkit-handle-cancel handle))
              (object (appkit-handle-object handle)))
          (if (functionp cancel)
              (funcall cancel object)
            (appkit--default-handle-cancel (appkit-handle-type handle) object)))
      (let ((owner (appkit-handle-owner handle)))
        (when (or (appkit-app-p owner)
                  (appkit-view-p owner)
                  (appkit-view-operation-p owner))
          (appkit--set-owner-handles
           owner (delq handle (appkit--owner-handles owner))))))
    t))

(defun appkit-view-operation--current-entry-p (operation)
  "Return non-nil when OPERATION occupies its view request slot."
  (and (appkit-view-operation-p operation)
       (eq operation
           (gethash
            (appkit-view-operation-key operation)
            (appkit-view-request-table
             (appkit-view-operation-view operation))))))

(defun appkit-view-operation-current-p (operation)
  "Return non-nil when OPERATION may still update its owning view."
  (and (appkit-view-operation-p operation)
       (eq (appkit-view-operation-state operation) 'active)
       (appkit-view-live-p (appkit-view-operation-view operation))
       (eq (appkit-view-operation-view-state operation)
           (appkit-view-state (appkit-view-operation-view operation)))
       (appkit-view-operation--current-entry-p operation)))

(defun appkit-view-operation--cancel-object (operation object)
  "Cancel OBJECT through OPERATION's transport cancellation boundary."
  (when (and object
             (functionp (appkit-view-operation-cancel-function operation)))
    (funcall (appkit-view-operation-cancel-function operation) object)))

(defun appkit-view-operation--cancel-owned (operation)
  "Cancel lifecycle-owned OPERATION without recursing through its handle."
  (when (and (appkit-view-operation-p operation)
             (eq (appkit-view-operation-state operation) 'active))
    (when (appkit-view-operation--current-entry-p operation)
      (remhash
       (appkit-view-operation-key operation)
       (appkit-view-request-table (appkit-view-operation-view operation))))
    (setf (appkit-view-operation-state operation) 'cancelled)
    (appkit-cancel-handles operation)
    t))

(defun appkit-view-operation-cancel (view key)
  "Cancel VIEW's current asynchronous operation under semantic KEY."
  (unless (appkit-view-p view)
    (signal 'wrong-type-argument (list 'appkit-view-p view)))
  (when-let* ((operation (gethash key (appkit-view-request-table view))))
    (unless (appkit-view-operation-p operation)
      (error "Appkit view request slot %S contains a foreign value" key))
    (or (appkit-cancel-handle (appkit-view-operation-handle operation))
        (appkit-view-operation--cancel-owned operation))))

(defun appkit-view-operation-cancel-all (view)
  "Cancel every keyed asynchronous operation currently owned by VIEW."
  (unless (appkit-view-p view)
    (signal 'wrong-type-argument (list 'appkit-view-p view)))
  (let (keys)
    (maphash
     (lambda (key _operation) (push key keys))
     (appkit-view-request-table view))
    (dolist (key keys)
      (appkit-view-operation-cancel view key))))

(cl-defun appkit-view-operation-begin
    (view key &key cancel-function)
  "Begin and return VIEW's newest asynchronous operation under KEY.

An existing operation under KEY is canceled first.  CANCEL-FUNCTION, when
non-nil, receives the transport object later supplied to
`appkit-view-operation-bind'."
  (unless (appkit-view-live-p view)
    (error "Cannot begin an operation for a dead Appkit view"))
  (unless (or (null cancel-function) (functionp cancel-function))
    (error "Appkit operation cancellation is not callable: %S"
           cancel-function))
  (appkit-view-operation-cancel view key)
  (let* ((operation
          (appkit-view-operation--create
           :view view
           :view-state (appkit-view-state view)
           :key key
           :cancel-function cancel-function
           :handles nil
           :state 'active))
         (handle
          (appkit-register-handle
           view 'view-operation operation
           #'appkit-view-operation--cancel-owned)))
    (setf (appkit-view-operation-handle operation) handle)
    (puthash key operation (appkit-view-request-table view))
    operation))

(cl-defun appkit-view-operation-bind
    (operation object &key (cancel-function nil cancel-function-p))
  "Bind transport OBJECT as OPERATION's sole lifecycle child.

When CANCEL-FUNCTION is supplied, replace the operation's cancellation
boundary.  Binding a new child cancels the old child.  If OPERATION was
canceled before transport startup returned, cancel OBJECT immediately.  If it
already finished normally, leave OBJECT alone."
  (unless (appkit-view-operation-p operation)
    (signal 'wrong-type-argument
            (list 'appkit-view-operation-p operation)))
  (when cancel-function-p
    (unless (or (null cancel-function) (functionp cancel-function))
      (error "Appkit operation cancellation is not callable: %S"
             cancel-function))
    (setf (appkit-view-operation-cancel-function operation) cancel-function))
  (pcase (appkit-view-operation-state operation)
    ('active
     (appkit-cancel-handles operation)
     (when object
       (appkit-register-handle
        operation 'function object
        (appkit-view-operation-cancel-function operation))))
    ('cancelled
     (appkit-view-operation--cancel-object operation object)))
  object)

(defun appkit-view-operation-retire (operation object)
  "Retire OPERATION's child matching OBJECT without cancelling it.

Return non-nil when a live child was found.  This is the normal-completion
boundary for opaque transports that cannot retire their Appkit handle
themselves before publishing a callback."
  (unless (appkit-view-operation-p operation)
    (signal 'wrong-type-argument
            (list 'appkit-view-operation-p operation)))
  (when-let* ((handle
               (cl-find
                object (appkit-view-operation-handles operation)
                :key #'appkit-handle-object :test #'eq)))
    (appkit-retire-handle handle)))

(defun appkit-view-operation-finish (operation)
  "Finish current OPERATION and return non-nil when it owned settlement.

The operation must still occupy its semantic key and its view must retain the
exact state captured by `appkit-view-operation-begin'.  Stale or duplicate
completions are inert and return nil."
  (when (appkit-view-operation-current-p operation)
    (remhash
     (appkit-view-operation-key operation)
     (appkit-view-request-table (appkit-view-operation-view operation)))
    (setf (appkit-view-operation-state operation) 'finished)
    (appkit-cancel-handles operation)
    (setf (appkit-view-operation-cancel-function operation) nil)
    (appkit-retire-handle (appkit-view-operation-handle operation))
    t))


(defun appkit-cancel-handles (owner)
  "Cancel and forget all lifecycle handles owned by OWNER."
  (let ((handles (appkit--owner-handles owner)))
    (appkit--set-owner-handles owner nil)
    (appkit--run-cleanup-items
     handles #'appkit-cancel-handle
     (lambda (err)
       (message "appkit: handle cleanup failed: %s"
                (error-message-string err))))))

(defun appkit-view-for-id (app id)
  "Return APP's live view identified by ID, or nil."
  (when (appkit-app-live-p app)
    (let* ((registry (appkit-app-view-registry app))
           (view (gethash id registry)))
      (cond
       ((and (appkit-view-live-p view)
             (eq app (appkit-view-app view))
             (equal id (appkit-view-id view))
             (with-current-buffer (appkit-view-buffer view)
               (eq appkit--current-view view)))
        view)
       ((and (appkit-view-live-p view)
             (or (not (eq app (appkit-view-app view)))
                 (not (equal id (appkit-view-id view)))))
        ;; Do not destroy a foreign or wrong-id live view merely because a
        ;; corrupt alias points at it; only remove this registry key.
        (remhash id registry)
        nil)
       ((appkit-view-p view)
        ;; A dead buffer can make a nominally alive view fail the live check
        ;; before its lifecycle handles were retired.  Reap it before a later
        ;; attachment overwrites the registry entry.
        (appkit-kill-view view)
        (when (eq view (gethash id registry))
          (remhash id registry))
        nil)
       (view
        (remhash id registry)
        nil)))))

(defun appkit--view-fingerprint-for (app id)
  "Return the persistent buffer fingerprint for APP's view ID."
  (list (appkit-app-kind app) (appkit-app-id app) id))

(defun appkit--detached-buffer-for-view (app id &optional allowed-live-view)
  "Return the unique detached buffer belonging to APP's view ID, or nil.

ALLOWED-LIVE-VIEW is the registry-owned view already selected by the caller;
any other live buffer with the same fingerprint is a collision."
  (let ((fingerprint (appkit--view-fingerprint-for app id))
        detached
        live)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal appkit--view-fingerprint fingerprint)
          (let ((allowed-owner-p
                 (and allowed-live-view
                      (eq appkit--current-view allowed-live-view)
                      (eq buffer (appkit-view-buffer allowed-live-view)))))
            (if (and (appkit-view-live-p appkit--current-view)
                     (not allowed-owner-p))
              (push buffer live)
              (unless allowed-owner-p
                (push buffer detached)))))))
    (cond
     (live
      (error "Appkit view fingerprint %S already belongs to a live buffer"
             fingerprint))
     ((and allowed-live-view detached)
      (error "Appkit view fingerprint %S belongs to a live and detached buffer"
             fingerprint))
     ((cdr detached)
      (error "Appkit view fingerprint %S belongs to multiple detached buffers"
             fingerprint))
     (detached (car detached)))))

(defun appkit--fallback-buffer-for-view (app id buffer-name)
  "Return a safe fallback buffer named BUFFER-NAME for APP's view ID.

A buffer carrying another view's persistent fingerprint remains that view's
property even when it happens to have the requested fallback name.  Allocate a
unique presentation name whether that foreign view is live or detached."
  (let ((named (get-buffer buffer-name))
        (fingerprint (appkit--view-fingerprint-for app id)))
    (if (and named
             (with-current-buffer named
               (and appkit--view-fingerprint
                    (not (equal appkit--view-fingerprint fingerprint)))))
        (generate-new-buffer buffer-name)
      (get-buffer-create buffer-name))))

(defun appkit--assert-current-buffer-owns-fingerprint (app id)
  "Assert that the current buffer can exclusively own APP's view ID."
  (let ((current (current-buffer))
        (fingerprint (appkit--view-fingerprint-for app id)))
    (when (and appkit--view-fingerprint
               (not (equal appkit--view-fingerprint fingerprint)))
      (error "Buffer retains another Appkit view fingerprint: %S"
             appkit--view-fingerprint))
    (dolist (buffer (buffer-list))
      (unless (eq buffer current)
        (with-current-buffer buffer
          (when (equal appkit--view-fingerprint fingerprint)
            (error "Appkit view fingerprint %S already belongs to another buffer"
                   fingerprint)))))))

(defun appkit--detach-current-view ()
  "Detach the appkit view belonging to the current buffer."
  (when (appkit-view-p appkit--current-view)
    (if (eq (appkit-view-buffer appkit--current-view) (current-buffer))
        (appkit-kill-view appkit--current-view)
      ;; Indirect buffers copy buffer-local variables and hooks.  A copied
      ;; alias must never tear down the real owner view.
      (setq-local appkit--current-view nil
                  appkit--view-fingerprint nil))))

(defun appkit--change-major-mode-detach-current-view ()
  "Forget the current buffer's view identity before changing major mode."
  (setq-local appkit--view-fingerprint nil)
  (appkit--detach-current-view))

(cl-defun appkit-attach-view
    (&key app id state mode sync-function parts position-policy)
  "Attach a new appkit view for APP and ID to the current buffer.

STATE is application-owned view state.  MODE records the view's major mode.
SYNC-FUNCTION accepts (VIEW INVALIDATIONS EVENTS) and synchronizes PARTS.
Appkit acknowledges the supplied event snapshot only after it returns normally.
POSITION-POLICY controls how render updates preserve the current position."
  (unless (appkit-app-live-p app)
    (error "Cannot attach a view to a dead appkit app"))
  (appkit--assert-current-buffer-owns-fingerprint app id)
  (when (and (appkit-view-p appkit--current-view)
             (appkit-view-live-p appkit--current-view))
    (unless (and (eq app (appkit-view-app appkit--current-view))
                 (equal id (appkit-view-id appkit--current-view))
                 (eq (current-buffer)
                     (appkit-view-buffer appkit--current-view)))
      (error "Buffer already belongs to another live appkit view"))
    (unless (eq state (appkit-view-state appkit--current-view))
      (appkit-view-operation-cancel-all appkit--current-view))
    (setf (appkit-view-state appkit--current-view) state
          (appkit-view-sync-function appkit--current-view) sync-function
          (appkit-view-parts appkit--current-view) (copy-sequence parts)
          (appkit-view-position-policy appkit--current-view) position-policy)
    (setq-local appkit--view-fingerprint
                (appkit--view-fingerprint-for app id))
    (cl-return-from appkit-attach-view appkit--current-view))
  (let ((existing (appkit-view-for-id app id)))
    (when (and existing
               (not (eq (appkit-view-buffer existing) (current-buffer))))
      (error "Appkit view id %S is already attached to another buffer" id)))
  (let ((view (appkit-view--create
               :id id
               :app app
               :buffer (current-buffer)
               :mode (or mode major-mode)
               :state state
               :engine nil
               :request-table (make-hash-table :test #'equal)
               :invalidations nil
               :pending-events nil
               :handles nil
               :position-policy position-policy
               :sync-function sync-function
               :parts (copy-sequence parts)
               :update-depth 0
               :alive-p t)))
    (setq-local appkit--current-view view)
    (setq-local appkit--view-fingerprint
                (appkit--view-fingerprint-for app id))
    (puthash id view (appkit-app-view-registry app))
    (add-hook 'change-major-mode-hook
              #'appkit--change-major-mode-detach-current-view nil t)
    (add-hook 'kill-buffer-hook #'appkit--detach-current-view nil t)
    view))

(cl-defun appkit-open-view
    (&key app id mode buffer-name state sync-function parts position-policy
          setup select)
  "Open or reuse one appkit view.

APP and ID identify the view, and BUFFER-NAME names its fallback buffer.  MODE
initializes a buffer exactly once.  STATE, SYNC-FUNCTION, PARTS, and
POSITION-POLICY configure the attached view.  SETUP runs only after a new view
is attached.  SELECT non-nil displays the resulting buffer."
  (unless (and (symbolp mode) (functionp mode))
    (error "Appkit view MODE must name a major mode function"))
  (unless (appkit-app-live-p app)
    (error "Cannot open a view for a dead appkit app"))
  (let* ((existing (appkit-view-for-id app id))
         (detached (appkit--detached-buffer-for-view app id existing))
         (buffer (or (and existing (appkit-view-buffer existing))
                     detached
                     (appkit--fallback-buffer-for-view app id buffer-name)))
         view)
    (with-current-buffer buffer
      (when (and (appkit-view-live-p appkit--current-view)
                 (not (eq appkit--current-view existing)))
        (error "Buffer already belongs to another live appkit view"))
      (unless (eq major-mode mode)
        (funcall mode))
      (setq view
            (appkit-attach-view
             :app app :id id :state state :mode mode
             :sync-function sync-function :parts parts
             :position-policy position-policy))
      (setf (appkit-view-state view) state
            (appkit-view-sync-function view) sync-function
            (appkit-view-parts view) (copy-sequence parts))
      (when (and (not (eq view existing)) (functionp setup))
        (funcall setup view)))
    (when select (pop-to-buffer buffer))
    view))

(defun appkit-kill-view (view &optional kill-buffer)
  "Detach VIEW and optionally KILL-BUFFER."
  (when (appkit-view-p view)
    (let ((buffer (appkit-view-buffer view))
          (app (appkit-view-app view))
          (id (appkit-view-id view)))
      (when (appkit-view-alive-p view)
        (setf (appkit-view-alive-p view) nil)
        (unwind-protect
            (appkit-cancel-handles view)
          (when (and (appkit-app-p app)
                     (eq view (gethash id (appkit-app-view-registry app))))
            (remhash id (appkit-app-view-registry app)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (when (eq appkit--current-view view)
                (setq-local appkit--current-view nil))))))
      (when (and kill-buffer (buffer-live-p buffer))
        (kill-buffer buffer))
      t)))

(defun appkit-stop-app (app)
  "Stop APP, detach its views, and cancel owned handles."
  (when (appkit-app-live-p app)
    (setf (appkit-app-alive-p app) nil)
    (let (views actions first-error)
      (maphash (lambda (_id view)
                 (when (and (appkit-view-p view)
                            (eq app (appkit-view-app view)))
                   (cl-pushnew view views :test #'eq)))
               (appkit-app-view-registry app))
      (setq actions
            (append
             (mapcar (lambda (view)
                       (lambda () (appkit-kill-view view)))
                     views)
             (list
              (lambda () (clrhash (appkit-app-view-registry app)))
              (lambda () (appkit-cancel-handles app)))
             (when-let* ((shutdown (appkit-app-shutdown app)))
               (list (lambda () (funcall shutdown app))))))
      (appkit--run-cleanup-items
       actions #'funcall
       (lambda (err)
         (if first-error
             (message "appkit: additional app shutdown failure: %s"
                      (error-message-string err))
           (setq first-error err))))
      (when first-error
        (signal (car first-error) (cdr first-error)))
      t)))

(defmacro appkit-with-live-view (view &rest body)
  "Evaluate BODY when VIEW is live, with its buffer current."
  (declare (indent 1) (debug t))
  `(let ((appkit-view-value ,view))
     (when (appkit-view-live-p appkit-view-value)
       (with-current-buffer (appkit-view-buffer appkit-view-value)
         ,@body))))

(defun appkit-view-enqueue-event (view event)
  "Append EVENT to live VIEW's synchronization queue."
  (when (appkit-view-live-p view)
    (setf (appkit-view-pending-events view)
          (append (appkit-view-pending-events view) (list event)))
    event))

(defun appkit-view-pending-events-snapshot (view)
  "Return a shallow snapshot of synchronization events queued for VIEW."
  (when (appkit-view-live-p view)
    (copy-sequence (appkit-view-pending-events view))))

(defun appkit-view-acknowledge-events (view count)
  "Forget the first COUNT successfully synchronized events from VIEW."
  (unless (and (integerp count) (>= count 0))
    (error "Appkit event acknowledgement count is invalid: %S" count))
  (when (appkit-view-live-p view)
    (setf (appkit-view-pending-events view)
          (nthcdr count (appkit-view-pending-events view)))))

(defun appkit-app-on (app event function)
  "Subscribe FUNCTION to APP EVENT and return a lifecycle handle."
  (unless (and (appkit-app-live-p app) (functionp function))
    (error "Cannot subscribe a dead app or non-function"))
  (let* ((bus (appkit-app-event-bus app))
         (functions (gethash event bus)))
    (puthash event (cons function (delq function functions)) bus)
    (appkit-register-handle
     app 'event-subscription
     (lambda ()
       (puthash event (delq function (gethash event bus)) bus)))))

(defun appkit-app-emit (app event &rest arguments)
  "Emit APP EVENT with ARGUMENTS."
  (when (appkit-app-live-p app)
    (dolist (function (copy-sequence
                       (gethash event (appkit-app-event-bus app))))
      (apply function arguments))))

(provide 'appkit-core)

;;; appkit-core.el ends here
