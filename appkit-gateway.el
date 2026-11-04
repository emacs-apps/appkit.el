;;; appkit-gateway.el --- Local service connections -*- lexical-binding: t; -*-

;;; Commentary:
;; A connection owns its handshake, bounded request queue and reconnect timers.
;; Cancelling a request retires the local observer; sent mutations belong to
;; the service and are never replayed by this transport.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'websocket)

(cl-defstruct (appkit-gateway (:constructor appkit-gateway-create) (:conc-name appkit-gateway--))
  websocket-url auth-token-file (client-name "emacs") (protocol-version 1)
  event-hook protocol-error-hook state-hook
  (timeout 30) (handshake-timeout 15) (reconnect-delay 3) reconnect-max-attempts
  ws connecting connection-owner stopping
  (pending (make-hash-table :test #'equal))
  (in-flight (make-hash-table :test #'equal)) (max-in-flight-requests 32)
  send-queue send-queue-tail draining (request-counter 0)
  reconnect-timer (reconnect-attempt 0) ready-timer (state 'stopped)
  hello-instance-id gateway-instance-id capabilities ready-accounts)

(defconst appkit-gateway-wire-null (make-symbol "appkit-gateway-wire-null")
  "Private value used to represent JSON null at the service boundary.")

(defun appkit-gateway-wire-null-p (value)
  "Return non-nil when VALUE is the service JSON null sentinel."
  (eq value appkit-gateway-wire-null))

(defun appkit-gateway-wire-object-p (value)
  "Return non-nil when VALUE has the shape of a decoded JSON object."
  (and (listp value)
       (cl-every (lambda (entry)
                   (and (consp entry) (symbolp (car entry))))
                 value)))

(defun appkit-gateway-wire-exact-object-keys-p (object keys)
  "Return non-nil when decoded OBJECT has exactly symbol KEYS."
  (and (appkit-gateway-wire-object-p object)
       (equal
        (sort (mapcar #'car object)
              (lambda (left right)
                (string-lessp (symbol-name left) (symbol-name right))))
        (sort (copy-sequence keys)
              (lambda (left right)
                (string-lessp (symbol-name left) (symbol-name right)))))))

(defun appkit-gateway-wire--copy (value domain-p)
  "Recursively copy VALUE.

When DOMAIN-P is non-nil, turn wire null into nil and wire vectors into
lists.  Supported compound values are conses, vectors, and hash tables."
  (cond
   ((and domain-p (appkit-gateway-wire-null-p value)) nil)
   ((stringp value) (copy-sequence value))
   ((consp value)
    ;; Member directories are wide, not deeply nested.  Walk each list spine
    ;; iteratively so a large group does not consume one Lisp frame per row.
    (let* ((head (list nil))
           (tail head)
           (rest value))
      (while (consp rest)
        (setcdr tail (list (appkit-gateway-wire--copy (car rest) domain-p)))
        (setf tail (cdr tail)
              rest (cdr rest)))
      (setcdr tail (appkit-gateway-wire--copy rest domain-p))
      (cdr head)))
   ((vectorp value)
    (let ((items (mapcar (lambda (item)
                           (appkit-gateway-wire--copy item domain-p))
                         (append value nil))))
      (if domain-p items (vconcat items))))
   ((hash-table-p value)
    (let ((copy (make-hash-table
                 :test (hash-table-test value)
                 :size (hash-table-size value)
                 :rehash-size (hash-table-rehash-size value)
                 :rehash-threshold (hash-table-rehash-threshold value)
                 :weakness (hash-table-weakness value))))
      (maphash (lambda (key item)
                 (puthash (appkit-gateway-wire--copy key domain-p)
                          (appkit-gateway-wire--copy item domain-p)
                          copy))
               value)
      copy))
   (t value)))

(defun appkit-gateway-value-copy (value)
  "Return a recursive ownership copy of service domain VALUE."
  (appkit-gateway-wire--copy value nil))

(defun appkit-gateway-wire-domain-copy (value)
  "Copy accepted wire VALUE into its public service domain form.

JSON null becomes nil and JSON array vectors become lists recursively."
  (appkit-gateway-wire--copy value t))

(defun appkit-gateway-wire-array (value context &optional allow-domain-list)
  "Validate wire array VALUE for CONTEXT and return a domain list.

Vectors are the only raw representation produced by the service decoder.
When ALLOW-DOMAIN-LIST is non-nil, proper lists are also accepted for an
explicitly internal, already-normalized registry boundary."
  (cond
   ((vectorp value)
    (mapcar #'appkit-gateway-value-copy (append value nil)))
   ((and allow-domain-list (proper-list-p value))
    (appkit-gateway-value-copy value))
   (t (error "Gateway: %s must be an array" context))))

(defun appkit-gateway-wire-decode (text)
  "Decode service JSON TEXT without collapsing wire value kinds."
  (json-parse-string text
                     :object-type 'alist
                     :array-type 'array
                     :false-object :false
                     :null-object appkit-gateway-wire-null))

(defun appkit-gateway-state (client)
  "Return the current native service connection state."
  (appkit-gateway--state client))

(defun appkit-gateway-gateway-instance-id (client)
  "Return the current authenticated Gateway instance ID, or nil."
  (and (appkit-gateway--gateway-instance-id client)
       (copy-sequence (appkit-gateway--gateway-instance-id client))))

(defun appkit-gateway-capabilities (client)
  "Return a copy of capabilities advertised by the current Gateway."
  (appkit-gateway-value-copy (appkit-gateway--capabilities client)))

(defun appkit-gateway-ready-accounts (client)
  "Return a copy of the accounts in the latest `gateway.ready' snapshot."
  (appkit-gateway-value-copy (appkit-gateway--ready-accounts client)))

(defun appkit-gateway-running-p (client)
  "Return non-nil while the native service connection is active."
  (or (appkit-gateway--connecting client)
      (and (appkit-gateway--ws client)
           (websocket-openp (appkit-gateway--ws client)))
      (timerp (appkit-gateway--reconnect-timer client))))

(defun appkit-gateway-ready-p (client)
  "Return non-nil when native service business requests may be sent."
  (and (eq (appkit-gateway--state client) 'ready)
       (appkit-gateway--ws client)
       (websocket-openp (appkit-gateway--ws client))))

(defun appkit-gateway--run-hook (hook &rest arguments)
  "Deliver ARGUMENTS to HOOK's functions with independent value ownership.
HOOK may be a hook symbol or a list of callback functions."
  (dolist (function (if (symbolp hook) (symbol-value hook) hook))
    (condition-case error-data
        (apply function (mapcar #'appkit-gateway-value-copy arguments))
      (error (message "Gateway listener failed: %s" (error-message-string error-data))))))

(defun appkit-gateway--set-state (client state)
  "Publish native service connection STATE when it changes."
  (unless (eq state (appkit-gateway--state client))
    (setf (appkit-gateway--state client) state)
    (appkit-gateway--run-hook
     (appkit-gateway--state-hook client) state))
  state)

(defun appkit-gateway--next-request-id (client)
  "Return a fresh opaque native service request ID."
  (format "gateway-%d" (cl-incf (appkit-gateway--request-counter client))))

(defun appkit-gateway--json-encode (object)
  "Encode OBJECT into compact JSON text."
  (let ((json-encoding-pretty-print nil)
        (json-false :false)
        (json-null nil))
    (json-encode object)))

(defun appkit-gateway--json-decode (text)
  "Decode native service JSON TEXT without collapsing wire value kinds."
  (appkit-gateway-wire-decode text))

(defun appkit-gateway--frame-text (frame)
  "Return UTF-8 text carried by websocket FRAME, or nil."
  (pcase (websocket-frame-opcode frame)
    ('text (websocket-frame-text frame))
    ('binary (error "Gateway server sent a binary frame"))
    (_ nil)))

(defun appkit-gateway--exact-object-keys-p (object keys)
  "Return non-nil when alist OBJECT has exactly symbol KEYS."
  (appkit-gateway-wire-exact-object-keys-p object keys))

(defun appkit-gateway--read-auth-token (client)
  "Read and validate CLIENT's private authentication token file."
  (let ((path (and (stringp (appkit-gateway--auth-token-file client))
                   (expand-file-name (appkit-gateway--auth-token-file client)))))
    (unless (and path (file-regular-p path))
      (user-error "Gateway: token file is not a regular file: %s"
                  (or path (appkit-gateway--auth-token-file client))))
    (when (and (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix
                                   darwin cygwin))
               (let ((mode (file-modes path)))
                 (or (null mode) (/= 0 (logand mode #o077)))))
      (user-error
       "Gateway: token file is accessible by group/others; use chmod 600: %s"
       path))
    (let ((token
           (with-temp-buffer
             (let ((coding-system-for-read 'utf-8))
               (insert-file-contents path))
             (replace-regexp-in-string
              "[[:space:]]+\\'" "" (buffer-substring-no-properties
                                    (point-min) (point-max))))))
      (when (or (< (string-bytes token) 32)
                (string-match-p "[[:space:]]" token))
        (user-error
         "Gateway: token must be one non-whitespace value of at least 32 bytes"))
      token)))

(defun appkit-gateway--cancel-entry-timer (entry)
  "Cancel the timeout stored in pending ENTRY."
  (when-let* ((timer (plist-get entry :timer)))
    (when (timerp timer)
      (cancel-timer timer))))

(defun appkit-gateway--invoke-callback (callback &rest arguments)
  "Invoke CALLBACK with ARGUMENTS while isolating ordinary errors."
  (when callback
    (condition-case error-data
        (apply callback arguments)
      (error
       (message "Gateway: callback error: %s"
                (error-message-string error-data))))))

(defun appkit-gateway--drain-send-queue (client)
  "Send accepted requests without exceeding the Gateway's wire request limit."
  (unless (appkit-gateway--draining client)
    (cl-letf (((appkit-gateway--draining client) t)
          (inhibit-quit t))
      (while (and (appkit-gateway--send-queue client)
                  (appkit-gateway--ws client)
                  (websocket-openp (appkit-gateway--ws client))
                  (< (hash-table-count (appkit-gateway--in-flight client))
                     (appkit-gateway--max-in-flight-requests client)))
        (let* ((id (pop (appkit-gateway--send-queue client)))
               (entry (gethash id (appkit-gateway--pending client))))
          (unless (appkit-gateway--send-queue client)
            (setf (appkit-gateway--send-queue-tail client) nil))
          ;; Locally retired queued requests never reach the Gateway.
          (when entry
            (let ((payload (plist-get entry :payload)))
              (condition-case error-data
                  (progn
                    (setf (plist-get entry :timer)
                          (let ((timeout (plist-get entry :timeout)))
                            (and (numberp timeout) (> timeout 0)
                                 (run-at-time timeout nil
                                              (apply-partially #'appkit-gateway--request-timeout client)
                                              id (plist-get entry :method)))))
                    (setf (plist-get entry :payload) nil)
                    (puthash id t (appkit-gateway--in-flight client))
                    (websocket-send-text (appkit-gateway--ws client) payload))
                (error
                 ;; A failed write has ambiguous remote admission.  Only
                 ;; connection teardown can safely reclaim its wire slot.
                 (unwind-protect
                     (appkit-gateway--complete client
                      id 'error nil (error-message-string error-data))
                   (appkit-gateway--disconnect client t)))))))))))

(defun appkit-gateway--complete (client id outcome value reason)
  "Complete pending request ID exactly once.

OUTCOME is `success' or `error'.  VALUE is the result or protocol error body;
REASON is a human-readable failure.  Return non-nil when ID was pending."
  (when-let* ((entry (gethash id (appkit-gateway--pending client))))
    (remhash id (appkit-gateway--pending client))
    (appkit-gateway--cancel-entry-timer entry)
    (if (eq outcome 'success)
        (appkit-gateway--invoke-callback
         (plist-get entry :success) value)
      (appkit-gateway--invoke-callback
       (plist-get entry :error) value reason))
    t))

(defun appkit-gateway--complete-wire (client id outcome value reason)
  "Release ID's Gateway slot on its reply, even after local cancellation."
  (when (gethash id (appkit-gateway--in-flight client))
    (remhash id (appkit-gateway--in-flight client))
    (unwind-protect
        (appkit-gateway--complete client id outcome value reason)
      (appkit-gateway--drain-send-queue client))))

(defun appkit-gateway-cancel (client id)
  "Revoke ID's callbacks and unsent work, without canceling a sent Gateway RPC."
  (when-let* ((entry (gethash id (appkit-gateway--pending client))))
    (remhash id (appkit-gateway--pending client))
    (appkit-gateway--cancel-entry-timer entry)
    t))

(defun appkit-gateway--request-timeout (client id method)
  "Fail pending request ID for METHOD after its local timeout."
  (appkit-gateway--complete client
   id 'error nil (format "%s request timed out" method)))

(defun appkit-gateway--fail-pending (client reason)
  "Fail CLIENT's pending requests with REASON, even if a callback quits."
  (let (ids quit-seen-p)
    (cl-letf (((appkit-gateway--draining client) t))
      (setf (appkit-gateway--send-queue client) nil
            (appkit-gateway--send-queue-tail client) nil)
      (clrhash (appkit-gateway--in-flight client))
      (maphash (lambda (id _entry) (push id ids)) (appkit-gateway--pending client))
      (dolist (id ids)
        (condition-case nil
            (appkit-gateway--complete client id 'error nil reason)
          (quit (setq quit-seen-p t quit-flag nil))))
      (when quit-seen-p (message "Gateway: ignored quit during cleanup")))))

(defun appkit-gateway--clear-session-metadata (client)
  "Forget metadata tied to the previous websocket handshake."
  (setf (appkit-gateway--hello-instance-id client) nil
        (appkit-gateway--gateway-instance-id client) nil
        (appkit-gateway--capabilities client) nil
        (appkit-gateway--ready-accounts client) nil))

(defun appkit-gateway--clear-owner-token (owner)
  "Erase the authentication token retained by connection OWNER."
  (when-let* ((token (plist-get owner :auth-token)))
    (when (stringp token)
      (clear-string token))
    (setf (plist-get owner :auth-token) nil)))

(defun appkit-gateway--connection-current-p (client owner socket)
  "Return non-nil when OWNER still owns SOCKET.

The first synchronous callback may bind OWNER before `websocket-open'
returns."
  (and (eq owner (appkit-gateway--connection-owner client))
       (not (plist-get owner :settled))
       (let ((owned-socket (plist-get owner :socket)))
         (cond
          ((null owned-socket)
           (setf (plist-get owner :socket) socket)
           t)
          ((eq owned-socket socket))))))

(defun appkit-gateway--clear-reconnect-timer (client)
  "Cancel the current native service reconnect timer."
  (when (timerp (appkit-gateway--reconnect-timer client))
    (cancel-timer (appkit-gateway--reconnect-timer client)))
  (setf (appkit-gateway--reconnect-timer client) nil))

(defun appkit-gateway--clear-ready-timer (client)
  "Cancel the timer awaiting the authoritative ready snapshot."
  (when (timerp (appkit-gateway--ready-timer client))
    (cancel-timer (appkit-gateway--ready-timer client)))
  (setf (appkit-gateway--ready-timer client) nil))

(defun appkit-gateway--ready-timeout (client owner)
  "Reconnect when connection OWNER never receives `gateway.ready'."
  (when (and (eq owner (appkit-gateway--connection-owner client))
             (eq (appkit-gateway--state client) 'authenticating))
    (setf (appkit-gateway--ready-timer client) nil)
    (message "Gateway: ready snapshot timed out")
    (appkit-gateway--disconnect client t)))

(defun appkit-gateway--start-ready-timer (client)
  "Start the current connection's `gateway.ready' deadline."
  (appkit-gateway--clear-ready-timer client)
  (when (and (numberp (appkit-gateway--handshake-timeout client))
             (> (appkit-gateway--handshake-timeout client) 0))
    (let ((owner (appkit-gateway--connection-owner client)))
      (setf (appkit-gateway--ready-timer client)
            (run-at-time (appkit-gateway--handshake-timeout client) nil
                         (apply-partially #'appkit-gateway--ready-timeout client) owner)))))

(defun appkit-gateway--schedule-reconnect (client)
  "Schedule a native service reconnect when policy permits."
  (let ((next-attempt (1+ (appkit-gateway--reconnect-attempt client)))
        (max-attempts (appkit-gateway--reconnect-max-attempts client)))
    (if (and (integerp max-attempts) (> next-attempt max-attempts))
        (progn
          (setf (appkit-gateway--stopping client) t)
          (appkit-gateway--set-state client 'stopped)
          (message "Gateway: reached reconnect attempt limit (%d)"
                   max-attempts))
      (setf (appkit-gateway--reconnect-attempt client) next-attempt)
      (setf (appkit-gateway--reconnect-timer client)
            (run-at-time
             (max 0.2 (float (appkit-gateway--reconnect-delay client))) nil
             (lambda ()
               (setf (appkit-gateway--reconnect-timer client) nil)
               (unless (appkit-gateway--stopping client)
                 (appkit-gateway--connect client)))))
      (appkit-gateway--set-state client 'reconnecting)
      (message "Gateway: reconnecting to Gateway in %.1fs (attempt %d)"
               (max 0.2 (float (appkit-gateway--reconnect-delay client))) next-attempt))))

(defun appkit-gateway--disconnect (client &optional reconnect)
  "Disconnect only this Emacs Gateway client.

When RECONNECT is non-nil, schedule a new websocket connection.  This never
  sends an account stop or logout command to the long-lived Gateway."
  (when (appkit-gateway--connection-owner client)
    (appkit-gateway--clear-owner-token
     (appkit-gateway--connection-owner client))
    (setf (plist-get (appkit-gateway--connection-owner client) :settled) t))
  (setf (appkit-gateway--connection-owner client) nil
        (appkit-gateway--connecting client) nil)
  (when (appkit-gateway--ws client)
    (ignore-errors (websocket-close (appkit-gateway--ws client)))
    (setf (appkit-gateway--ws client) nil))
  ;; Revoke presentation generations before any pending request can fail back
  ;; into a renderer using the old connection's grants.
  (appkit-gateway--set-state client
   (if (and reconnect (not (appkit-gateway--stopping client))) 'reconnecting 'stopped))
  (appkit-gateway--fail-pending client "Gateway transport disconnected")
  (appkit-gateway--clear-session-metadata client)
  (appkit-gateway--clear-reconnect-timer client)
  (appkit-gateway--clear-ready-timer client)
  (if (and reconnect (not (appkit-gateway--stopping client)))
      (appkit-gateway--schedule-reconnect client)
    (appkit-gateway--set-state client 'stopped)))

(defun appkit-gateway--settle-connection (client owner socket event &optional error-data)
  "Settle OWNER's SOCKET once for EVENT and optional ERROR-DATA."
  (when (appkit-gateway--connection-current-p client owner socket)
    (appkit-gateway--clear-owner-token owner)
    (setf (plist-get owner :settled) t)
    (setf (appkit-gateway--connection-owner client) nil
          (appkit-gateway--connecting client) nil)
    (pcase event
      ('close (message "Gateway: websocket closed"))
      ('error (message "Gateway: websocket error: %s"
                       (error-message-string error-data))))
    (unless (appkit-gateway--stopping client)
      (appkit-gateway--disconnect client t))
    t))

(defun appkit-gateway--protocol-violation (client format-string &rest arguments)
  "Close and reconnect after a native protocol violation.

FORMAT-STRING and ARGUMENTS describe the violation."
  (message "Gateway: protocol violation: %s"
           (apply #'format format-string arguments))
  (appkit-gateway--disconnect client t))

(defun appkit-gateway--dispatch-response (client payload)
  "Dispatch a validated response PAYLOAD."
  (let ((id (alist-get 'id payload)))
    (unless (and (stringp id) (not (string-empty-p id)))
      (error "Gateway response id must be a non-empty string"))
    (appkit-gateway--complete-wire client
     id 'success (alist-get 'result payload nil nil #'eq) nil)))

(defun appkit-gateway--dispatch-error (client payload)
  "Dispatch a validated error PAYLOAD."
  (let* ((wire-id (alist-get 'id payload nil nil #'eq))
         (id (if (appkit-gateway-wire-null-p wire-id) nil wire-id))
         (wire-body (alist-get 'error payload nil nil #'eq))
         (code (and (appkit-gateway-wire-object-p wire-body)
                    (alist-get 'code wire-body)))
         (reason (and (appkit-gateway-wire-object-p wire-body)
                      (alist-get 'message wire-body))))
    (unless (and (or (null id)
                     (and (stringp id) (not (string-empty-p id))))
                 (appkit-gateway--exact-object-keys-p
                  wire-body '(code message))
                 (stringp code) (not (string-empty-p code))
                 (stringp reason) (not (string-empty-p reason)))
      (error "Gateway error envelope is malformed"))
    (let ((body (appkit-gateway-wire-domain-copy wire-body)))
      (if id
          (appkit-gateway--complete-wire client id 'error body reason)
        (progn
          (appkit-gateway--run-hook
           (appkit-gateway--protocol-error-hook client) body)
          (message "Gateway: protocol error %s: %s" code reason))))))

(defun appkit-gateway--dispatch-event (client payload)
  "Dispatch a validated event PAYLOAD."
  (let ((event (alist-get 'event payload))
        (data (alist-get 'data payload nil nil #'eq)))
    (unless (and (stringp event) (not (string-empty-p event)))
      (error "Gateway event name must be a non-empty string"))
    (when (equal event "gateway.ready")
      (unless (and (appkit-gateway--exact-object-keys-p
                    data '(gateway_instance_id accounts))
                   (stringp (alist-get 'gateway_instance_id data)))
        (error "Gateway ready data is malformed"))
      (let ((domain-accounts
             (appkit-gateway-wire-array
              (alist-get 'accounts data nil nil #'eq)
              "Gateway ready accounts")))
        (unless (and (appkit-gateway--hello-instance-id client)
                     (equal (appkit-gateway--hello-instance-id client)
                            (alist-get 'gateway_instance_id data)))
          (error "Gateway ready instance does not match hello response"))
        (setf data (appkit-gateway-value-copy data))
        (setf (appkit-gateway--gateway-instance-id client)
              (copy-sequence (alist-get 'gateway_instance_id data))
              (appkit-gateway--ready-accounts client)
              (mapcar #'appkit-gateway-wire-domain-copy domain-accounts)
              (appkit-gateway--reconnect-attempt client) 0)
        (appkit-gateway--clear-ready-timer client)
        (appkit-gateway--set-state client 'ready)
        (message "Gateway: native service ready")))
    (appkit-gateway--run-hook
     (appkit-gateway--event-hook client) event data)))

(defun appkit-gateway--handle-payload (client payload)
  "Validate and dispatch one decoded native service PAYLOAD."
  (unless (appkit-gateway-wire-object-p payload)
    (error "Gateway envelope must be an object"))
  (pcase (alist-get 'kind payload)
    ("response"
     (unless (appkit-gateway--exact-object-keys-p
              payload '(kind id result))
       (error "Gateway response envelope has invalid fields"))
     (appkit-gateway--dispatch-response client payload))
    ("error"
     (unless (appkit-gateway--exact-object-keys-p
              payload '(kind id error))
       (error "Gateway error envelope has invalid fields"))
     (appkit-gateway--dispatch-error client payload))
    ("event"
     (unless (appkit-gateway--exact-object-keys-p
              payload '(kind event data))
       (error "Gateway event envelope has invalid fields"))
     (appkit-gateway--dispatch-event client payload))
    (_ (error "Unknown Gateway envelope kind"))))

(cl-defun appkit-gateway-send (client method params &optional callback errback allow-before-ready
                                    (timeout (appkit-gateway--timeout client)))
  "Accept native service METHOD with PARAMS, queuing behind the Gateway limit.

CALLBACK receives the successful result object.  ERRBACK receives the
protocol error body (or nil) and a human-readable reason.  Return the opaque
request ID, or nil when unavailable.  Unavailable requests invoke ERRBACK
before returning nil.  Queued requests keep FIFO order; their response timeout
starts at dispatch.  Canceling a queued request prevents the RPC from running.
A non-nil ID denotes accepted work whose callback runs later from the
WebSocket or timeout event loop, never before this function returns.
TIMEOUT is the dispatch deadline in seconds; nil disables it.
ALLOW-BEFORE-READY is private transport machinery for `gateway.hello'."
  (if (not (and (appkit-gateway--ws client)
                (websocket-openp (appkit-gateway--ws client))
                (or allow-before-ready
                    (appkit-gateway-ready-p client))))
      (progn
        (appkit-gateway--invoke-callback
         errback nil
         (if (and (appkit-gateway--ws client)
                  (websocket-openp (appkit-gateway--ws client)))
             "Gateway handshake is not ready"
           "Gateway transport is not connected"))
        nil)
    (when quit-flag
      (setf quit-flag nil)
      (signal 'quit nil))
    (let* ((id (appkit-gateway--next-request-id client))
           (entry (list :method method :success callback :error errback
                        :timer nil :timeout timeout :payload nil)))
      (let ((inhibit-quit t))
        (condition-case error-data
            (progn
              ;; Encode at acceptance so queued calls own their wire values.
              (setf (plist-get entry :payload)
                    (appkit-gateway--json-encode
                     `((kind . "request")
                       (id . ,id)
                       (method . ,method)
                       (params . ,(or params (make-hash-table :test #'equal))))))
              (puthash id entry (appkit-gateway--pending client))
              (let ((node (list id)))
                (if (appkit-gateway--send-queue-tail client)
                    (setcdr (appkit-gateway--send-queue-tail client) node)
                  (setf (appkit-gateway--send-queue client) node))
                (setf (appkit-gateway--send-queue-tail client) node))
              (appkit-gateway--drain-send-queue client)
              (setf quit-flag nil)
              (and (gethash id (appkit-gateway--pending client)) id))
          (quit
           (if (gethash id (appkit-gateway--pending client))
               (progn (setf quit-flag nil) id)
             (appkit-gateway--cancel-entry-timer entry)
             (setf quit-flag nil)
             (signal 'quit nil)))
          (error
           (let ((reason (error-message-string error-data)))
             (if (gethash id (appkit-gateway--pending client))
                 (appkit-gateway--complete client id 'error nil reason)
               (appkit-gateway--cancel-entry-timer entry)
               (appkit-gateway--invoke-callback errback nil reason)))
           (setf quit-flag nil)
           nil))))))

(defun appkit-gateway--hello-failed (client _error-body reason)
  "Stop reconnecting after terminal handshake failure REASON."
  (setf (appkit-gateway--stopping client) t)
  (appkit-gateway--disconnect client nil)
  (appkit-gateway--set-state client 'failed)
  (message "Gateway: handshake failed: %s" reason))

(defun appkit-gateway--send-hello (client token)
  "Send the mandatory first request using authentication TOKEN."
  (unwind-protect
      (unless
          (appkit-gateway-send client
           "gateway.hello"
           `((protocol_version . ,(appkit-gateway--protocol-version client))
             (client_name . ,(appkit-gateway--client-name client))
             (auth_token . ,token))
           (lambda (result)
             (condition-case nil
                 (let ((capabilities
                        (appkit-gateway-wire-array
                         (and (appkit-gateway-wire-object-p result)
                              (alist-get 'capabilities result nil nil #'eq))
                         "Gateway capabilities")))
                   (if (and (equal (alist-get 'protocol_version result)
                                   (appkit-gateway--protocol-version client))
                            (appkit-gateway--exact-object-keys-p
                             result
                             '(protocol_version gateway_instance_id
                               capabilities))
                            (stringp (alist-get 'gateway_instance_id result))
                            (cl-every (lambda (capability)
                                        (and (stringp capability)
                                             (not (string-empty-p capability))))
                                      capabilities))
                       (progn
                         (setf (appkit-gateway--hello-instance-id client)
                               (copy-sequence
                                (alist-get 'gateway_instance_id result))
                               (appkit-gateway--capabilities client)
                               (appkit-gateway-value-copy capabilities))
                         (appkit-gateway--start-ready-timer client))
                     (appkit-gateway--protocol-violation client
                      "Malformed gateway.hello result")))
               (error
                (appkit-gateway--protocol-violation client
                 "Malformed gateway.hello result"))))
           (apply-partially #'appkit-gateway--hello-failed client)
           t)
        (error "Could not send gateway.hello"))
    ;; Avoid retaining the original file-backed secret in the connection
    ;; callback after the request text has been handed to websocket.el.
    (clear-string token)))

(defun appkit-gateway--connect (client)
  "Open and authenticate the native service websocket when needed."
  (unless (appkit-gateway-running-p client)
    ;; Resolve credentials before publishing a connecting owner.  A local
    ;; configuration error is terminal until the user calls start again.
    (condition-case error-data
        (let ((auth-token (appkit-gateway--read-auth-token client)))
          (let ((owner (list :socket nil :settled nil
                             :auth-token auth-token)))
            (setf (appkit-gateway--connection-owner client) owner
                  (appkit-gateway--connecting client) t
                  (appkit-gateway--stopping client) nil)
            (appkit-gateway--set-state client 'connecting)
            (condition-case socket-error
                (let ((socket
                       (websocket-open
                        (appkit-gateway--websocket-url client)
                        :on-open
                        (lambda (ws)
                          (when (appkit-gateway--connection-current-p client
                                 owner ws)
                            (setf (appkit-gateway--ws client) ws
                                  (appkit-gateway--connecting client) nil)
                            (appkit-gateway--set-state client 'authenticating)
                            (condition-case hello-error
                                (progn
                                  (appkit-gateway--send-hello client auth-token)
                                  (setf (plist-get owner :auth-token) nil))
                              (error
                               (appkit-gateway--hello-failed client
                                nil (error-message-string hello-error))))))
                        :on-message
                        (lambda (ws frame)
                          (when (appkit-gateway--connection-current-p client
                                 owner ws)
                            (condition-case payload-error
                                (when-let* ((text
                                             (appkit-gateway--frame-text
                                              frame)))
                                  (unless (string-empty-p (string-trim text))
                                    (appkit-gateway--handle-payload client
                                     (appkit-gateway--json-decode text))))
                              (error
                               (when
                                   (appkit-gateway--connection-current-p client
                                    owner ws)
                                 (appkit-gateway--protocol-violation client
                                  "%s"
                                  (error-message-string payload-error)))))))
                        :on-close
                        (lambda (ws)
                          (appkit-gateway--settle-connection client
                           owner ws 'close))
                        :on-error
                        (lambda (ws _type error-value)
                          (appkit-gateway--settle-connection client
                           owner ws 'error error-value)))))
                  (when (appkit-gateway--connection-current-p client
                         owner socket)
                    (setf (appkit-gateway--ws client) socket)))
              (error
               (appkit-gateway--settle-connection client
                owner nil 'error socket-error)))))
      (error
       (setf (appkit-gateway--stopping client) t)
       (appkit-gateway--set-state client 'failed)
       (message "Gateway: cannot start Gateway transport: %s"
                (error-message-string error-data))))))

(defun appkit-gateway-start (client)
  "Start the native service websocket without starting a Native Session."
  (setf (appkit-gateway--stopping client) nil)
  (unless (timerp (appkit-gateway--reconnect-timer client))
    (setf (appkit-gateway--reconnect-attempt client) 0))
  (appkit-gateway--connect client))

(defun appkit-gateway-stop (client)
  "Stop this Emacs websocket without stopping any QQ account."
  (setf (appkit-gateway--stopping client) t
        (appkit-gateway--reconnect-attempt client) 0)
  (appkit-gateway--disconnect client nil)
  (message "Gateway: native service connection stopped; accounts remain managed"))

(provide 'appkit-gateway)
;;; appkit-gateway.el ends here
