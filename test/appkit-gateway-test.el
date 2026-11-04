;;; appkit-gateway-test.el --- Gateway lifecycle tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'appkit-gateway)
(defvar appkit-gateway-test-events nil)
(defvar appkit-gateway-test-errors nil)
(defvar appkit-gateway-test-states nil)

(defmacro appkit-gateway-test-with-state (&rest body)
  "Run BODY on an isolated ready connection."
  (declare (indent 0) (debug t))
  (cons 'let (cons '((appkit-gateway-test-events nil) (appkit-gateway-test-errors nil) (appkit-gateway-test-states nil) (client (appkit-gateway-create :event-hook 'appkit-gateway-test-events :protocol-error-hook 'appkit-gateway-test-errors :state-hook 'appkit-gateway-test-states :ws 'test-websocket :state 'ready :protocol-version 29 :client-name "gateway" :hello-instance-id "gateway-1" :gateway-instance-id "gateway-1" :capabilities '("account.list") :timeout nil :handshake-timeout nil))) body)))

(ert-deftest appkit-gateway-token-file-matches-server-policy nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (let ((path (make-temp-file "qq-account-token-")))
      (unwind-protect
	  (progn
	    (with-temp-file path
	      (insert "0123456789abcdef0123456789abcdef\n"))
	    (set-file-modes path 384)
	    (cl-letf (((appkit-gateway--auth-token-file client) path))
	      (should
	       (equal (appkit-gateway--read-auth-token client)
		      "0123456789abcdef0123456789abcdef"))))
	(delete-file path)))))

(ert-deftest appkit-gateway-token-file-rejects-weak-secret nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (let ((path (make-temp-file "qq-account-token-")))
      (unwind-protect
	  (progn
	    (with-temp-file path (insert "too-short\n"))
	    (set-file-modes path 384)
	    (cl-letf (((appkit-gateway--auth-token-file client) path))
	      (should-error (appkit-gateway--read-auth-token client)
			    :type 'user-error)))
	(delete-file path)))))

(ert-deftest appkit-gateway-token-file-rejects-public-mode nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (skip-unless
     (memq system-type
	   '(gnu gnu/linux gnu/kfreebsd berkeley-unix darwin cygwin)))
    (let ((path (make-temp-file "qq-account-token-")))
      (unwind-protect
	  (progn
	    (with-temp-file path
	      (insert "0123456789abcdef0123456789abcdef\n"))
	    (set-file-modes path 416)
	    (cl-letf (((appkit-gateway--auth-token-file client) path))
	      (should-error (appkit-gateway--read-auth-token client)
			    :type 'user-error)))
	(delete-file path)))))

(ert-deftest appkit-gateway-send-uses-native-envelope nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (appkit-gateway-test-with-state
      (let (wire)
	(cl-letf
	    (((symbol-function 'websocket-openp) (lambda (_) t))
	     ((symbol-function 'websocket-send-text)
	      (lambda (_ws text) (setf wire text))))
	  (let*
	      ((id
		(appkit-gateway-send client "account.list"
				     (make-hash-table :test #'equal)))
	       (decoded (appkit-gateway--json-decode wire)))
	    (should (equal (alist-get 'kind decoded) "request"))
	    (should (equal (alist-get 'id decoded) id))
	    (should (equal (alist-get 'method decoded) "account.list"))
	    (should (equal (alist-get 'params decoded) nil))
	    (should-not (assq 'action decoded))
	    (should-not (assq 'echo decoded))))))))

(ert-deftest appkit-gateway-request-waits-for-ready nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (appkit-gateway-test-with-state
      (let (reason sent)
	(cl-letf (((appkit-gateway--state client) 'authenticating))
	  (cl-letf
	      (((symbol-function 'websocket-openp) (lambda (_) t))
	       ((symbol-function 'websocket-send-text)
		(lambda (&rest _) (setf sent t))))
	    (should-not
	     (appkit-gateway-send client "account.list" nil nil
				  (lambda (_body failure)
				    (setf reason failure))))
	    (should (equal reason "Gateway handshake is not ready"))
	    (should-not sent)
	    (should
	     (= 0 (hash-table-count (appkit-gateway--pending client))))))))))

(ert-deftest appkit-gateway-response-completes-exactly-once nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (appkit-gateway-test-with-state
      (let (wire result (calls 0))
	(cl-letf
	    (((symbol-function 'websocket-openp) (lambda (_) t))
	     ((symbol-function 'websocket-send-text)
	      (lambda (_ws text) (setf wire text))))
	  (let
	      ((id
		(appkit-gateway-send client "account.list" nil
				     (lambda (value) (cl-incf calls)
				       (setf result value)))))
	    (appkit-gateway--handle-payload client
					    `((kind . "response")
					      (id \, id)
					      (result (accounts . []))))
	    (appkit-gateway--handle-payload client
					    `((kind . "response")
					      (id \, id)
					      (result (accounts . []))))
	    (should wire) (should (= calls 1))
	    (should (equal (alist-get 'accounts result) []))
	    (should-not (gethash id (appkit-gateway--pending client)))))))))

(ert-deftest appkit-gateway-burst-completes-without-overloading-gateway ()
  (appkit-gateway-test-with-state
    (let ((remote (make-hash-table :test #'equal)) ids sent completed)
      (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                ((symbol-function 'websocket-send-text)
                 (lambda (_ text)
                   (let ((id (alist-get 'id (appkit-gateway-wire-decode text))))
                     (should (< (hash-table-count remote) 32))
                     (puthash id t remote)
                     (setq sent (append sent (list id)))))))
        (dotimes (index 45)
          (setq ids (append ids (list (appkit-gateway-send client "account.list" nil
                                 (lambda (value) (push (alist-get 'index value) completed)))))))
        (should (= (length sent) 32))
        (cl-loop for id in ids for index from 0 do
                 (should (gethash id remote))
                 (remhash id remote)
                 (appkit-gateway--dispatch-response client
                   (list (cons 'id id) (list 'result (cons 'index index)))))
        (should (equal sent ids))
        (should (equal (nreverse completed) (number-sequence 0 44)))))))

(ert-deftest
    appkit-gateway-local-retirement-does-not-release-a-wire-slot nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (dolist (retirement '(cancel timeout))
      (appkit-gateway-test-with-state
	(let (sent successes failures)
	  (cl-letf
	      (((appkit-gateway--max-in-flight-requests client) 1))
	    (cl-letf
		(((symbol-function 'websocket-openp) (lambda (_) t))
		 ((symbol-function 'websocket-send-text)
		  (lambda (_ws text)
		    (push
		     (alist-get 'id (appkit-gateway-wire-decode text))
		     sent))))
	      (let*
		  ((first
		    (appkit-gateway-send client "resource.acquire_url"
					 nil
					 (lambda (_)
					   (push 'first successes))
					 (lambda (_body reason)
					   (push reason failures))))
		   (canceled
		    (appkit-gateway-send client "message.send" nil
					 (lambda (_)
					   (ert-fail
					    "Canceled queued RPC completed"))))
		   (last
		    (appkit-gateway-send client "resource.open_local"
					 nil
					 (lambda (_)
					   (push 'last successes)))))
		(appkit-gateway-cancel client canceled)
		(if (eq retirement 'cancel)
		    (appkit-gateway-cancel client first)
		  (appkit-gateway--request-timeout client first
						   "resource.acquire_url"))
		(should (equal sent (list first)))
		(appkit-gateway--dispatch-response client
						   `((id \, first)
						     (result)))
		(should (equal (nreverse sent) (list first last)))
		(appkit-gateway--dispatch-response client
						   `((id \, first)
						     (result)))
		(appkit-gateway--dispatch-response client
						   `((id \, last)
						     (result)))
		(should (equal successes '(last)))
		(should
		 (equal failures
			(and (eq retirement 'timeout)
			     '("resource.acquire_url request timed out"))))))))))))

(ert-deftest
    appkit-gateway-queued-parameters-survive-reentrant-submission nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (appkit-gateway-test-with-state
      (let
	  ((name (copy-sequence "old")) frames first second third
	   completed)
	(cl-letf (((appkit-gateway--max-in-flight-requests client) 1))
	  (cl-letf
	      (((symbol-function 'websocket-openp) (lambda (_) t))
	       ((symbol-function 'websocket-send-text)
		(lambda (_ws text)
		  (setf frames
			(nconc frames
			       (list (appkit-gateway-wire-decode text)))))))
	    (setf first
		  (appkit-gateway-send client "account.list" nil
				       (lambda (_value)
					 (setf third
					       (appkit-gateway-send
						client "resource.list"
						nil
						(lambda (_)
						  (push 'third
							completed)))))))
	    (setf second
		  (appkit-gateway-send client "resource.acquire_url"
				       `((name \, name))
				       (lambda (_)
					 (push 'second completed))))
	    (aset name 0 110)
	    (appkit-gateway--dispatch-response client
					       `((id \, first)
						 (result)))
	    (should
	     (equal
	      (mapcar (lambda (frame) (alist-get 'id frame)) frames)
	      (list first second)))
	    (should
	     (equal
	      (alist-get 'name (alist-get 'params (cadr frames)))
	      "old"))
	    (appkit-gateway--dispatch-response client
					       `((id \, second)
						 (result)))
	    (should
	     (equal
	      (mapcar (lambda (frame) (alist-get 'id frame)) frames)
	      (list first second third)))
	    (appkit-gateway--dispatch-response client
					       `((id \, third)
						 (result)))
	    (should (equal (nreverse completed) '(second third)))))))))

(ert-deftest
    appkit-gateway-disconnect-discards-queued-work-before-reconnect
    nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (appkit-gateway-test-with-state
      (let (sent failed late-callback fresh)
	(cl-letf (((appkit-gateway--max-in-flight-requests client) 1))
	  (cl-letf
	      (((symbol-function 'websocket-openp) (lambda (_) t))
	       ((symbol-function 'websocket-close) #'ignore)
	       ((symbol-function 'websocket-send-text)
		(lambda (_ws text)
		  (push
		   (alist-get 'id (appkit-gateway-wire-decode text))
		   sent))))
	    (let*
		((first
		  (appkit-gateway-send client "resource.acquire_url"
				       nil
				       (lambda (_)
					 (setf late-callback t))
				       (lambda (_body reason)
					 (push reason failed))))
		 (queued
		  (appkit-gateway-send client "message.send" nil
				       (lambda (_)
					 (setf late-callback t))
				       (lambda (_body reason)
					 (push reason failed)))))
	      (should queued) (appkit-gateway--disconnect client)
	      (should
	       (equal failed
		      '("Gateway transport disconnected"
			"Gateway transport disconnected")))
	      (setf (appkit-gateway--ws client) 'replacement-websocket
		    (appkit-gateway--state client) 'ready)
	      (setf fresh
		    (appkit-gateway-send client "account.list" nil
					 (lambda (_)
					   (push 'fresh failed))))
	      (appkit-gateway--dispatch-response client
						 `((id \, first)
						   (result)))
	      (appkit-gateway--dispatch-response client
						 `((id \, queued)
						   (result)))
	      (should (equal (nreverse sent) (list first fresh)))
	      (should-not late-callback)
	      (appkit-gateway--dispatch-response client
						 `((id \, fresh)
						   (result)))
	      (should (eq (car failed) 'fresh)))))))))

(ert-deftest
    appkit-gateway-write-failure-cleans-queue-even-if-callback-quits
    nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (appkit-gateway-test-with-state
      (let ((writes 0) first queued-failure cleanup quit-seen)
	(cl-letf
	    (((appkit-gateway--max-in-flight-requests client) 1)
	     ((appkit-gateway--stopping client) t))
	  (cl-letf
	      (((symbol-function 'websocket-openp) (lambda (_) t))
	       ((symbol-function 'websocket-close) #'ignore)
	       ((symbol-function 'websocket-send-text)
		(lambda (&rest _)
		  (when (> (cl-incf writes) 1)
		    (error "Socket write failed")))))
	    (setf first
		  (appkit-gateway-send client "account.list" nil))
	    (setf queued-failure
		  (appkit-gateway-send client "resource.acquire_url"
				       nil nil
				       (lambda (_body reason)
					 (should
					  (equal reason
						 "Socket write failed"))
					 (signal 'quit nil))))
	    (should queued-failure)
	    (appkit-gateway-send client "message.send" nil nil
				 (lambda (_body reason)
				   (setf cleanup reason)))
	    (condition-case nil
		(appkit-gateway--dispatch-response client
						   `((id \, first)
						     (result)))
	      (quit (setf quit-seen t quit-flag nil)))
	    (should quit-seen)
	    (should (equal cleanup "Gateway transport disconnected"))
	    (should-not (appkit-gateway--ws client))
	    (should (= writes 2))))))))

(ert-deftest appkit-gateway-error-completes-request nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (appkit-gateway-test-with-state
      (let (failure body)
	(cl-letf
	    (((symbol-function 'websocket-openp) (lambda (_) t))
	     ((symbol-function 'websocket-send-text) #'ignore))
	  (let
	      ((id
		(appkit-gateway-send client "account.status"
				     '((account_id . "missing")) nil
				     (lambda (error-body reason)
				       (setf body error-body failure
					     reason)))))
	    (appkit-gateway--handle-payload client
					    `((kind . "error")
					      (id \, id)
					      (error
					       (code
						. "account_not_found")
					       (message
						. "unknown account"))))
	    (should (equal failure "unknown account"))
	    (should (equal (alist-get 'code body) "account_not_found"))
	    (should-not (gethash id (appkit-gateway--pending client)))))))))

(ert-deftest appkit-gateway-unsolicited-error-has-separate-hook nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (appkit-gateway-test-with-state
      (let (observed)
	(add-hook (appkit-gateway--protocol-error-hook client)
		  (lambda (body) (setf observed body)))
	(appkit-gateway--handle-payload client
					'((kind . "error") (id)
					  (error
					   (code
					    . "event_stream_lagged")
					   (message
					    . "missed 2 events"))))
	(should
	 (equal (alist-get 'code observed) "event_stream_lagged"))
	(should
	 (= 0 (hash-table-count (appkit-gateway--pending client))))))))

(ert-deftest appkit-gateway-unsolicited-wire-null-id-is-accepted nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (appkit-gateway-test-with-state
      (let (observed)
	(add-hook (appkit-gateway--protocol-error-hook client)
		  (lambda (body) (setf observed body)))
	(appkit-gateway--handle-payload client
					(appkit-gateway--json-decode
					 "{\"kind\":\"error\",\"id\":null,\"error\":{\"code\":\"event_stream_lagged\",\"message\":\"missed\"}}"))
	(should
	 (equal (alist-get 'code observed) "event_stream_lagged"))
	(should-not (appkit-gateway-wire-null-p observed))))))

(ert-deftest appkit-gateway-hooks-own-deep-copies nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (appkit-gateway-test-with-state
      (let* ((source (copy-sequence "account-1")))
	(cl-letf
	    (((appkit-gateway--event-hook client)
	      (list
	       (lambda (_event data)
		 (aset (alist-get 'account_id data) 0 88))
	       (lambda (_event data)
		 (should
		  (equal (alist-get 'account_id data) "account-1"))))))
	  (appkit-gateway--run-hook
	   (appkit-gateway--event-hook client) "account.changed"
	   `((account_id \, source)))
	  (should (equal source "account-1")))))))

(ert-deftest appkit-gateway-metadata-accessors-own-strings nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (appkit-gateway-test-with-state
      (let*
	  ((instance (copy-sequence "gateway-1"))
	   (capability (copy-sequence "account.list"))
	   (account-id (copy-sequence "account-1")))
	(cl-letf
	    (((appkit-gateway--gateway-instance-id client) instance)
	     ((appkit-gateway--capabilities client) (list capability))
	     ((appkit-gateway--ready-accounts client)
	      `(((account_id \, account-id)))))
	  (let
	      ((returned-instance
		(appkit-gateway-gateway-instance-id client))
	       (returned-capabilities
		(appkit-gateway-capabilities client))
	       (returned-accounts
		(appkit-gateway-ready-accounts client)))
	    (aset returned-instance 0 71)
	    (aset (car returned-capabilities) 0 65)
	    (aset (alist-get 'account_id (car returned-accounts)) 0 81))
	  (should (equal instance "gateway-1"))
	  (should (equal capability "account.list"))
	  (should (equal account-id "account-1")))))))

(ert-deftest appkit-gateway-consumer-hook-error-is-isolated nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (appkit-gateway-test-with-state
      (let (later-called)
	(add-hook (appkit-gateway--event-hook client)
		  (lambda (&rest _) (setf later-called t)) 90)
	(add-hook (appkit-gateway--event-hook client)
		  (lambda (&rest _) (error "broken consumer")) 10)
	(should
	 (condition-case nil
	     (progn
	       (appkit-gateway--handle-payload client
					       '((kind . "event")
						 (event
						  . "account.changed")
						 (data
						  (account_id . "a-1"))))
	       t)
	   (error nil)))
	(should later-called)
	(should (eq (appkit-gateway--state client) 'ready))))))

(ert-deftest appkit-gateway-handshake-requires-authoritative-ready ()
  (appkit-gateway-test-with-state
    (let (success states events)
      (setf (appkit-gateway--state client) 'authenticating
            (appkit-gateway--gateway-instance-id client) nil
            (appkit-gateway--state-hook client) (list (lambda (state) (push state states)))
            (appkit-gateway--event-hook client) (list (lambda (name _data) (push name events))))
      (cl-letf (((symbol-function 'appkit-gateway-send)
                 (lambda (_client _method _params callback &rest _) (setq success callback) "hello")))
        (appkit-gateway--send-hello client (make-string 32 ?x))
        (funcall success '((protocol_version . 29) (gateway_instance_id . "gateway-42") (capabilities . ["account.list"])))
        (should (eq (appkit-gateway-state client) 'authenticating))
        (should-not (appkit-gateway-gateway-instance-id client))
        (appkit-gateway--handle-payload client
          '((kind . "event") (event . "gateway.ready")
            (data . ((gateway_instance_id . "gateway-42") (accounts . [])))))
        (should (eq (appkit-gateway-state client) 'ready))
        (should (equal states '(ready)))
        (should (equal events '("gateway.ready")))))))

(ert-deftest appkit-gateway-hello-result-has-closed-schema ()
  (appkit-gateway-test-with-state
    (let (success violations timer-started)
      (cl-letf (((symbol-function 'appkit-gateway-send)
                 (lambda (_client _method _params callback &rest _) (setq success callback) "hello"))
                ((symbol-function 'appkit-gateway--protocol-violation)
                 (lambda (_client &rest args) (push (apply #'format args) violations)))
                ((symbol-function 'appkit-gateway--start-ready-timer)
                 (lambda (_client) (setq timer-started t))))
        (appkit-gateway--send-hello client (make-string 32 ?x))
        (funcall success '((protocol_version . 29) (gateway_instance_id . "gateway-42")
                           (capabilities . ["account.list" "account.start"])))
        (should timer-started)
        (should (equal (appkit-gateway-gateway-instance-id client) "gateway-1"))
        (should (equal (appkit-gateway--hello-instance-id client) "gateway-42"))
        (dolist (extra '((generation . 7) (future_field . "unknown")))
          (setq timer-started nil)
          (funcall success (cons extra '((protocol_version . 29) (gateway_instance_id . "gateway-42")
                                        (capabilities . ["account.list"]))))
          (should-not timer-started))
        (should (= (length violations) 2))))))

(ert-deftest appkit-gateway-ready-null-is-not-an-empty-account-array
    nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (appkit-gateway-test-with-state
      (cl-letf (((appkit-gateway--state client) 'authenticating))
	(should-error
	 (appkit-gateway--handle-payload client
					 `((kind . "event")
					   (event . "gateway.ready")
					   (data
					    (gateway_instance_id
					     . "gateway-1")
					    (accounts \
						      , appkit-gateway-wire-null))))
	 :type 'error)
	(should (eq (appkit-gateway--state client) 'authenticating))))))

(ert-deftest appkit-gateway-ready-object-is-not-an-account-array nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (appkit-gateway-test-with-state
      (cl-letf (((appkit-gateway--state client) 'authenticating))
	(dolist (object '(nil ((field . "object"))))
	  (should-error
	   (appkit-gateway--handle-payload client
					   `((kind . "event")
					     (event . "gateway.ready")
					     (data
					      (gateway_instance_id
					       . "gateway-1")
					      (accounts \, object))))
	   :type 'error))
	(should (eq (appkit-gateway--state client) 'authenticating))))))

(ert-deftest appkit-gateway-ready-instance-mismatch-reconnects nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (appkit-gateway-test-with-state
      (let (reconnect)
	(cl-letf (((appkit-gateway--state client) 'authenticating))
	  (cl-letf
	      (((symbol-function 'appkit-gateway--disconnect)
		(lambda (_client &optional requested)
		  (setf reconnect requested))))
	    (appkit-gateway--protocol-violation client "%s"
						(condition-case
						    error-data
						    (progn
						      (appkit-gateway--handle-payload
						       client
						       '((kind
							  . "event")
							 (event
							  . "gateway.ready")
							 (data
							  (gateway_instance_id
							   . "different")
							  (accounts
							   . []))))
						      "not rejected")
						  (error
						   (error-message-string
						    error-data))))
	    (should reconnect)))))))

(ert-deftest appkit-gateway-ready-timeout-is-connection-owned nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (appkit-gateway-test-with-state
      (let*
	  ((old-owner (list :socket 'old :settled nil))
	   (new-owner (list :socket 'new :settled nil)) reconnect)
	(cl-letf
	    (((appkit-gateway--connection-owner client) new-owner)
	     ((appkit-gateway--state client) 'authenticating))
	  (cl-letf
	      (((symbol-function 'appkit-gateway--disconnect)
		(lambda (_client &optional requested)
		  (setf reconnect requested))))
	    (appkit-gateway--ready-timeout client old-owner)
	    (should-not reconnect)
	    (appkit-gateway--ready-timeout client new-owner)
	    (should reconnect)))))))

(ert-deftest appkit-gateway-stop-never-sends-account-command nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (appkit-gateway-test-with-state
      (let (sent closed)
	(cl-letf
	    (((symbol-function 'websocket-openp) (lambda (_) t))
	     ((symbol-function 'websocket-send-text)
	      (lambda (&rest _) (setf sent t)))
	     ((symbol-function 'websocket-close)
	      (lambda (socket) (setf closed socket))))
	  (appkit-gateway-stop client)
	  (should (eq closed 'test-websocket)) (should-not sent)
	  (should (eq (appkit-gateway--state client) 'stopped)))))))

(ert-deftest appkit-gateway-old-socket-cannot-retire-new-owner nil
  (let
      ((client
	(appkit-gateway-create :protocol-version 29 :client-name
			       "gateway")))
    (let*
	((old-owner (list :socket 'old-socket :settled nil))
	 (new-owner (list :socket 'new-socket :settled nil)))
      (cl-letf
	  (((appkit-gateway--connection-owner client) new-owner)
	   ((appkit-gateway--ws client) 'new-socket)
	   ((appkit-gateway--connecting client) nil)
	   ((appkit-gateway--stopping client) nil))
	(should-not
	 (appkit-gateway--settle-connection client old-owner
					    'old-socket 'close))
	(should
	 (eq (appkit-gateway--connection-owner client) new-owner))
	(should (eq (appkit-gateway--ws client) 'new-socket))))))


(ert-deftest appkit-gateway-connections-own-independent-queues-and-handshakes ()
  (let ((qq (appkit-gateway-create :ws 'qq :state 'ready :timeout nil :protocol-version 29))
        (wechat (appkit-gateway-create :ws 'wechat :state 'ready :timeout nil :protocol-version 1))
        frames results)
    (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
              ((symbol-function 'websocket-close) #'ignore)
              ((symbol-function 'websocket-send-text)
               (lambda (socket text) (push (cons socket (appkit-gateway-wire-decode text)) frames))))
      (let ((q (appkit-gateway-send qq "account.list" nil (lambda (_) (push 'qq results))))
            (w (appkit-gateway-send wechat "account.list" nil (lambda (_) (push 'wechat results)))))
        (should (equal q w))
        (appkit-gateway-stop qq)
        (should (eq (appkit-gateway-state wechat) 'ready))
        (appkit-gateway--dispatch-response wechat (list (cons 'id w) '(result . nil)))
        (should (equal results '(wechat)))
        (should (= (hash-table-count (appkit-gateway--pending qq)) 0))
        (should (= (hash-table-count (appkit-gateway--pending wechat)) 0))
        (should (= (length frames) 2))))))

(ert-deftest appkit-gateway-request-timeouts-belong-to-each-request ()
  (appkit-gateway-test-with-state
    (let (durations)
      (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                ((symbol-function 'websocket-send-text) #'ignore)
                ((symbol-function 'run-at-time)
                 (lambda (duration &rest _) (push duration durations) nil)))
        (appkit-gateway-send client "media.image" nil nil nil nil 130)
        (appkit-gateway-send client "account.list" nil nil nil nil 10)
        (should (equal durations '(10 130)))
        (should-not (appkit-gateway--timeout client))))))

(provide 'appkit-gateway-test)
