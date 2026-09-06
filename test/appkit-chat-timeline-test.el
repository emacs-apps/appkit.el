;;; appkit-chat-timeline-test.el --- Tests for projected chat timelines -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'appkit-chat-timeline)
(require 'appkit-test-helper)

(defun appkit-chat-timeline-test--printer (prints)
  "Return a row printer recording render counts in PRINTS."
  (lambda (row)
    (let* ((key (appkit-chat-timeline-row-key row))
           (payload (appkit-chat-timeline-row-payload row))
           (context (appkit-chat-timeline-row-context row))
           (start (point)))
      (puthash key (1+ (gethash key prints 0)) prints)
      (insert (format "%s:%s:%s\n"
                      key payload (or (plist-get context :layout) "plain")))
      (add-text-properties start (point) (list 'test-message-key key)))))

(defun appkit-chat-timeline-test--row (key payload &optional context dependencies)
  "Create one test row from KEY, PAYLOAD, CONTEXT, and DEPENDENCIES."
  (appkit-chat-timeline-row-create
   :key key
   :payload payload
   :context context
   :dependencies dependencies))

(ert-deftest appkit-chat-timeline-projects-context-and-dependencies ()
  (let ((rows
         (appkit-chat-timeline-project
          '((a . "one") (b . "two"))
          #'car
          :context-function
          (lambda (previous current)
            (list :previous (car-safe previous)
                  :current (car current)))
          :dependencies-function
          (lambda (entry)
            (list (list :source (cdr entry)))))))
    (should (equal '(a b) (mapcar #'appkit-chat-timeline-row-key rows)))
    (should (equal '(:previous a :current b)
                   (appkit-chat-timeline-row-context (cadr rows))))
    (should (equal '((:source "two"))
                   (appkit-chat-timeline-row-dependencies (cadr rows))))))

(ert-deftest appkit-chat-timeline-sync-is-keyed-and-context-sensitive ()
  (appkit-test-with-surface
    (let ((prints (make-hash-table :test #'equal)))
      (appkit-chat-timeline-ensure
       :printer (appkit-chat-timeline-test--printer prints)
       :anchor-property 'test-message-key)
      (appkit-chat-timeline-sync
       (list (appkit-chat-timeline-test--row 'a "A")
             (appkit-chat-timeline-test--row 'b "B")))
      (let ((a-node (appkit-chat-timeline-node 'a))
            (b-node (appkit-chat-timeline-node 'b)))
        (appkit-chat-timeline-sync
         (list (appkit-chat-timeline-test--row 'a "A")
               (appkit-chat-timeline-test--row 'b "B" '(:layout compact))))
        (should (eq a-node (appkit-chat-timeline-node 'a)))
        (should (eq b-node (appkit-chat-timeline-node 'b)))
        (should (= 1 (gethash 'a prints)))
        (should (= 2 (gethash 'b prints)))
        (should (equal '(a b) (appkit-chat-timeline-keys)))))))

(ert-deftest appkit-chat-timeline-sync-handles-arbitrary-reordering ()
  (appkit-test-with-surface
    (let ((prints (make-hash-table :test #'equal)))
      (appkit-chat-timeline-ensure
       :printer (appkit-chat-timeline-test--printer prints)
       :anchor-property 'test-message-key)
      (appkit-chat-timeline-sync
       (list (appkit-chat-timeline-test--row 'a "A")
             (appkit-chat-timeline-test--row 'b "B")
             (appkit-chat-timeline-test--row 'c "C")))
      (appkit-chat-timeline-sync
       (list (appkit-chat-timeline-test--row 'c "C")
             (appkit-chat-timeline-test--row 'a "A")
             (appkit-chat-timeline-test--row 'd "D")))
      (should (equal '(c a d) (appkit-chat-timeline-keys)))
      (should-not (appkit-chat-timeline-node 'b))
      (should (string-match-p "c:C:plain\na:A:plain\nd:D:plain"
                              (buffer-string))))))

(ert-deftest appkit-chat-timeline-refresh-reprints-unchanged-rows ()
  (appkit-test-with-surface
    (let ((prints (make-hash-table :test #'equal)))
      (appkit-chat-timeline-ensure
       :printer (appkit-chat-timeline-test--printer prints)
       :anchor-property 'test-message-key)
      (appkit-chat-timeline-sync
       (list (appkit-chat-timeline-test--row 'a "A")
             (appkit-chat-timeline-test--row 'b "B")))
      (let ((a-node (appkit-chat-timeline-node 'a))
            (b-node (appkit-chat-timeline-node 'b)))
        (appkit-chat-timeline-refresh)
        (should (eq a-node (appkit-chat-timeline-node 'a)))
        (should (eq b-node (appkit-chat-timeline-node 'b)))
        (should (= 2 (gethash 'a prints)))
        (should (= 2 (gethash 'b prints)))))))

(ert-deftest appkit-chat-timeline-invalidates-old-and-new-resource-dependents ()
  (appkit-test-with-surface
    (let ((prints (make-hash-table :test #'equal))
          (resource '(:message "source")))
      (appkit-chat-timeline-ensure
       :printer (appkit-chat-timeline-test--printer prints)
       :anchor-property 'test-message-key)
      (appkit-chat-timeline-sync
       (list (appkit-chat-timeline-test--row 'source "source")
             (appkit-chat-timeline-test--row 'reply "reply" nil
                                             (list resource))))
      (appkit-chat-timeline-sync
       (list (appkit-chat-timeline-test--row 'source "updated")
             (appkit-chat-timeline-test--row 'reply "reply" nil
                                             (list resource)))
       :changed-resources (list resource))
      (should (= 2 (gethash 'source prints)))
      (should (= 2 (gethash 'reply prints)))
      (should (equal '(reply)
                     (appkit-chat-timeline-dependent-keys (list resource)))))))

(ert-deftest appkit-chat-timeline-rekey-preserves-node-and-semantic-point ()
  (appkit-test-with-surface
    (let ((prints (make-hash-table :test #'equal)))
      (appkit-chat-timeline-ensure
       :printer (appkit-chat-timeline-test--printer prints)
       :anchor-property 'test-message-key)
      (appkit-chat-timeline-sync
       (list (appkit-chat-timeline-test--row "local-1" "pending")))
      (let ((node (appkit-chat-timeline-node "local-1")))
        (goto-char (appkit-chat-timeline-key-position "local-1"))
        (move-to-column 3)
        (appkit-chat-timeline-sync
         (list (appkit-chat-timeline-test--row
                "7467703692092974645" "sent"))
         :rekeys '(("local-1" . "7467703692092974645")))
        (should (eq node
                    (appkit-chat-timeline-node "7467703692092974645")))
        (should-not (appkit-chat-timeline-node "local-1"))
        (should (equal "7467703692092974645"
                       (appkit-chat-timeline-key-at-point)))
        (should (= 3 (current-column)))))))

(ert-deftest appkit-chat-timeline-frame-update-preserves-composer-and-undo ()
  (appkit-test-with-surface
    (let ((prints (make-hash-table :test #'equal)))
      (appkit-chatbuf-init-state 8)
      (appkit-chat-timeline-ensure
       :printer (appkit-chat-timeline-test--printer prints)
       :anchor-property 'test-message-key
       :header "old header\n"
       :footer "old footer\n")
      (appkit-chat-timeline-sync
       (list (appkit-chat-timeline-test--row 'a "A")))
      (appkit-chat-timeline-set-frame
       "old header\n" "old footer\n"
       :bind-input-function
       (lambda ()
         (appkit-chatbuf-bind-input-region
          :visible-p t :prompt ">>> " :input-text "draft")))
      (goto-char (+ (appkit-chatbuf-input-start-position) 2))
      (setq buffer-undo-list nil)
      (let ((input-marker appkit-chatbuf--input-marker)
            (prompt-marker appkit-chatbuf--prompt-marker))
        (appkit-chat-timeline-set-frame
         "new header\n" "new footer\n"
         :bind-input-function
         (lambda ()
           (appkit-chatbuf-bind-input-region
            :visible-p t :prompt "qq> " :input-text "draft")))
        (should (eq input-marker appkit-chatbuf--input-marker))
        (should (eq prompt-marker appkit-chatbuf--prompt-marker))
        (should (= 2 (- (point) (appkit-chatbuf-input-start-position))))
        (should (equal "draft" (appkit-chatbuf-input-string)))
        (should-not buffer-undo-list)))))

(ert-deftest appkit-chat-timeline-frame-update-does-not-rebind-live-composer ()
  (appkit-test-with-surface
    (let ((prints (make-hash-table :test #'equal))
          (bind-count 0))
      (appkit-chatbuf-init-state 8)
      (appkit-chat-timeline-ensure
       :printer (appkit-chat-timeline-test--printer prints)
       :anchor-property 'test-message-key
       :header "old header\n"
       :footer "old footer\n")
      (appkit-chat-timeline-sync
       (list (appkit-chat-timeline-test--row 'a "A")))
      (let ((binder
             (lambda ()
               (cl-incf bind-count)
               (appkit-chatbuf-bind-input-region
                :visible-p t :prompt ">>> " :input-text "draft"))))
        (appkit-chat-timeline-set-frame
         "old header\n" "old footer\n"
         :bind-input-function binder
         :composer-visible-p t)
        (let ((prompt-marker appkit-chatbuf--prompt-marker)
              (input-marker appkit-chatbuf--input-marker))
          (appkit-chatbuf-input-set-text "live edit")
          (appkit-chat-timeline-set-frame
           "new header\n" "a substantially longer new footer\n"
           :bind-input-function binder
           :composer-visible-p t)
          (should (= 1 bind-count))
          (should (eq prompt-marker appkit-chatbuf--prompt-marker))
          (should (eq input-marker appkit-chatbuf--input-marker))
          (should (equal "live edit" (appkit-chatbuf-input-string)))
          (should-not
           (text-property-not-all
            (point-min) (appkit-chatbuf-prompt-start-position)
            'read-only t))
          (should (string-match-p "new header" (buffer-string)))
          (should (string-match-p "substantially longer new footer"
                                  (buffer-string)))
          (should (string-match-p "a:A:plain" (buffer-string))))))))

(ert-deftest appkit-chat-timeline-composer-follows-later-row-insertion ()
  (appkit-test-with-surface
    (let ((prints (make-hash-table :test #'equal)))
      (appkit-chatbuf-init-state 8)
      (appkit-chat-timeline-ensure
       :printer (appkit-chat-timeline-test--printer prints)
       :anchor-property 'test-message-key
       :footer "footer\n")
      ;; This is the client render order: establish the frame/composer first,
      ;; then reconcile projected message rows.
      (appkit-chat-timeline-set-frame
       "" "footer\n"
       :bind-input-function
       (lambda ()
         (appkit-chatbuf-bind-input-region
          :visible-p t :prompt ">>> " :input-text "draft"))
       :composer-visible-p t)
      (appkit-chat-timeline-sync
       (list (appkit-chat-timeline-test--row 'a "A")
             (appkit-chat-timeline-test--row 'b "B")))
      (let ((footer (appkit-chat-timeline-footer-start-position))
            (prompt (appkit-chatbuf-prompt-start-position))
            (input (appkit-chatbuf-input-start-position)))
        (should (integerp footer))
        (should (<= footer prompt input))
        (should (appkit-chatbuf-prompt-button-live-p))
        (should (equal "draft" (appkit-chatbuf-input-string)))
        (should-not
         (text-property-not-all (point-min) prompt 'read-only t))
        (should (string-match-p
                 "a:A:plain\nb:B:plain\nfooter\n>>> draft\\'"
                 (buffer-string)))))))

(ert-deftest appkit-chat-timeline-invalid-printer-keeps-composer ()
  (appkit-test-with-surface
    (appkit-chatbuf-init-state 8)
    (appkit-chatbuf-bind-input-region
     :visible-p t :prompt ">>> " :input-text "draft")
    (let ((before (buffer-string)))
      (should-error (appkit-chat-timeline-ensure))
      (should (equal before (buffer-string)))
      (should (equal "draft" (appkit-chatbuf-input-string))))))

(ert-deftest appkit-chat-timeline-failed-mutation-reprotects-generated-content ()
  (appkit-test-with-surface
    (let ((prints (make-hash-table :test #'equal)))
      (appkit-chatbuf-init-state 8)
      (appkit-chat-timeline-ensure
       :printer (appkit-chat-timeline-test--printer prints)
       :anchor-property 'test-message-key)
      (appkit-chat-timeline-set-frame
       "" ""
       :bind-input-function
       (lambda ()
         (appkit-chatbuf-bind-input-region
          :visible-p t :prompt ">>> " :input-text "draft"))
       :composer-visible-p t)
      (let ((start (appkit-chatbuf-prompt-start-position)))
        (should-error
         (appkit-chat-timeline-run-preserving-position
          (lambda ()
            (goto-char start)
            (insert "partial")
            (error "printer failed"))))
        (should-not
         (text-property-not-all
          start (appkit-chatbuf-prompt-start-position) 'read-only t))))))

(ert-deftest appkit-chat-timeline-frame-update-removes-hidden-composer-only ()
  (appkit-test-with-surface
    (let ((prints (make-hash-table :test #'equal)))
      (appkit-chatbuf-init-state 8)
      (appkit-chat-timeline-ensure
       :printer (appkit-chat-timeline-test--printer prints)
       :anchor-property 'test-message-key)
      (appkit-chat-timeline-sync
       (list (appkit-chat-timeline-test--row 'a "A")))
      (let* ((visible-p t)
             (binder
              (lambda ()
                (appkit-chatbuf-bind-input-region
                 :visible-p visible-p :prompt ">>> " :input-text "draft"))))
        (appkit-chat-timeline-set-frame
         "" "" :bind-input-function binder :composer-visible-p t)
        (setq visible-p nil)
        (appkit-chat-timeline-set-frame
         "" "" :bind-input-function binder :composer-visible-p nil)
        (should-not (appkit-chatbuf-prompt-start-position))
        (should-not (appkit-chatbuf-input-start-position))
        (should (string-match-p "a:A:plain" (buffer-string)))))))

(ert-deftest appkit-chat-timeline-frame-update-refuses-crossed-composer-boundary ()
  (appkit-test-with-surface
    (let ((prints (make-hash-table :test #'equal)))
      (appkit-chatbuf-init-state 8)
      (appkit-chat-timeline-ensure
       :printer (appkit-chat-timeline-test--printer prints)
       :anchor-property 'test-message-key)
      (appkit-chat-timeline-sync
       (list (appkit-chat-timeline-test--row 'a "A")))
      (appkit-chatbuf-bind-input-region
       :visible-p t :prompt ">>> " :input-text "draft")
      ;; Simulate a corrupted external mutation moving the prompt boundary
      ;; into the EWOC-owned region.  Frame refresh must fail before deleting
      ;; any text from that marker through `point-max'.
      (set-marker appkit-chatbuf--prompt-marker (point-min))
      (let ((before (buffer-string)))
        (should-error
         (appkit-chat-timeline-set-frame
          "new header\n" "new footer\n"
          :bind-input-function #'ignore
          :composer-visible-p t))
        (should (equal before (buffer-string)))))))

(ert-deftest appkit-chat-timeline-window-visible-end-stops-at-footer ()
  (appkit-test-with-surface
    (let ((window 'test-window)
          (buffer (current-buffer))
          (window-end-position 900)
          (footer-position 1000))
      (cl-letf (((symbol-function 'window-live-p)
                 (lambda (candidate) (eq candidate window)))
                ((symbol-function 'window-buffer)
                 (lambda (_window) buffer))
                ((symbol-function 'window-end)
                 (lambda (_window update)
                   (should update)
                   window-end-position))
                ((symbol-function 'pos-visible-in-window-p)
                 (lambda (_position candidate _partially)
                   (eq candidate window)))
                ((symbol-function 'appkit-chat-timeline-footer-start-position)
                 (lambda () footer-position)))
        (should (= 900
                   (appkit-chat-timeline-window-visible-end-position window)))
        (setq window-end-position 1200)
        (should (= 1000
                   (appkit-chat-timeline-window-visible-end-position window)))
        (setq footer-position nil)
        (should (= 1200
                   (appkit-chat-timeline-window-visible-end-position window)))))))

(ert-deftest appkit-chat-timeline-window-visible-end-rejects-foreign-window ()
  (appkit-test-with-surface
    (let ((window 'test-window)
          (foreign (get-buffer-create " *appkit-foreign*")))
      (unwind-protect
          (cl-letf (((symbol-function 'window-live-p) (lambda (_window) t))
                    ((symbol-function 'window-buffer)
                     (lambda (_window) foreign))
                    ((symbol-function 'window-end)
                     (lambda (&rest _args)
                       (ert-fail "foreign window end must not be inspected"))))
            (should-not
             (appkit-chat-timeline-window-visible-end-position window)))
        (kill-buffer foreign)))))

(ert-deftest appkit-chat-timeline-rejects-invalid-projections-before-mutation ()
  (appkit-test-with-surface
    (let ((prints (make-hash-table :test #'equal)))
      (appkit-chat-timeline-ensure
       :printer (appkit-chat-timeline-test--printer prints)
       :anchor-property 'test-message-key)
      (should-error
       (appkit-chat-timeline-sync
        (list (appkit-chat-timeline-test--row 'same "one")
              (appkit-chat-timeline-test--row 'same "two"))))
      (should-not (appkit-chat-timeline-keys)))))

(ert-deftest appkit-chat-timeline-owns-one-history-observer ()
  (appkit-test-with-surface
    (let ((view (appkit-current-surface)))
      (appkit-chat-timeline-ensure
       :printer (appkit-chat-timeline-test--printer
                 (make-hash-table :test #'equal))
       :anchor-property 'test-message-key)
      (let ((old
             (appkit-chat-timeline-install-history-observer
              view :start-function #'ignore)))
        (should (eq old (appkit-chat-timeline-scroll-observer)))
        (let ((new
               (appkit-chat-timeline-install-history-observer
                view :end-function #'ignore)))
          (should-not (appkit-scroll-observer-active-p old))
          (should (eq new (appkit-chat-timeline-scroll-observer)))
          (appkit-chat-timeline-reset)
          (should-not (appkit-scroll-observer-active-p new))
          (should-not (appkit-chat-timeline-scroll-observer)))))))

(provide 'appkit-chat-timeline-test)

;;; appkit-chat-timeline-test.el ends here
