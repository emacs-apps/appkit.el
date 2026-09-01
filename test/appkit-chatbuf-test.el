;;; appkit-chatbuf-test.el --- Tests for appkit-chatbuf -*- lexical-binding: t; -*-

(require 'ert)

(require 'appkit-chatbuf)

(defvar appkit-chatbuf-test--timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    map))

(define-minor-mode appkit-chatbuf-test--timeline-mode
  "Enable test-only timeline keys."
  :init-value nil
  :lighter nil
  :keymap appkit-chatbuf-test--timeline-mode-map)

(ert-deftest appkit-chatbuf-mode-owns-prefixes-and-wraps-by-default ()
  (with-temp-buffer
    (appkit-chatbuf-mode)
    (should appkit-chatbuf-owns-wrap-prefix-p)
    (should visual-line-mode)
    (should word-wrap)
    (should-not truncate-lines)
    (should-not (bound-and-true-p visual-wrap-prefix-mode))))

(ert-deftest appkit-chatbuf-soft-wrap-toggle-preserves-rendered-prefixes ()
  (with-temp-buffer
    (appkit-chatbuf-mode)
    (insert (make-string 160 ?x) "\n")
    (appkit-ui-apply-line-prefix
     (point-min) (point-max)
     (appkit-ui-make-prefix-state "HEAD> " "REST> "))
    (let ((line-prefix (get-text-property (point-min) 'line-prefix))
          (wrap-prefix (get-text-property (point-min) 'wrap-prefix)))
      (appkit-chatbuf-set-soft-wrap nil)
      (should-not appkit-chatbuf-wrap-long-lines)
      (should-not visual-line-mode)
      (should truncate-lines)
      (should-not word-wrap)
      (should (equal line-prefix
                     (get-text-property (point-min) 'line-prefix)))
      (should (equal wrap-prefix
                     (get-text-property (point-min) 'wrap-prefix)))
      (appkit-chatbuf-set-soft-wrap t)
      (should appkit-chatbuf-wrap-long-lines)
      (should visual-line-mode)
      (should word-wrap)
      (should-not truncate-lines)
      (should (equal wrap-prefix
                     (get-text-property (point-min) 'wrap-prefix))))))

(ert-deftest appkit-chatbuf-mode-allows-soft-wrap-opt-out ()
  (with-temp-buffer
    (let ((appkit-chatbuf-wrap-long-lines nil))
      (appkit-chatbuf-mode))
    (should appkit-chatbuf-owns-wrap-prefix-p)
    (should-not visual-line-mode)
    (should truncate-lines)
    (should-not word-wrap)))

(ert-deftest appkit-chatbuf-soft-wrap-coordinates-optional-visual-fill ()
  (let (calls)
    (with-temp-buffer
      (let ((appkit-chatbuf-use-visual-fill-column t))
        (cl-letf (((symbol-function 'visual-fill-column-mode)
                   (lambda (argument)
                     (setq-local visual-fill-column-mode (> argument 0))
                     (setq calls (append calls (list argument))))))
          (appkit-chatbuf-mode)
          (should visual-fill-column-mode)
          (should (equal calls '(1)))
          (appkit-chatbuf-set-soft-wrap nil)
          (should-not visual-fill-column-mode)
          (should (equal calls '(1 -1)))
          (appkit-chatbuf-set-soft-wrap t)
          (should visual-fill-column-mode)
          (should (equal calls '(1 -1 1))))))))

(ert-deftest appkit-chatbuf-install-prompt-creates-tail-input-region ()
  (with-temp-buffer
    (insert "timeline\n")
    (appkit-chatbuf-init-state 8)
    (appkit-chatbuf-install-prompt ">>> ")
    (should (appkit-chatbuf-prompt-button-live-p))
    (should (= (appkit-chatbuf-input-start-position) (point-max)))
    (insert "hello")
    (should (appkit-chatbuf-has-input-p))
    (should (equal "hello" (appkit-chatbuf-input-string)))))

(ert-deftest appkit-chatbuf-string-helpers-preserve-and-inspect-properties ()
  (let ((text (copy-sequence "[file] a.txt")))
    (add-text-properties 0 (length text)
                         (list appkit-chatbuf-input-object-property
                               '(:kind attachment :path "/tmp/a.txt"))
                         text)
    (should (appkit-chatbuf-string-has-objects-p text))
    (should (equal "[file] a.txt"
                   (appkit-chatbuf-string-plain-text text)))
    (let ((copy (appkit-chatbuf-copy-string text)))
      (should-not (eq copy text))
      (should (appkit-chatbuf-string-has-objects-p copy)))))

(ert-deftest appkit-chatbuf-reset-state-reinitializes-history-and-markers ()
  (with-temp-buffer
    (appkit-chatbuf-init-state 8)
    (let ((old-input-marker appkit-chatbuf--input-marker)
          (old-prompt-marker appkit-chatbuf--prompt-marker))
      (appkit-chatbuf-aux-set '(:aux-type reply))
      (appkit-chatbuf-input-options-set '(:send-on-return t))
      (appkit-chatbuf-input-history-push "hello")
      (setq-local appkit-chatbuf--prompt-button 'dummy)
      (appkit-chatbuf-reset-state 3)
      (should (markerp appkit-chatbuf--input-marker))
      (should (markerp appkit-chatbuf--prompt-marker))
      (should-not (eq old-input-marker appkit-chatbuf--input-marker))
      (should-not (eq old-prompt-marker appkit-chatbuf--prompt-marker))
      (should (ring-p appkit-chatbuf--input-ring))
      (should (= 3 (ring-size appkit-chatbuf--input-ring)))
      (should (= 0 (ring-length appkit-chatbuf--input-ring)))
      (should-not appkit-chatbuf--input-idx)
      (should-not appkit-chatbuf--input-pending)
      (should-not appkit-chatbuf--aux-plist)
      (should-not appkit-chatbuf--input-options-plist)
      (should-not appkit-chatbuf--prompt-button))))

(ert-deftest appkit-chatbuf-input-state-set-clear-and-sync-preserve-properties ()
  (with-temp-buffer
    (appkit-chatbuf-init-state 8)
    (let ((text (copy-sequence "[file] a.txt")))
      (add-text-properties 0 (length text)
                           (list appkit-chatbuf-input-object-property
                                 '(:kind attachment :path "/tmp/a.txt"))
                           text)
      (appkit-chatbuf-input-state-set text)
      (should (appkit-chatbuf-string-has-objects-p
               (appkit-chatbuf-input-state)))
      (setq-local appkit-chatbuf--input-idx 3)
      (setq-local appkit-chatbuf--input-pending "pending")
      (appkit-chatbuf-input-state-clear :reset-history-p t)
      (should (equal "" (appkit-chatbuf-input-state)))
      (should-not appkit-chatbuf--input-idx)
      (should-not appkit-chatbuf--input-pending)
      (appkit-chatbuf-install-prompt ">>> ")
      (insert text)
      (setq-local appkit-chatbuf--input-idx 1)
      (setq-local appkit-chatbuf--input-pending "later")
      (let ((result (appkit-chatbuf-input-state-sync)))
        (should (eq t (plist-get result :changed-p)))
        (should (appkit-chatbuf-string-has-objects-p
                 (plist-get result :value)))
        (should (appkit-chatbuf-string-has-objects-p
                 (appkit-chatbuf-input-state)))
        (should-not appkit-chatbuf--input-idx)
        (should-not appkit-chatbuf--input-pending))
      (setq-local appkit-chatbuf--input-idx 2)
      (setq-local appkit-chatbuf--input-pending "keep")
      (let ((result (appkit-chatbuf-input-state-sync)))
        (should-not (plist-get result :changed-p))
        (should (= 2 appkit-chatbuf--input-idx))
        (should (equal "keep" appkit-chatbuf--input-pending))))))

(ert-deftest appkit-chatbuf-composer-revision-tracks-semantic-slot-mutations ()
  (with-temp-buffer
    (appkit-chatbuf-init-state 8)
    (should (= 0 (appkit-chatbuf-composer-revision)))
    (appkit-chatbuf-input-state-set "draft")
    (should (= 1 (appkit-chatbuf-composer-revision)))
    (appkit-chatbuf-input-state-set (copy-sequence "draft"))
    (should (= 1 (appkit-chatbuf-composer-revision)))
    (appkit-chatbuf-input-options-set '(:send-on-return t))
    (should (= 1 (appkit-chatbuf-composer-revision)))
    (appkit-chatbuf-aux-set '(:aux-type reply :message-id "m1"))
    (should (= 2 (appkit-chatbuf-composer-revision)))
    (appkit-chatbuf-aux-set '(:aux-type reply :message-id "m1"))
    (should (= 2 (appkit-chatbuf-composer-revision)))
    (appkit-chatbuf-aux-reset)
    (should (= 3 (appkit-chatbuf-composer-revision)))
    (appkit-chatbuf-reset-state 8)
    (should (= 4 (appkit-chatbuf-composer-revision)))))

(ert-deftest appkit-chatbuf-composer-revision-includes-rich-input-properties ()
  (with-temp-buffer
    (appkit-chatbuf-init-state 8)
    (appkit-chatbuf-input-state-set "file")
    (let ((rich (copy-sequence "file")))
      (add-text-properties 0 4 '(appkit-chatbuf-input-object attachment) rich)
      (appkit-chatbuf-input-state-set rich))
    (should (= 2 (appkit-chatbuf-composer-revision)))))

(ert-deftest appkit-chatbuf-input-mutations-update-canonical-state ()
  (with-temp-buffer
    (appkit-chatbuf-init-state 8)
    (appkit-chatbuf-input-state-set "cached")
    (should (equal "cached" (appkit-chatbuf-input-state)))
    (appkit-chatbuf-install-prompt ">>> ")
    (appkit-chatbuf-input-set-text "live")
    (should (equal "live" (appkit-chatbuf-input-state)))))

(ert-deftest appkit-chatbuf-input-replace-preserves-relative-point-offset ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (appkit-chatbuf-input-set-text "hello world")
    (goto-char (+ (appkit-chatbuf-input-start-position) 5))
    (appkit-chatbuf-input-replace "goodbye")
    (should (equal "goodbye" (appkit-chatbuf-input-string)))
    (should (= 5 (- (point) (appkit-chatbuf-input-start-position))))))

(ert-deftest appkit-chatbuf-prompt-update-preserves-input-and-point-offset ()
  (with-temp-buffer
    (insert "timeline\n")
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "hello")
    (goto-char (+ (appkit-chatbuf-input-start-position) 2))
    (appkit-chatbuf-prompt-update "qq> ")
    (should (appkit-chatbuf-prompt-button-live-p))
    (should (equal "hello" (appkit-chatbuf-input-string)))
    (should (= 2 (- (point) (appkit-chatbuf-input-start-position))))))

(ert-deftest appkit-chatbuf-prompt-update-is-generated-presentation ()
  (with-temp-buffer
    (buffer-enable-undo)
    (insert "timeline\n")
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "draft")
    (let ((prompt (copy-sequence "# >>> "))
          (input-start (appkit-chatbuf-input-start-position))
          modification-ran-p)
      (put-text-property 0 1 'display '(image :type png :data "avatar") prompt)
      (add-hook 'after-change-functions
                (lambda (&rest _arguments)
                  (setq modification-ran-p t))
                nil t)
      (setq buffer-undo-list nil)
      (set-buffer-modified-p nil)
      (goto-char (+ input-start 2))
      (appkit-chatbuf-prompt-update prompt)
      (should-not modification-ran-p)
      (should-not (buffer-modified-p))
      (should-not buffer-undo-list)
      (should (equal "draft" (appkit-chatbuf-input-string)))
      (should (= 2 (- (point) (appkit-chatbuf-input-start-position))))
      (should
       (equal '(image :type png :data "avatar")
              (get-text-property
               (appkit-chatbuf-prompt-start-position) 'display))))))

(ert-deftest appkit-chatbuf-bind-input-region-hides-and-restores-tail-input ()
  (with-temp-buffer
    (insert "timeline\n")
    (appkit-chatbuf-bind-input-region :visible-p nil)
    (should-not
     (text-property-not-all (point-min) (point-max) 'read-only t))
    (goto-char (point-min))
    (should-error (delete-char 1) :type 'text-read-only)
    (appkit-chatbuf-bind-input-region
     :visible-p t
     :prompt ">>> "
     :input-text "hello"
     :post-bind-function
     (lambda ()
       (when-let* ((bounds (appkit-chatbuf-input-region-bounds)))
         (add-text-properties (car bounds) (cdr bounds) '(demo t)))))
    (should (appkit-chatbuf-prompt-button-live-p))
    (should (equal "hello" (appkit-chatbuf-input-string)))
    (should (eq t (get-text-property (appkit-chatbuf-input-start-position) 'demo)))
    (appkit-chatbuf-bind-input-region :visible-p nil)
    (should-not (appkit-chatbuf-prompt-button-live-p))
    (should-not (appkit-chatbuf-input-start-position))
    (appkit-chatbuf-bind-input-region
     :visible-p t
     :prompt "qq> "
     :input-text "world")
    (should (appkit-chatbuf-prompt-button-live-p))
    (should (equal "world" (appkit-chatbuf-input-string)))))

(ert-deftest appkit-chatbuf-mode-uses-client-input-sync-function-once ()
  (with-temp-buffer
    (appkit-chatbuf-mode)
    (appkit-chatbuf-bind-input-region
     :visible-p t :prompt ">>> " :input-text "draft")
    (let ((sync-count 0))
      (setq-local appkit-chatbuf-input-sync-function
                  (lambda ()
                    (cl-incf sync-count)
                    (appkit-chatbuf-input-state-sync)))
      (goto-char (point-max))
      (insert "!")
      (should (= sync-count 1))
      (should (equal (appkit-chatbuf-input-state) "draft!")))))

(ert-deftest appkit-chatbuf-post-command-clamp-point-skips-prompt-glyphs ()
  (with-temp-buffer
    (insert "timeline\n")
    (appkit-chatbuf-install-prompt ">>> ")
    (goto-char (appkit-chatbuf-prompt-start-position))
    (appkit-chatbuf-post-command-clamp-point)
    (should (= (point) (appkit-chatbuf-input-start-position)))))

(ert-deftest appkit-chatbuf-bind-protects-generated-content-not-input ()
  (with-temp-buffer
    (insert "timeline\n")
    (appkit-chatbuf-bind-input-region
     :visible-p t :prompt ">>> " :input-text "draft")
    (should-not
     (text-property-not-all
      (point-min) (appkit-chatbuf-prompt-start-position) 'read-only t))
    (goto-char (point-min))
    (should-error (delete-char 1) :type 'text-read-only)
    (goto-char (point-max))
    (insert "!")
    (should (equal "draft!" (appkit-chatbuf-input-string)))))

(ert-deftest appkit-chatbuf-mode-switches-timeline-keys-by-point-context ()
  (with-temp-buffer
    (appkit-chatbuf-mode)
    (insert "timeline\n")
    (appkit-chatbuf-bind-input-region
     :visible-p t :prompt ">>> " :input-text "")
    (appkit-chatbuf-use-timeline-mode
     #'appkit-chatbuf-test--timeline-mode)
    (goto-char (point-min))
    (appkit-chatbuf-post-command)
    (should appkit-chatbuf-test--timeline-mode)
    (should (eq (key-binding (kbd "q")) #'quit-window))
    (goto-char (point-max))
    (appkit-chatbuf-post-command)
    (should-not appkit-chatbuf-test--timeline-mode)
    (should (eq (key-binding (kbd "q")) #'self-insert-command))
    (insert "draft")
    (should (equal "draft" (appkit-chatbuf-input-state)))))

(ert-deftest appkit-chatbuf-focus-input-moves-point-without-owning-modal-state ()
  (with-temp-buffer
    (appkit-chatbuf-mode)
    (insert "timeline\n")
    (appkit-chatbuf-bind-input-region
     :visible-p t :prompt ">>> " :input-text "draft")
    (appkit-chatbuf-use-timeline-mode
     #'appkit-chatbuf-test--timeline-mode)
    (goto-char (point-min))
    (appkit-chatbuf-update-context-mode)
    (should appkit-chatbuf-test--timeline-mode)
    (should (= (point-max) (appkit-chatbuf-focus-input)))
    (should (= (point) (appkit-chatbuf-input-logical-end-position)))
    (should-not appkit-chatbuf-test--timeline-mode)))

(ert-deftest appkit-chatbuf-structured-object-insert-and-prune ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (appkit-chatbuf-input-insert "[file:a.txt]"
                                 :object '(:type file :path "/tmp/a.txt"))
    (goto-char (appkit-chatbuf-input-start-position))
    (should (equal '(:type file :path "/tmp/a.txt")
                   (appkit-chatbuf-input-object-at-point)))
    (should (appkit-chatbuf-input-has-objects-p))
    (delete-char 1)
    (appkit-chatbuf-input-prune-broken-objects)
    (should (equal "" (or (appkit-chatbuf-input-string) "")))
    (should-not (appkit-chatbuf-input-has-objects-p))))

(ert-deftest appkit-chatbuf-after-change-syncs-deletion-at-input-start ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (appkit-chatbuf-input-set-text "deleted")
    (add-hook
     'after-change-functions
     (lambda (beg end old-length)
       (appkit-chatbuf-after-change
        beg end
        :old-length old-length
        :sync-function #'appkit-chatbuf-input-state-sync))
     nil t)
    (delete-region (appkit-chatbuf-input-start-position) (point-max))
    (should (equal "" (appkit-chatbuf-input-state)))
    ;; A later frame rebind must not resurrect the stale canonical value.
    (appkit-chatbuf-bind-input-region
     :visible-p t :prompt ">>> " :input-text (appkit-chatbuf-input-state))
    (should (equal "" (appkit-chatbuf-input-string)))))

(ert-deftest appkit-chatbuf-after-change-syncs-backspace-at-input-end ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (appkit-chatbuf-input-set-text "abc")
    (add-hook
     'after-change-functions
     (lambda (beg end old-length)
       (appkit-chatbuf-after-change
        beg end
        :old-length old-length
        :sync-function #'appkit-chatbuf-input-state-sync))
     nil t)
    (goto-char (point-max))
    (delete-char -1)
    (should (equal "ab" (appkit-chatbuf-input-state)))
    (appkit-chatbuf-bind-input-region
     :visible-p t :prompt ">>> " :input-text (appkit-chatbuf-input-state))
    (should (equal "ab" (appkit-chatbuf-input-string)))))

(ert-deftest appkit-chatbuf-input-objects-delete-as-one-unit ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (appkit-chatbuf-input-insert
     "@Alice" :object '(:kind mention :user-id "1"))
    (insert "after")
    (goto-char (+ (appkit-chatbuf-input-start-position) 7))
    (appkit-chatbuf-input-backward-delete 1)
    (should (equal "after" (appkit-chatbuf-input-string)))
    (goto-char (appkit-chatbuf-input-start-position))
    (appkit-chatbuf-input-insert
     "@Alice" :object '(:kind mention :user-id "1"))
    (goto-char (appkit-chatbuf-input-start-position))
    (appkit-chatbuf-input-forward-delete 1)
    (should (equal "after" (appkit-chatbuf-input-string)))
    (should (equal "after" (appkit-chatbuf-input-state)))))

(ert-deftest appkit-chatbuf-equal-adjacent-objects-keep-distinct-spans ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (let ((object '(:kind mention :user-id "1")))
      ;; Reusing the exact payload instance must not merge occurrences.
      (appkit-chatbuf-input-insert "@Alice" :object object)
      (appkit-chatbuf-input-insert "@Alice" :object object)
      (let* ((input (appkit-chatbuf-input-string))
             (chunks
              (appkit-chatbuf-split-by-text-property
               input appkit-chatbuf-input-object-property)))
        (should (= 2 (length chunks)))
        (should (equal '("@Alice " "@Alice ")
                       (mapcar #'substring-no-properties chunks))))
      (goto-char (point-max))
      (appkit-chatbuf-input-backward-delete 1)
      (should (equal "@Alice " (appkit-chatbuf-input-string)))
      (should (equal "@Alice " (appkit-chatbuf-input-state)))
      (should (equal object
                     (appkit-chatbuf-input-object-at-point
                      (appkit-chatbuf-input-start-position)))))))

(ert-deftest appkit-chatbuf-object-insertion-never-splits-existing-object ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (appkit-chatbuf-input-insert "@Alice" :object '(:id "1"))
    ;; An unexpected point inside an intangible body normalizes after it.
    (goto-char (1+ (appkit-chatbuf-input-start-position)))
    (appkit-chatbuf-input-insert "@Bob" :object '(:id "2"))
    (appkit-chatbuf-input-prune-broken-objects)
    (should (equal "@Alice @Bob " (appkit-chatbuf-input-string)))
    ;; Its exact start is the meaningful boundary before the block.
    (goto-char (appkit-chatbuf-input-start-position))
    (appkit-chatbuf-input-insert "@Carol" :object '(:id "3"))
    (appkit-chatbuf-input-prune-broken-objects)
    (should (equal "@Carol @Alice @Bob "
                   (appkit-chatbuf-input-string)))
    (should (= 3
               (length
                (appkit-chatbuf-split-by-text-property
                 (appkit-chatbuf-input-string)
                 appkit-chatbuf-input-object-property))))))

(ert-deftest appkit-chatbuf-input-object-string-uses-canonical-boundaries ()
  (let* ((object '(:kind attachment :path "/tmp/a.png"))
         (text (appkit-chatbuf-input-object-string "[image]" object)))
    (should (equal "[image] " (substring-no-properties text)))
    (should (equal object
                   (get-text-property
                    0 appkit-chatbuf-input-object-property text)))
    (should (symbolp
             (get-text-property
              0 appkit-chatbuf-input-object-span-property text)))
    (should (get-text-property
             0 appkit-chatbuf-input-object-start-property text))
    (should (get-text-property
             (1- (length text)) appkit-chatbuf-input-object-end-property text))
    (should (equal "[image]"
                   (get-text-property
                    0 appkit-chatbuf-input-object-text-property text)))))

(ert-deftest appkit-chatbuf-prunes-object-after-interior-edit ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (appkit-chatbuf-input-insert
     "@Alice" :object '(:kind mention :user-id "1"))
    (goto-char (+ (appkit-chatbuf-input-start-position) 2))
    (let ((inhibit-modification-hooks t))
      (delete-char 1))
    (appkit-chatbuf-input-prune-broken-objects)
    (should (equal "" (appkit-chatbuf-input-string)))))

(ert-deftest appkit-chatbuf-input-history-restores-pending-input ()
  (with-temp-buffer
    (appkit-chatbuf-init-state 8)
    (appkit-chatbuf-install-prompt ">>> ")
    (appkit-chatbuf-input-set-text "first")
    (appkit-chatbuf-input-history-push)
    (appkit-chatbuf-input-set-text "second")
    (appkit-chatbuf-input-history-push)
    (should (equal '("second" "first")
                   (appkit-chatbuf-input-history-elements)))
    (appkit-chatbuf-input-set-text "pending")
    (appkit-chatbuf-input-history-prev)
    (should (appkit-chatbuf-input-history-active-p))
    (should (equal "second" (appkit-chatbuf-input-string)))
    (appkit-chatbuf-input-history-prev)
    (should (equal "first" (appkit-chatbuf-input-string)))
    (appkit-chatbuf-input-history-next)
    (should (equal "second" (appkit-chatbuf-input-string)))
    (appkit-chatbuf-input-history-next)
    (should (equal "pending" (appkit-chatbuf-input-string)))
    (should-not (appkit-chatbuf-input-history-active-p))))

(ert-deftest appkit-chatbuf-input-history-value-navigation-restores-structured-pending-input ()
  (with-temp-buffer
    (appkit-chatbuf-init-state 8)
    (appkit-chatbuf-input-history-push "first")
    (appkit-chatbuf-input-history-push "second")
    (let ((pending (copy-sequence "[file] a.txt")))
      (add-text-properties 0 (length pending)
                           (list appkit-chatbuf-input-object-property
                                 '(:kind attachment :path "/tmp/a.txt"))
                           pending)
      (let ((prev (appkit-chatbuf-input-history-prev-value pending)))
        (should (eq 'ok (plist-get prev :status)))
        (should (equal "second" (plist-get prev :value))))
      (let ((next (appkit-chatbuf-input-history-next-value)))
        (should (eq 'ok (plist-get next :status)))
        (should (appkit-chatbuf-string-has-objects-p (plist-get next :value)))
        (should-not appkit-chatbuf--input-idx)
        (should-not appkit-chatbuf--input-pending)))))

(ert-deftest appkit-chatbuf-input-history-value-navigation-reports-empty-and-latest ()
  (with-temp-buffer
    (appkit-chatbuf-init-state 8)
    (should (eq 'empty
                (plist-get (appkit-chatbuf-input-history-prev-value "pending")
                           :status)))
    (appkit-chatbuf-input-history-push "first")
    (should (eq 'latest
                (plist-get (appkit-chatbuf-input-history-next-value)
                           :status)))))

(ert-deftest appkit-chatbuf-input-history-push-explicit-text-ignores-live-object-buffer ()
  (with-temp-buffer
    (appkit-chatbuf-init-state 8)
    (appkit-chatbuf-install-prompt ">>> ")
    (appkit-chatbuf-input-insert "[file:a.txt]"
                                 :object '(:type file :path "/tmp/a.txt"))
    (appkit-chatbuf-input-history-push "plain text")
    (should (equal '("plain text")
                   (appkit-chatbuf-input-history-elements)))))

(ert-deftest appkit-chatbuf-empty-input-remains-editable-at-point-max ()
  (save-window-excursion
    (let ((buffer (get-buffer-create " *appkit-chatbuf-input*")))
      (unwind-protect
          (progn
            (switch-to-buffer buffer)
            (erase-buffer)
            (appkit-chatbuf-init-state 8)
            (appkit-chatbuf-install-prompt ">>> ")
            (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max)))
            (execute-kbd-macro "qs")
            (should (equal "qs" (appkit-chatbuf-input-string))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest appkit-chatbuf-aux-state-roundtrip ()
  (with-temp-buffer
    (appkit-chatbuf-init-state)
    (should-not (appkit-chatbuf-aux-active-p))
    (appkit-chatbuf-aux-set '(:aux-type reply :aux-msg ((id . "m1")) :message-id "m1"))
    (should (appkit-chatbuf-aux-active-p))
    (should (equal 'reply (appkit-chatbuf-aux-type)))
    (should (equal "m1" (appkit-chatbuf-aux-message-id)))
    (should (equal '(:aux-type reply :aux-msg ((id . "m1")) :message-id "m1")
                   (appkit-chatbuf-aux-state)))
    (appkit-chatbuf-aux-reset)
    (should-not (appkit-chatbuf-aux-active-p))))

(ert-deftest appkit-chatbuf-aux-render-builds-unified-action-card ()
  (let (cancelled)
    (with-temp-buffer
      (insert
       (appkit-chatbuf-aux-render :title "Reply to Alice" :preview (appkit-ui-one-line-preview-create :text "  (sticker)\n preview  ") :cancel-action (lambda () (setq cancelled t))
				  :cancel-help "Cancel reply"
				  :width 40))
      (should
       (equal "× ▏ Reply to Alice\n  ▏ (sticker) preview\n"
              (buffer-substring-no-properties (point-min) (point-max))))
      (goto-char (point-min))
      (should (get-text-property (point) 'appkit-chatbuf-aux-ui))
      (should (eq (get-text-property (point) 'face)
                  'appkit-chatbuf-aux-close))
      (should (eq (get-text-property (+ (point) 2) 'face)
                  'appkit-chatbuf-aux-accent))
      (should (appkit-ui-activate-at))
      (should cancelled))))

(ert-deftest appkit-chatbuf-aux-render-keeps-image-separate-from-close-action ()
  (let* ((image '(image :type png :data "bytes"))
         (preview
          (appkit-ui-one-line-preview-create
           :text "caption"
           :visual (propertize "[image]" 'display image)
           :visual-columns 2))
         (action #'ignore))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'display-images-p) (lambda (&rest _) t)))
      (with-temp-buffer
        (insert
         (appkit-chatbuf-aux-render
          :title "Reply to Alice"
          :preview preview
          :cancel-action action
          :width 32))
        (should (= 2 (count-lines (point-min) (point-max))))
        (should (eq action
                    (get-text-property
                     (point-min) appkit-ui-action-property)))
        (let ((image-position
               (text-property-any (point-min) (point-max) 'display image)))
          (should image-position)
          (should-not
           (get-text-property image-position appkit-ui-action-property)))))))

(ert-deftest appkit-chatbuf-aux-render-bounds-both-lines ()
  (let* ((card
          (appkit-chatbuf-aux-render :title "Reply to a very long display name" :preview (appkit-ui-one-line-preview-create :text "a long preview that cannot escape the card") :width 16))
         (lines (split-string (substring-no-properties card) "\n" t)))
    (should (= 2 (length lines)))
    (dolist (line lines)
      (should (<= (string-width line) 16)))))

(ert-deftest appkit-chatbuf-composer-idle-rejects-semantic-content ()
  (with-temp-buffer
    (appkit-chatbuf-init-state)
    (should (appkit-chatbuf-composer-idle-p))
    (appkit-chatbuf-input-state-set "  \n\t")
    (should (appkit-chatbuf-composer-idle-p))
    (appkit-chatbuf-input-state-set "hello")
    (should-not (appkit-chatbuf-composer-idle-p))
    (appkit-chatbuf-input-state-clear)
    (appkit-chatbuf-aux-set '(:aux-type reply))
    (should-not (appkit-chatbuf-composer-idle-p))
    (appkit-chatbuf-aux-reset)
    (appkit-chatbuf-input-state-set
     (appkit-chatbuf-input-object-string
      "[image]" '(:kind image :path "/tmp/image.png")))
    (should-not (appkit-chatbuf-composer-idle-p))))

(ert-deftest appkit-chatbuf-input-options-state-roundtrip ()
  (with-temp-buffer
    (appkit-chatbuf-init-state)
    (appkit-chatbuf-input-options-set
     '(:send-on-return t :allowed-mentions none))
    (should (equal '(:send-on-return t :allowed-mentions none)
                   (appkit-chatbuf-input-options-state)))
    (should (eq t (appkit-chatbuf-input-option :send-on-return)))
    (should (eq 'none (appkit-chatbuf-input-option :allowed-mentions)))
    (should (eq 'fallback (appkit-chatbuf-input-option :missing 'fallback)))
    (appkit-chatbuf-input-options-reset)
    (should-not (appkit-chatbuf-input-options-state))))

(provide 'appkit-chatbuf-test)

;;; appkit-chatbuf-test.el ends here
