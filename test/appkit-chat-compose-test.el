;;; appkit-chat-compose-test.el --- Tests for appkit-chat-compose -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'appkit-test-helper)
(require 'appkit-chat-compose)

(ert-deftest appkit-chat-compose-refresh-separates-body-and-generated-chrome ()
  "Generated compose chrome must remain outside the editable composer."
  (with-temp-buffer
    (appkit-chat-compose-mode)
    (let (field-action)
      (appkit-chat-compose-setup
       :context-function (lambda () "Context")
       :status-fields-function
       (lambda ()
         (list (list :label "Audience"
                     :value "Everyone"
                     :action (lambda ()
                               (setq field-action t)))))
       :attachments-function
       (lambda ()
         (list :title "Media"
               :items
               (list (list :label "photo.png"
                           :object 'photo))))
       :footer-function (lambda () "Send / Cancel"))
      (should (string-match-p "Context" (appkit-chat-compose-display-string)))
      (should (string-match-p "Audience: Everyone"
                              (appkit-chat-compose-display-string)))
      (should (string-match-p "Send / Cancel"
                              (appkit-chat-compose-display-string)))
      (goto-char (appkit-chat-compose-body-start-position))
      (insert "hello")
      (should (equal (appkit-chat-compose-body) "hello"))
      (should (appkit-chatbuf-point-in-input-p))
      (should-not
       (get-text-property (appkit-chat-compose-body-start-position) 'read-only))
      (appkit-chat-compose-refresh)
      (should (equal (appkit-chat-compose-body) "hello"))
      (should (string-match-p "hello" (appkit-chat-compose-display-string))))))

(ert-deftest appkit-chat-compose-refresh-omits-empty-attachment-section ()
  "An empty attachment section should not occupy the composer frame."
  (with-temp-buffer
    (appkit-chat-compose-mode)
    (appkit-chat-compose-setup
     :attachments-function
     (lambda ()
       '(:title "Media"
         :items nil
         :empty-label "  No media attached.")))
    (goto-char (appkit-chat-compose-body-start-position))
    (insert "draft")
    (appkit-chat-compose-add-item)
    (should-not (string-match-p "Media" (appkit-chat-compose-display-string)))
    (should-not (string-match-p "No media attached"
                                (appkit-chat-compose-display-string)))
    (should (equal (appkit-chat-compose-bodies) '("draft" "")))))

(ert-deftest appkit-chat-compose-frame-keeps-header-footer-and-prompt-apart ()
  "Header, footer, and prompt must not concatenate into one line."
  (with-temp-buffer
    (appkit-chat-compose-mode)
    (appkit-chat-compose-setup
     :context-function (lambda () "Context")
     :attachments-function
     (lambda ()
       (list :title "Media"
             :items (list (list :label "photo.png"))))
     :footer-function (lambda () "Send / Cancel"))
    (let ((display (appkit-chat-compose-display-string)))
      (should (string-match-p "Context\n\n" display))
      (should (string-match-p "Media\n" display))
      (should (string-match-p "Send / Cancel\n\n>>> " display))
      (should-not (string-match-p "ContextMedia" display))
      (should-not (string-match-p "Cancel>>>" display)))))

(ert-deftest appkit-chat-compose-setup-rejects-non-callable-callbacks ()
  "Compose setup should reject malformed client callbacks immediately."
  (with-temp-buffer
    (appkit-chat-compose-mode)
    (should-error
     (appkit-chat-compose-setup :context-function "not a function"))))

(ert-deftest appkit-chat-compose-refresh-preserves-multiple-bodies ()
  "A multi-part compose surface should keep each body on refresh."
  (with-temp-buffer
    (appkit-chat-compose-mode)
    (let ((count 2))
      (appkit-chat-compose-setup
       :context-function (lambda () "Context")
       :parts-function
       (lambda ()
         (cl-loop for index from 1 to count
                  collect (list :title (format "Part %d" index)
                                :attachments
                                (list :title "Media"
                                      :items nil
                                      :empty-label "  None."))))
       :footer-function (lambda () "Footer"))
      (goto-char (appkit-chat-compose-body-start-position))
      (insert "first")
      (setq count 3)
      (appkit-chat-compose-add-item)
      (should (equal (appkit-chat-compose-bodies) '("first" "")))
      (appkit-chat-compose-goto-part 0)
      (should (equal (appkit-chat-compose-body) "first"))
      (appkit-chat-compose-add-item)
      (insert "second")
      (appkit-chat-compose-refresh)
      (should (equal (appkit-chat-compose-bodies) '("first" "second" "")))
      (should (eq (appkit-chat-compose-current-part-index) 1))
      (should (equal (appkit-chat-compose-body) "second")))))

(ert-deftest appkit-chat-compose-undo-does-not-duplicate-after-chrome-refresh ()
  "Chrome refresh must not record undo or rewrite editable bodies."
  (with-temp-buffer
    (appkit-chat-compose-mode)
    (appkit-chat-compose-setup
     :status-fields-function
     (lambda ()
       (list (list :label "Length"
                   :value (format "%d" (length (appkit-chat-compose-body))))))
     :footer-function (lambda () "Footer"))
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (goto-char (appkit-chat-compose-body-start-position))
    (insert "hello")
    (undo-boundary)
    (appkit-chat-compose-refresh)
    (should (equal (appkit-chat-compose-body) "hello"))
    (should (string-match-p "Length: 5" (appkit-chat-compose-display-string)))
    (undo)
    (should (equal (appkit-chat-compose-body) ""))
    (should-not (string-match-p "hellohello"
                                (appkit-chat-compose-display-string)))))

(ert-deftest appkit-chat-compose-add-and-drop-items-keep-order ()
  "Adding or dropping a draft item should keep the surrounding items."
  (with-temp-buffer
    (appkit-chat-compose-mode)
    (appkit-chat-compose-setup)
    (goto-char (appkit-chat-compose-body-start-position))
    (insert "first")
    (appkit-chat-compose-add-item)
    (insert "third")
    (appkit-chat-compose-goto-part 0)
    (appkit-chat-compose-add-item)
    (insert "second")
    (should (equal (appkit-chat-compose-bodies) '("first" "second" "third")))
    (appkit-chat-compose-goto-part 1)
    (appkit-chat-compose-drop-item)
    (should (equal (appkit-chat-compose-bodies) '("first" "third")))
    (should (eq (appkit-chat-compose-current-part-index) 1))))

(ert-deftest appkit-chat-compose-enumerates-attachment-only-input ()
  "Client metadata must make an otherwise empty input a compose item."
  (with-temp-buffer
    (appkit-chat-compose-mode)
    (appkit-chat-compose-setup)
    (let ((item (appkit-chat-compose-current-item)))
      (appkit-chat-compose-update-current-item
       (plist-put item :attachments '((:path "only.png")))))
    (should (equal (appkit-chat-compose-bodies) '("")))
    (should (equal (plist-get (car (appkit-chat-compose-items)) :attachments)
                   '((:path "only.png"))))))

(ert-deftest appkit-chat-compose-flushes-mixed-text-and-file-parts ()
  "Input flushing must retain both textual and metadata-only parts."
  (with-temp-buffer
    (appkit-chat-compose-mode)
    (appkit-chat-compose-setup)
    (goto-char (appkit-chat-compose-body-start-position))
    (insert "text")
    (appkit-chat-compose-add-item)
    (let ((item (appkit-chat-compose-current-item)))
      (appkit-chat-compose-update-current-item
       (plist-put item :attachments '((:path "only.png")))))
    (appkit-chat-compose-add-item)
    (should (equal (mapcar (lambda (item) (plist-get item :text))
                           (appkit-chat-compose-items))
                   '("text" "" "")))
    (should (equal (plist-get (nth 1 (appkit-chat-compose-items)) :attachments)
                   '((:path "only.png"))))))

(ert-deftest appkit-chat-compose-structural-edits-have-exact-revisions ()
  "Navigation is observational; each structural draft edit advances once."
  (with-temp-buffer
    (appkit-chat-compose-mode)
    (appkit-chat-compose-setup)
    (appkit-chat-compose-set-items
     '((:text "first" :attachments nil)
       (:text "second" :attachments nil)))
    (should (= 0 (appkit-compose-generation)))
    (appkit-chat-compose-goto-part 0)
    (should (= 0 (appkit-compose-generation)))
    (appkit-chat-compose-add-item)
    (should (= 1 (appkit-compose-generation)))
    (appkit-chat-compose-update-current-item
     '(:text "inserted" :attachments ((:path "photo.png"))))
    (should (= 2 (appkit-compose-generation)))
    (appkit-chat-compose-drop-item)
    (should (= 3 (appkit-compose-generation)))))

(defvar-local appkit-chat-compose-test--account nil)

(define-derived-mode appkit-chat-compose-test-mode appkit-chat-compose-mode "Compose-Test")

(ert-deftest appkit-chat-compose-preserves-client-mode-and-stopped-draft ()
  (let ((app (appkit-app-start appkit-test--app-type :identity (make-symbol "compose-app")))
        (buffer (generate-new-buffer " *appkit-compose-owner-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (appkit-chat-compose-test-mode)
          (setq-local appkit-chat-compose-test--account "test-account")
          (appkit-chat-compose-setup :app app)
          (appkit-chat-compose-set-items '((:text "first") (:text "typed draft")))
          (should (eq major-mode 'appkit-chat-compose-test-mode))
          (should (equal appkit-chat-compose-test--account "test-account"))
          (should-not buffer-read-only)
          (let ((surface (appkit-current-surface)))
            (appkit-app-close app)
            (should (buffer-live-p buffer))
            (should-not (appkit-surface-live-p surface))
            (should (equal (mapcar (lambda (item) (plist-get item :text))
                                   (appkit-chat-compose-items))
                           '("first" "typed draft")))
            (should-error (appkit-chat-compose-refresh))
            (should-not (appkit-current-surface))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (appkit-app-live-p app) (appkit-app-close app)))))

(ert-deftest appkit-chat-compose-replacement-preserves-active-publication ()
  "Persisting publication progress must not cancel the owning operation."
  (with-temp-buffer
    (appkit-chat-compose-mode)
    (appkit-chat-compose-setup)
    (appkit-compose-touch)
    (let* ((canceled 0)
           (owner (appkit-compose-operation-begin
                   'publish :cancel-function (lambda () (cl-incf canceled)))))
      (appkit-chat-compose-set-items
       '((:text "remaining" :attachments ((:id "uploaded")))))
      (should (appkit-compose-operation-current-p owner))
      (should (= 1 (appkit-compose-generation)))
      (should (= 0 canceled))
      (let ((item (car (appkit-chat-compose-items))))
        (should (equal "remaining" (plist-get item :text)))
        (should (equal '((:id "uploaded")) (plist-get item :attachments))))
      (should (appkit-compose-operation-finish owner))
      (should-not (appkit-compose-operation-active-p))
      (should (= 0 canceled)))))

(provide 'appkit-chat-compose-test)

;;; appkit-chat-compose-test.el ends here
