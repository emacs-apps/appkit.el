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
  "Unfrozen async work allows edits; each structural edit advances once."
  (with-temp-buffer
    (appkit-chat-compose-mode)
    (appkit-chat-compose-setup)
    (appkit-chat-compose-set-items
     '((:text "first" :attachments nil)
       (:text "second" :attachments nil)))
    (let ((generation (appkit-compose-generation))
          (capture (appkit-compose-capture))
          (owner (appkit-compose-operation-begin 'preview)))
      (unwind-protect
          (progn
            (appkit-chat-compose-goto-part 0)
            (should (equal (appkit-compose-capture) capture))
            (appkit-chat-compose-add-item)
            (should (= (1+ generation) (appkit-compose-generation)))
            (appkit-chat-compose-update-current-item
             '(:attachments ((:path "photo.png"))))
            (should (= (+ generation 2) (appkit-compose-generation)))
            (appkit-chat-compose-drop-item)
            (should (= (+ generation 3) (appkit-compose-generation)))
            (should (equal (appkit-chat-compose-bodies) '("first" "second")))
            (should (appkit-compose-operation-current-p owner)))
        (appkit-compose-operation-finish owner)))))

(ert-deftest appkit-chat-compose-navigation-preserves-uncommitted-tail ()
  "Leaving live input retains both text and attachment-only tail parts."
  (dolist (tail '("last" ""))
    (with-temp-buffer
      (appkit-chat-compose-mode)
      (appkit-chat-compose-setup)
      (appkit-chat-compose-set-items
       (list (list :text "first")
             (list :text tail :attachments '((:path "tail.png")))))
      (let ((capture (appkit-compose-capture)))
        (appkit-chat-compose-goto-part 0)
        (should (equal (appkit-compose-capture) capture))
        (appkit-chat-compose-goto-part 1)
        (should (equal (appkit-chat-compose-body) tail))
        (should (equal (appkit-compose-capture) capture))
        (appkit-chat-compose-goto-part 0)
        (should (equal (appkit-compose-capture) capture))))))

(ert-deftest appkit-chat-compose-revisiting-current-part-preserves-edits ()
  "Refocusing an edited part must not reload its stale generated row."
  (with-temp-buffer
    (appkit-chat-compose-mode)
    (appkit-chat-compose-setup)
    (appkit-chat-compose-set-items '((:text "first") (:text "last")))
    (appkit-chat-compose-goto-part 0)
    (goto-char (appkit-chat-compose-body-end-position))
    (insert " edited")
    (let ((capture (appkit-compose-capture)))
      (appkit-chat-compose-goto-part 0)
      (should (equal (appkit-chat-compose-body) "first edited"))
      (should (equal (appkit-compose-capture) capture))
      (should (= (point) (appkit-chat-compose-body-start-position))))))

(ert-deftest appkit-chat-compose-new-tail-does-not-duplicate-edited-part ()
  "Fresh input neither repeats the last editor text nor inherits metadata."
  (with-temp-buffer
    (appkit-chat-compose-mode)
    (appkit-chat-compose-setup)
    (appkit-chat-compose-set-items
     '((:text "first" :attachments ((:path "first.png")))
       (:text "last" :attachments ((:path "last.png")))))
    (appkit-chat-compose-goto-part 0)
    (goto-char (appkit-chat-compose-body-end-position))
    (insert " edited")
    (let ((capture (appkit-compose-capture)))
      (appkit-chat-compose-goto-part nil)
      (should (equal (appkit-chat-compose-body) ""))
      (should-not (plist-get (appkit-chat-compose-current-item) :attachments))
      (should (equal (appkit-compose-capture) capture))
      (appkit-chat-compose-goto-part nil)
      (should (equal (appkit-compose-capture) capture)))
    (insert "fresh")
    (should (equal (appkit-chat-compose-bodies)
                   '("first edited" "last" "fresh")))
    (should-not (plist-get (car (last (appkit-chat-compose-items)))
                           :attachments))))

(ert-deftest appkit-chat-compose-frozen-entrypoints-preserve-draft ()
  "Rejected navigation and structural edits leave the frozen draft intact."
  (with-temp-buffer
    (appkit-chat-compose-mode)
    (appkit-chat-compose-setup)
    (appkit-chat-compose-set-items
     '((:text "first" :attachments ((:path "first.png")))
       (:text "last" :attachments ((:path "last.png")))))
    (appkit-chat-compose-goto-part 0)
    (goto-char (appkit-chat-compose-body-end-position))
    (insert " edited")
    (let ((capture (appkit-compose-capture))
          (text (buffer-string))
          (buffer-read-only t))
      (dolist (edit (list (lambda () (appkit-chat-compose-goto-part 1))
                          (lambda () (appkit-chat-compose-goto-part nil))
                          #'appkit-chat-compose-edit-at-point
                          #'appkit-chat-compose-add-item
                          (lambda () (appkit-chat-compose-drop-item 0))
                          (lambda () (appkit-chat-compose-drop-item 1))
                          (lambda ()
                            (appkit-chat-compose-update-current-item
                             '(:attachments ((:path "replacement.png")))))
                          (lambda ()
                            (appkit-chat-compose-set-items '((:text "replacement"))))))
        (should-error (funcall edit) :type 'buffer-read-only)
        (should (equal (appkit-compose-capture) capture))
        (should (equal (buffer-string) text))))))

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
  "Authorized settlement of a frozen draft preserves its owning operation."
  (with-temp-buffer
    (appkit-chat-compose-mode)
    (appkit-chat-compose-setup)
    (appkit-compose-touch)
    (let* ((canceled 0)
           (generation (appkit-compose-generation))
           (owner (appkit-compose-operation-begin
                   'publish :cancel-function (lambda () (cl-incf canceled))))
           (buffer-read-only t))
      (let ((inhibit-read-only t))
        (appkit-chat-compose-set-items
         '((:text "remaining" :attachments ((:id "uploaded"))))))
      (should (appkit-compose-operation-current-p owner))
      (should (= generation (appkit-compose-generation)))
      (should (= 0 canceled))
      (should (appkit-compose-operation-finish owner))
      (should-not (appkit-compose-operation-active-p))
      (should (= 0 canceled)))))

(provide 'appkit-chat-compose-test)

;;; appkit-chat-compose-test.el ends here
