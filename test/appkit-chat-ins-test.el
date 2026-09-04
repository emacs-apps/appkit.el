;;; appkit-chat-ins-test.el --- Chat insertion tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'button)
(require 'appkit-chat-ins)

(ert-deftest appkit-chat-ins-insert-full-width-divider-applies-face-and-properties ()
  (with-temp-buffer
    (let ((span (appkit-chat-ins-insert-full-width-divider "Label" 'shadow 24 '(demo t))))
      (should (string-match-p "( Label )" (buffer-string)))
      (should (eq t (get-text-property (car span) 'demo)))
      (let ((face (get-text-property (car span) 'face)))
        (should (or (eq face 'shadow)
                    (and (listp face) (memq 'shadow face))))))))

(ert-deftest appkit-chat-ins-insert-divider-row-is-read-only ()
  (with-temp-buffer
    (let ((span (appkit-chat-ins-insert-divider-row "Unread" 'shadow 20 '(section unread))))
      (should (string-match-p "Unread" (buffer-string)))
      (should (eq t (get-text-property (car span) 'read-only)))
      (should (equal 'unread (get-text-property (car span) 'section))))))

(ert-deftest appkit-chat-ins-insert-reaction-line-supports-adapter-label-and-action ()
  (with-temp-buffer
    (let ((reaction '((code . "178") (count . 3) (chosen-p . t)))
          clicked)
      (appkit-chat-ins-insert-reaction-line
       (list reaction)
       :selected-face 'success
       :unselected-face 'shadow
       :label-function
       (lambda (item)
         (format " face-%s %d "
                 (alist-get 'code item) (alist-get 'count item)))
       :selected-p-function (lambda (item) (alist-get 'chosen-p item))
       :action-function (lambda (item) (setq clicked item))
       :help-echo-function (lambda (_item) "Toggle reaction"))
      (goto-char (point-min))
      (search-forward "face-178 3")
      (let ((button (button-at (match-beginning 0))))
        (should button)
        (should (equal (button-get button 'help-echo) "Toggle reaction"))
        (button-activate button)
        (should (equal clicked reaction))))))

(ert-deftest appkit-chat-ins-insert-right-aligned-text-uses-target-width ()
  (with-temp-buffer
    (insert "Alice")
    (let ((span (appkit-chat-ins-insert-right-aligned-text
                 "12:34" 30 :face 'shadow)))
      (should (equal (buffer-substring-no-properties
                      (car span) (cdr span))
                     " 12:34"))
      (should (equal (get-text-property (car span) 'display)
                     '(space :align-to 25)))
      (should (eq (get-text-property (1- (point)) 'face) 'shadow)))))

(ert-deftest appkit-chat-ins-insert-right-aligned-text-tracks-window-edge ()
  (with-temp-buffer
    (insert "Alice")
    (let ((span (appkit-chat-ins-insert-right-aligned-text
                 "12:34" 30 :right-edge-margin 2)))
      (should (equal (buffer-substring-no-properties
                      (car span) (cdr span))
                     " 12:34"))
      (should (equal (get-text-property (car span) 'display)
                     '(space :align-to (- right (7 . width))))))))

(ert-deftest appkit-chat-ins-insert-right-aligned-text-reserves-future-prefix ()
  (with-temp-buffer
    (insert (make-string 20 ?x))
    (let ((span (appkit-chat-ins-insert-right-aligned-text
                 "12:34" 30 :left-prefix-width 4)))
      (should (= (car span) (line-beginning-position)))
      (should (= (line-number-at-pos) 2))
      (should (equal (get-text-property (car span) 'display)
                     '(space :align-to 25))))))

(ert-deftest appkit-chat-ins-insert-media-card-stores-context-and-prefixes ()
  (with-temp-buffer
    (let* ((context (appkit-media-card-context-create
                     :kind 'image :title "photo.png"))
           (span (appkit-chat-ins-insert-media-card
                  :kind 'image
                  :title "photo.png"
                  :details '("12 KiB" "40×30")
                  :meta "image/png"
                  :prefix "│ "
                  :context context)))
      (should (string-match-p (regexp-quote "[image] photo.png (12 KiB, 40×30)")
                              (buffer-string)))
      (should (eq context
                  (get-text-property
                   (car span) appkit-media-card-context-property)))
      (should (equal "│ "
                     (get-text-property (car span) 'line-prefix))))))

(ert-deftest appkit-chat-ins-insert-media-card-renders-transfer-control ()
  (with-temp-buffer
    (appkit-chat-ins-insert-media-card
     :kind 'file
     :title "notes.pdf"
     :transfer (list :direction 'upload
                     :state 'active
                     :bytes-done 128
                     :bytes-total 256
                     :bar-width 10))
    (should (string-match-p "\\[\\+\\+\\+\\+>     \\] 128/256"
                            (buffer-string)))))

(ert-deftest appkit-chat-ins-voice-note-text-clamps-progress-and-formats-hours ()
  (should
   (equal
    (appkit-chat-ins-voice-note-text
     :state 'playing
     :duration-seconds 3661
     :played-seconds 9000
     :bar-width 4)
    "⏸ ━━━━ 1:01:01 / 1:01:01"))
  (should
   (equal
    (appkit-chat-ins-voice-note-text
     :state 'idle
     :duration-seconds nil
     :played-seconds 0
     :bar-width 3)
    "▶ ─── 0:00 / --:--")))

(ert-deftest appkit-chat-ins-transfer-text-uses-bytes-and-direction ()
  (should (equal (appkit-chat-ins-transfer-measure 0.25 "4096" "8192")
                 "4096/8192"))
  (should (equal (appkit-chat-ins-transfer-measure 0.25 nil nil) "25%"))
  (should (equal
           (appkit-chat-ins-transfer-text
            :direction 'download
            :state 'active
            :bytes-done 4096
            :bytes-total 8192
            :bar-width 10)
           "[====>     ] 4096/8192"))
  (should (equal
           (appkit-chat-ins-transfer-text
            :direction 'upload
            :state 'active
            :progress 0.5
            :bar-width 10)
           "[++++>     ] 50%"))
  (should (string-match-p "⏸"
                          (appkit-chat-ins-transfer-text
                           :direction 'download
                           :state 'paused
                           :progress 0.5
                           :bar-width 4))))

(ert-deftest appkit-chat-ins-insert-transfer-keeps-action-on-the-button ()
  (with-temp-buffer
    (let (canceled)
      (let ((span
             (appkit-chat-ins-insert-transfer
              :direction 'download
              :state 'active
              :bytes-done 4096
              :bytes-total 8192
              :action (lambda () (setq canceled t))
              :action-label "Cancel"
              :prefix "│ ")))
        (should (string-match-p "\\[====>     \\] 4096/8192"
                                (buffer-string)))
        (should (string-match-p "\\[Cancel\\]" (buffer-string)))
        (should (equal "│ "
                       (get-text-property (car span) 'line-prefix)))
        (goto-char (point-min))
        (search-forward "[Cancel]")
        (push-button (match-beginning 0))
        (should canceled)))))

(provide 'appkit-chat-ins-test)

;;; appkit-chat-ins-test.el ends here
