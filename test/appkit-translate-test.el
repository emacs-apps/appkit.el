;;; appkit-translate-test.el --- Translation boundaries -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit-test-helper)
(require 'appkit-translate)

(ert-deftest appkit-translate-replacement-revokes-old-result-not-other-source ()
  (appkit-test-with-surface
    (let* (callbacks cancellations
           (backend (list :id 'one :label "One"
                          :start (lambda (source _language resolve _reject)
                                   (let ((text (plist-get source :text)))
                                     (push (cons text resolve) callbacks)
                                     (lambda () (push text cancellations))))))
           (old '(:key a :version "old" :text "Old text"))
           (new '(:key a :version "new" :text "New text"))
           (other '(:key b :version "b" :text "Other text")))
      (appkit-translate-request old backend "zh")
      (appkit-translate-request other backend "zh")
      (let ((obsolete (appkit-translate-state old)))
        (appkit-translate-request new backend "zh")
        (should (equal cancellations '("Old text")))
        (funcall (cdr (assoc "Old text" callbacks)) "Obsolete translation")
        (should-not (appkit-translate-state old))
        (should (eq (plist-get (appkit-translate-state new) :status) 'running))
        ;; A button from the old rendered region has lost its authority too.
        (appkit-translate-hide obsolete)
        (should (plist-get (appkit-translate-state new) :visible))
        (funcall (cdr (assoc "New text" callbacks)) "Current translation")
        (should (equal (plist-get (appkit-translate-state new) :text) "Current translation"))
        (should (eq (plist-get (appkit-translate-state other) :status) 'running))
        (appkit-translate-hide (appkit-translate-state other))
        (funcall (cdr (assoc "Other text" callbacks)) "Cancelled translation")
        (should (eq (plist-get (appkit-translate-state other) :status) 'cancelled))
        (should (equal (plist-get (appkit-translate-state new) :text) "Current translation"))))))

(ert-deftest appkit-translate-cache-is-bound-to-language-and-backend ()
  (appkit-test-with-surface
    (let* ((source '(:key a :version 1 :text "Hello")) calls
           (start (lambda (_source language resolve _reject)
                    (push language calls)
                    (funcall resolve (concat "  " language "\n"))
                    nil))
           (first (list :id '(model one) :label "One" :start start))
           (second (list :id '(model two) :label "Two" :start start)))
      (appkit-translate-request source first "zh")
      (let ((state (appkit-translate-state source)))
        (appkit-translate-hide state)
        (appkit-translate-request source first "zh")
        (should (equal calls '("zh")))
        (should (equal (plist-get state :text) "  zh\n"))
        (should (plist-get state :visible)))
      (appkit-translate-request source first "ja")
      (should (equal calls '("ja" "zh")))
      (appkit-translate-request source second "ja")
      (should (equal calls '("ja" "ja" "zh")))
      (appkit-translate-request source second "ja" t)
      (should (equal calls '("ja" "ja" "ja" "zh"))))))

(ert-deftest appkit-translate-queued-source-is-frozen-and-owner-close-revokes-it ()
  (appkit-test-with-surface
    (let* (callbacks seen
           (backend (list :id 'one :label "One"
                          :start (lambda (source _language resolve _reject)
                                   (push (plist-get source :text) seen)
                                   (push resolve callbacks)
                                   nil)))
           (text (copy-sequence "Original"))
           (version (vector (copy-sequence "Version")))
           (source (list :key 'queued :version version :text text)))
      (appkit-translate-request '(:key a :version 1 :text "First") backend "zh")
      (appkit-translate-request '(:key b :version 1 :text "Second") backend "zh")
      (appkit-translate-request source backend "zh")
      (should (eq (plist-get (appkit-translate-state source) :status) 'queued))
      (aset text 0 ?X)
      (aset (aref version 0) 0 ?X)
      (funcall (cadr callbacks) "First completed")
      (should (equal (car seen) "Original"))
      (should-not (appkit-translate-state source))
      (let ((state (appkit-translate-state '(:key queued :version ["Version"])))
            (late (car callbacks)))
        (should (eq (plist-get state :status) 'running))
        (appkit-surface-stop appkit-test-surface)
        (funcall late "Late translation")
        (should-not (equal (plist-get state :text) "Late translation"))))))

(ert-deftest appkit-translate-surface-lifecycle-detaches-cleanly ()
  "Stopping a Surface revokes its translations, and requests require a live surface."
  (appkit-test-with-surface
    (let* (notifications
           (notify (lambda (key) (push key notifications)))
           (backend (list :id 'echo :label "Echo"
                          :start (lambda (source _lang resolve _reject)
                                   (funcall resolve (plist-get source :text))
                                   nil)))
           (source '(:key msg1 :version 1 :text "Hello world")))
      (appkit-translate-enable appkit-test-surface notify)
      (appkit-translate-request source backend "zh")
      (should (equal (plist-get (appkit-translate-state source) :text) "Hello world"))
      (should (member 'msg1 notifications))
      ;; Stopping the surface cleans up the translation context
      (appkit-surface-stop appkit-test-surface)
      (should-error (appkit-translate-request source backend "zh") :type 'user-error))))

(ert-deftest appkit-translate-insert-renders-buttons-and-triggers-action ()
  "Inserting translations produces inline region with working Hide and Show actions."
  (appkit-test-with-surface
    (let* ((backend (list :id 'echo :label "Echo"
                          :start (lambda (source _lang resolve _reject)
                                   (funcall resolve (concat "Translated: " (plist-get source :text)))
                                   nil)))
           (source '(:key msg1 :version 1 :text "Sample text")))
      (appkit-translate-request source backend "zh")
      (with-temp-buffer
        (appkit-translate-insert source "  " nil appkit-test-surface)
        (should (string-match-p "Translation · zh · Echo" (buffer-string)))
        (should (string-match-p "Translated: Sample text" (buffer-string)))
        (should (string-match-p "Hide" (buffer-string)))
        (should (string-match-p "Translate again" (buffer-string))))
      (appkit-translate-hide source appkit-test-surface)
      (with-temp-buffer
        (appkit-translate-insert source "  " nil appkit-test-surface)
        (should (string-match-p "Show" (buffer-string)))
        (should-not (string-match-p "Translated: Sample text" (buffer-string)))))))

(ert-deftest appkit-translate-batch-freezes-settings-and-isolates-failure ()
  (appkit-test-with-surface
    (let* ((appkit-translate-target-language "zh")
           (factories 0)
           callbacks calls
           (appkit-translate-backend-function
            (lambda ()
              (cl-incf factories)
              (list :id factories :label "Batch"
                    :start
                    (lambda (source language resolve reject)
                      (let ((key (plist-get source :key)))
                        (push (list key language) calls)
                        (push (list key resolve reject) callbacks)
                        ;; A synchronous callback can change global settings,
                        ;; but must not split this batch across configurations.
                        (setq appkit-translate-target-language "ja")
                        nil)))))
           (sources '((:key a :version 1 :text "First")
                      (:key b :version 1 :text "Second")
                      (:key c :version 1 :text "Third")))
           (states (appkit-translate-request-many sources)))
      (should (equal (reverse calls) '((a "zh") (b "zh"))))
      (funcall (nth 2 (assq 'a callbacks)) "Service rejected first message")
      (should (equal (reverse calls) '((a "zh") (b "zh") (c "zh"))))
      (funcall (nth 1 (assq 'b callbacks)) "第二条")
      (funcall (nth 1 (assq 'c callbacks)) "第三条")
      (should (= factories 1))
      (should (equal (mapcar (lambda (state) (plist-get state :status)) states)
                     '(failed completed completed)))
      (should (equal (plist-get (appkit-translate-state (nth 2 sources)) :text)
                     "第三条")))))

(provide 'appkit-translate-test)
;;; appkit-translate-test.el ends here
