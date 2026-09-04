;;; appkit-media-resource-test.el --- Tests for media resources -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-media-resource)

(appkit-define-app-kind appkit-media-test)

(ert-deftest appkit-media-resource-classifies-canonical-resources ()
  (should (equal "cat%20photo.PNG"
                 (appkit-media-url-filename
                  "https://cdn.example/media/cat%20photo.PNG?token=1#preview")))
  (should (equal "explicit-name.bin"
                 (appkit-media-resource-name
                  '((name . "explicit-name.bin")
                    (url . "https://cdn.example/fallback.png")))))
  (should (equal "local.webp"
                 (appkit-media-resource-name
                  '((file . "/tmp/local.webp")))))
  (should (eq 'video
              (appkit-media-resource-kind
               '((mime-type . "VIDEO/MP4")
                 (name . "opaque.bin")))))
  (should (eq 'image
              (appkit-media-resource-kind
               '((name . "photo.JPEG?download=1")))))
  (should (eq 'video
              (appkit-media-resource-kind
               '((url . "https://cdn.example/clip.webm#fragment")))))
  (should (eq 'file
              (appkit-media-resource-kind
               '((name . "report.pdf")))))
  (should-error
   (appkit-media-resource-kind '((name . "photo.png")) 'custom)))

(ert-deftest appkit-media-resource-construction-is-strict-and-canonical ()
  (should
   (equal '((file . "/tmp/image.png")
            (name . "image.png")
            (mime-type . "image/png"))
          (appkit-media-resource-create
           :file "/tmp/image.png"
           :name "image.png"
           :mime-type "image/png")))
  (let* ((resource '((url . "https://example.invalid/image.png")))
         (normalized (appkit-media-resource-normalize resource)))
    (should (equal resource normalized))
    (should-not (eq resource normalized)))
  (dolist (legacy '(((filename . "legacy.png"))
                    ((file_name . "legacy.png"))
                    ((content_type . "image/png"))))
    (should-error (appkit-media-resource-normalize legacy)))
  (should-error (appkit-media-resource-normalize '((name . ""))))
  (should-error
   (appkit-media-resource-normalize
    '((name . "first") (name . "second")))))

(ert-deftest appkit-media-resource-sanitizes-untrusted-filenames ()
  (should (equal "folder_file_name.txt"
                 (appkit-media-sanitize-filename
                  (concat "folder/file" (string ?\n) "name.txt"))))
  (should (equal "media.bin" (appkit-media-sanitize-filename nil)))
  (should (appkit-media-image-file-name-p "photo.WeBp?token=1"))
  (should (appkit-media-video-file-name-p "clip.MP4#play"))
  (should-not (appkit-media-image-file-name-p "archive.png.zip")))

(ert-deftest appkit-media-file-presence-requires-a-regular-file ()
  (let ((directory (make-temp-file "appkit-media-file-" t))
        file)
    (unwind-protect
        (progn
          (setq file (expand-file-name "payload.bin" directory))
          (with-temp-file file (insert "payload"))
          (should (appkit-media-file-present-p file))
          (should (appkit-media-readable-file-p file))
          (should-not (appkit-media-file-present-p directory))
          (should-not (appkit-media-readable-file-p directory)))
      (delete-directory directory t))))

(ert-deftest appkit-media-file-reader-enforces-regular-file-at-both-boundaries ()
  (let* ((directory (make-temp-file "appkit-media-reader-" t))
         (file (expand-file-name "payload.bin" directory))
         reader-arguments)
    (unwind-protect
        (progn
          (with-temp-file file (insert "payload"))
          (cl-letf (((symbol-function 'read-file-name)
                     (lambda (&rest arguments)
                       (setq reader-arguments arguments)
                       file)))
            (should
             (equal file
                    (appkit-media-read-file-name
                     "Attachment: " directory)))
            (let ((accept (nth 3 reader-arguments)))
              (should (functionp accept))
              (should (funcall accept file))
              (should-not (funcall accept directory))))
          ;; Recheck the result independently of the reader.  Native graphical
          ;; dialogs are not required to honor the acceptance function.
          (cl-letf (((symbol-function 'read-file-name)
                     (lambda (&rest _) directory)))
            (should-error
             (appkit-media-read-file-name "Attachment: " directory)
             :type 'user-error)))
      (delete-directory directory t))))

(ert-deftest appkit-media-local-resource-copy-completes-synchronously ()
  (let* ((directory (make-temp-file "appkit-media-copy" t))
         (source (expand-file-name "source.bin" directory))
         (target (expand-file-name "nested/target.bin" directory))
         success-value
         failure)
    (unwind-protect
        (progn
          (with-temp-file source
            (insert "local media bytes"))
          (appkit-media-copy-or-download-resource-async
           `((file . ,source))
           target
           (lambda (file) (setq success-value file))
           (lambda (reason) (setq failure reason)))
          (should (equal target success-value))
          (should-not failure)
          (should (file-exists-p target))
          (should (equal "local media bytes"
                         (with-temp-buffer
                           (insert-file-contents-literally target)
                           (buffer-string)))))
      (delete-directory directory t))))

(ert-deftest appkit-media-open-local-resource-opens-and-reports-canonical-copy ()
  (let* ((directory (make-temp-file "appkit-media-open-local" t))
         (file (expand-file-name "report.txt" directory))
         opened
         updated
         (resource `((file . ,file) (name . "report.txt"))))
    (unwind-protect
        (progn
          (with-temp-file file (insert "report"))
          (let ((appkit-media-animate-gifs nil)
                (appkit-media-open-file-function
                 (lambda (path)
                   (setq opened path)
                   :opened)))
            (should
             (eq :opened
                 (appkit-media-open-resource
                  resource
                  :kind 'file
                  :cache-update-function
                  (lambda (local-resource)
                    (setq updated local-resource))))))
          (should (equal file opened))
          (should (equal file (alist-get 'file updated)))
          (should (equal "report.txt" (alist-get 'name updated)))
          (should (equal resource `((file . ,file) (name . "report.txt")))))
      (delete-directory directory t))))

(ert-deftest appkit-media-remote-resource-dispatches-through-shared-scheduler ()
  (let* ((directory (make-temp-file "appkit-media-remote" t))
         (target (expand-file-name "nested/report.pdf" directory))
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         plz-arguments
         curl-arguments
         success-value
         failure)
    (unwind-protect
        (progn
          (make-directory (file-name-directory target) t)
          (with-temp-file target (insert "old contents"))
          (cl-letf (((symbol-function 'plz)
                     (lambda (&rest arguments)
                       (setq plz-arguments arguments
                             curl-arguments plz-curl-default-args)
                       :remote-process))
                    ((symbol-function 'url-copy-file)
                     (lambda (&rest _)
                       (ert-fail
                        "generic remote files must use asynchronous plz"))))
            (should
             (appkit-media-transfer-p
              (appkit-media-copy-or-download-resource-async
               '((url . "https://example.invalid/report.pdf"))
               target
               (lambda (file) (setq success-value file))
               (lambda (reason) (setq failure reason)))))
            (should (eq 'get (nth 0 plz-arguments)))
            (should (equal "https://example.invalid/report.pdf"
                           (nth 1 plz-arguments)))
            (should (equal "--disable" (car curl-arguments)))
            (should
             (equal
              '("--proto" "=https" "--proto-redir" "=https"
                "--max-redirs" "5")
              (last curl-arguments 6)))
            (let ((properties (nthcdr 2 plz-arguments)))
              (let ((part (cadr (plist-get properties :as))))
                (should (string-suffix-p "/download.part" part))
                (should (file-directory-p (file-name-directory part)))
                (should-not (file-exists-p part))
                (should (equal "old contents"
                               (with-temp-buffer
                                 (insert-file-contents-literally target)
                                 (buffer-string))))
                (with-temp-file part (insert "new remote contents"))
                (funcall (plist-get properties :then) part)
                (should-not (file-exists-p part))
                (should-not (file-directory-p (file-name-directory part))))
              (should (eq t (plist-get properties :noquery)))
              (should (functionp (plist-get properties :then)))
              (should (functionp (plist-get properties :else)))
              (should (equal "new remote contents"
                             (with-temp-buffer
                               (insert-file-contents-literally target)
                               (buffer-string)))))
            (should (equal target success-value))
            (should-not failure)
            (should (= 0 appkit-media--active-transfer-count))))
      (delete-directory directory t))))

(ert-deftest appkit-media-remote-transfer-deduplicates-and-broadcasts-success ()
  (let* ((directory (make-temp-file "appkit-media-dedupe" t))
         (target (expand-file-name "shared.bin" directory))
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         plz-arguments
         (queue-count 0)
         handles
         successes
         failures)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest arguments)
                     (cl-incf queue-count)
                     (setq plz-arguments arguments)
                     :remote-process)))
          (dolist (id '(first second))
            (push
             (appkit-media-copy-or-download-resource-async
              '((url . "https://example.invalid/shared.bin")) target
              (lambda (file) (setq successes
                                   (append successes (list (cons id file)))))
              (lambda (reason) (push (cons id reason) failures)))
             handles))
          (should (= 1 queue-count))
          (should-not (eq (car handles) (cadr handles)))
          (let* ((properties (nthcdr 2 plz-arguments))
                 (part (cadr (plist-get properties :as))))
            (with-temp-file part (insert "complete"))
            (funcall (plist-get properties :then) part))
          (should (equal `((first . ,target) (second . ,target)) successes))
          (should-not failures)
          (should (= 0 (hash-table-count appkit-media--inflight-transfers))))
      (delete-directory directory t))))

(ert-deftest appkit-media-deduplicated-callers-cancel-independently ()
  (let* ((directory (make-temp-file "appkit-media-caller-cancel" t))
         (target (expand-file-name "shared.bin" directory))
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         plz-arguments
         first-success
         first-error
         second-success
         second-error)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest arguments)
                     (setq plz-arguments arguments)
                     nil)))
          (let ((first
                 (appkit-media-copy-or-download-resource-async
                  '((url . "https://example.invalid/shared.bin")) target
                  (lambda (file) (setq first-success file))
                  (lambda (reason) (setq first-error reason))))
                (second
                 (appkit-media-copy-or-download-resource-async
                  '((url . "https://example.invalid/shared.bin")) target
                  (lambda (file) (setq second-success file))
                  (lambda (reason) (setq second-error reason)))))
            (should-not (eq first second))
            (should (appkit-media-cancel-transfer first))
            (should (equal "transfer canceled" first-error))
            (should-not first-success)
            (let* ((properties (nthcdr 2 plz-arguments))
                   (part (cadr (plist-get properties :as))))
              (with-temp-file part (insert "complete"))
              (funcall (plist-get properties :then) part))
            (should (equal target second-success))
            (should-not second-error)))
      (delete-directory directory t))))

(ert-deftest appkit-media-transfer-rejects-conflicting-source-for-target ()
  (let* ((directory (make-temp-file "appkit-media-conflict" t))
         (target (expand-file-name "shared.bin" directory))
         (local (expand-file-name "local.bin" directory))
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         plz-arguments
         (plz-count 0)
         remote-error
         local-error)
    (unwind-protect
        (progn
          (with-temp-file local (insert "local"))
          (cl-letf (((symbol-function 'plz)
                     (lambda (&rest arguments)
                       (cl-incf plz-count)
                       (setq plz-arguments arguments)
                       nil)))
            (appkit-media-copy-or-download-resource-async
             '((url . "https://example.invalid/first.bin")) target
             #'ignore #'ignore)
            (should-not
             (appkit-media-copy-or-download-resource-async
              '((url . "https://example.invalid/second.bin")) target
              #'ignore (lambda (reason) (setq remote-error reason))))
            (should-not
             (appkit-media-copy-or-download-resource-async
              `((file . ,local)) target
              #'ignore (lambda (reason) (setq local-error reason))))
            (should (= 1 plz-count))
            (should (string-match-p "another source" remote-error))
            (should (string-match-p "another source" local-error))
            (funcall (plist-get (nthcdr 2 plz-arguments) :else)
                     "test cleanup")))
      (delete-directory directory t))))

(ert-deftest appkit-media-transfer-broadcast-survives-caller-quit ()
  (let* ((directory (make-temp-file "appkit-media-callback-quit" t))
         (target (expand-file-name "shared.bin" directory))
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         plz-arguments
         second-called)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest arguments)
                     (setq plz-arguments arguments)
                     nil))
                  ((symbol-function 'message) #'ignore))
          (appkit-media-copy-or-download-resource-async
           '((url . "https://example.invalid/shared.bin")) target
           (lambda (_file) (signal 'quit nil)) #'ignore)
          (appkit-media-copy-or-download-resource-async
           '((url . "https://example.invalid/shared.bin")) target
           (lambda (_file) (setq second-called t)) #'ignore)
          (let* ((properties (nthcdr 2 plz-arguments))
                 (part (cadr (plist-get properties :as))))
            (with-temp-file part (insert "complete"))
            (funcall (plist-get properties :then) part))
          (should second-called))
      (delete-directory directory t))))

(ert-deftest appkit-media-delayed-start-error-finishes-pending-transfer ()
  (let* ((directory (make-temp-file "appkit-media-delayed-error" t))
         (first-target (expand-file-name "first.bin" directory))
         (second-target (expand-file-name "second.bin" directory))
         (appkit-media-transfer-concurrency 1)
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         first-arguments
         (plz-count 0)
         second-error)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest arguments)
                     (cl-incf plz-count)
                     (if (= plz-count 1)
                         (progn (setq first-arguments arguments) nil)
                       (error "delayed setup exploded")))))
          (appkit-media-copy-or-download-resource-async
           '((url . "https://example.invalid/first.bin")) first-target
           #'ignore #'ignore)
          (appkit-media-copy-or-download-resource-async
           '((url . "https://example.invalid/second.bin")) second-target
           #'ignore (lambda (reason) (setq second-error reason)))
          (should (= 1 (length appkit-media--pending-transfers)))
          (let* ((properties (nthcdr 2 first-arguments))
                 (part (cadr (plist-get properties :as))))
            (with-temp-file part (insert "first"))
            (funcall (plist-get properties :then) part))
          (should (string-match-p "delayed setup exploded" second-error))
          (should-not appkit-media--pending-transfers)
          (should (= 0 appkit-media--active-transfer-count))
          (should (= 0 (hash-table-count appkit-media--inflight-transfers))))
      (delete-directory directory t))))

(ert-deftest appkit-media-remote-transfer-else-cleans-partial-and-broadcasts ()
  (let* ((directory (make-temp-file "appkit-media-else" t))
         (target (expand-file-name "shared.bin" directory))
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         plz-arguments
         successes
         failures)
    (unwind-protect
        (progn
          (with-temp-file target (insert "previous complete value"))
          (cl-letf (((symbol-function 'plz)
                     (lambda (&rest arguments)
                       (setq plz-arguments arguments)
                       :remote-process)))
            (dotimes (id 2)
              (appkit-media-copy-or-download-resource-async
               '((url . "https://example.invalid/shared.bin")) target
               (lambda (file) (push (cons id file) successes))
               (lambda (reason) (push (cons id reason) failures))))
            (let* ((properties (nthcdr 2 plz-arguments))
                   (part (cadr (plist-get properties :as)))
                   (staging (file-name-directory part)))
              (with-temp-file part (insert "incomplete"))
              (funcall (plist-get properties :else) "network failed")
              (should-not (file-exists-p part))
              (should-not (file-directory-p staging))))
          (should-not successes)
          (should (= 2 (length failures)))
          (should (seq-every-p
                   (lambda (entry) (equal "network failed" (cdr entry)))
                   failures))
          (should (equal "previous complete value"
                         (with-temp-buffer
                           (insert-file-contents-literally target)
                           (buffer-string))))
          (should (= 0 (hash-table-count appkit-media--inflight-transfers))))
      (delete-directory directory t))))

(ert-deftest appkit-media-remote-transfer-cleans-up-synchronous-enqueue-error ()
  (let* ((directory (make-temp-file "appkit-media-enqueue-error" t))
         (target (expand-file-name "target.bin" directory))
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         part
         success-value
         failure)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest arguments)
                     (setq part (cadr (plist-get (nthcdr 2 arguments) :as)))
                     (with-temp-file part (insert "partial"))
                     (error "enqueue exploded"))))
          (appkit-media-copy-or-download-resource-async
           '((url . "https://example.invalid/target.bin")) target
           (lambda (file) (setq success-value file))
           (lambda (reason) (setq failure reason)))
          (should-not success-value)
          (should (string-match-p "enqueue exploded" failure))
          (should part)
          (should-not (file-exists-p part))
          (should-not (file-directory-p (file-name-directory part)))
          (should-not (file-exists-p target))
          (should (= 0 (hash-table-count appkit-media--inflight-transfers))))
      (delete-directory directory t))))

(ert-deftest appkit-media-transfer-cleans-staging-on-enqueue-construction-error ()
  (let* ((directory (make-temp-file "appkit-media-construction-error" t))
         (target (expand-file-name "target.bin" directory))
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         transfer
         failure)
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-media--enqueue-transfer)
                   (lambda (candidate)
                     (setq transfer candidate)
                     (error "constructor enqueue failed"))))
          (should-not
           (appkit-media-copy-or-download-resource-async
            '((url . "https://example.invalid/target.bin")) target
            #'ignore (lambda (reason) (setq failure reason))))
          (should (string-match-p "constructor enqueue failed" failure))
          (should (appkit-media--transfer-p transfer))
          (should-not
           (file-directory-p
            (appkit-media--transfer-staging-directory transfer)))
          (should-not appkit-media--pending-transfers)
          (should (= 0 appkit-media--active-transfer-count))
          (should (= 0 (hash-table-count
                        appkit-media--inflight-transfers))))
      (delete-directory directory t))))

(ert-deftest appkit-media-remote-transfer-rejects-missing-partial ()
  (let* ((directory (make-temp-file "appkit-media-missing-part" t))
         (target (expand-file-name "target.bin" directory))
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         plz-arguments
         success-value
         failure)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest arguments)
                     (setq plz-arguments arguments)
                     :remote-process)))
          (appkit-media-copy-or-download-resource-async
           '((url . "https://example.invalid/target.bin")) target
           (lambda (file) (setq success-value file))
           (lambda (reason) (setq failure reason)))
          (let* ((properties (nthcdr 2 plz-arguments))
                 (part (cadr (plist-get properties :as)))
                 (staging (file-name-directory part)))
            (funcall (plist-get properties :then) part)
            (should-not (file-directory-p staging)))
          (should-not success-value)
          (should (string-match-p "no regular partial file" failure))
          (should-not (file-exists-p target)))
      (delete-directory directory t))))

(ert-deftest appkit-media-image-cache-normalizes-sniffs-and-cleans-siblings ()
  (let* ((directory (make-temp-file "appkit-media-image-cache" t))
         (source (expand-file-name "opaque" directory))
         (cache-base (expand-file-name "cache/entry" directory))
         (stale (format "%s.jpg" cache-base))
         final
         failure)
    (unwind-protect
        (progn
          (make-directory (file-name-directory cache-base) t)
          (with-temp-file source
            (set-buffer-multibyte nil)
            (insert "\r\n\x89PNG\r\n\x1a\nimage bytes"))
          (with-temp-file stale (insert "stale"))
          (should-not
           (appkit-media-cache-image-resource-async
            `((file . ,source)) cache-base
            (lambda (file) (setq final file))
            (lambda (reason) (setq failure reason))))
          (should-not failure)
          (should (equal (format "%s.png" cache-base) final))
          (should (file-exists-p final))
          (should-not (file-exists-p stale))
          (should
           (equal "\x89PNG"
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally final nil 0 4)
                    (buffer-string)))))
      (delete-directory directory t))))

(ert-deftest appkit-media-image-cache-isolates-success-callback-errors ()
  (let* ((directory (make-temp-file "appkit-media-image-callback" t))
         (source (expand-file-name "source.png" directory))
         (cache-base (expand-file-name "cache/entry" directory))
         (success-count 0)
         errors)
    (unwind-protect
        (progn
          (with-temp-file source
            (set-buffer-multibyte nil)
            (insert "\x89PNG\r\n\x1a\nimage bytes"))
          (should-not
           (appkit-media-cache-image-resource-async
            `((file . ,source)) cache-base
            (lambda (_file)
              (cl-incf success-count)
              (error "consumer callback failed"))
            (lambda (reason) (push reason errors))))
          (should (= 1 success-count))
          (should-not errors)
          (should (file-exists-p (format "%s.png" cache-base))))
      (delete-directory directory t))))

(ert-deftest appkit-media-remote-transfer-cancel-removes-pending-request ()
  (let* ((directory (make-temp-file "appkit-media-cancel" t))
         (target (expand-file-name "target.bin" directory))
         (appkit-media-transfer-concurrency 1)
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 1)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         success-value
         failures)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest _)
                     (ert-fail "pending transfer must not start"))))
          (let ((transfer
                 (appkit-media-copy-or-download-resource-async
                  '((url . "https://example.invalid/target.bin")) target
                  (lambda (file) (setq success-value file))
                  (lambda (reason) (push reason failures)))))
            (should (appkit-media-transfer-p transfer))
            (should (= 1 (length appkit-media--pending-transfers)))
            (should (appkit-media-cancel-transfer transfer))
            (should-not (appkit-media-cancel-transfer transfer))
            (should-not appkit-media--pending-transfers)
            (should-not success-value)
            (should (equal '("transfer canceled") failures))
            (should-not (file-exists-p target))
            (should (= 0 (hash-table-count appkit-media--inflight-transfers)))))
      (delete-directory directory t))))

(ert-deftest appkit-media-remote-image-open-is-asynchronous-and-atomic ()
  (let* ((directory (make-temp-file "appkit-media-open-image" t))
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         plz-arguments
         opened
         updated)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest arguments)
                     (setq plz-arguments arguments)
                     :remote-process))
                  ((symbol-function 'url-copy-file)
                   (lambda (&rest _)
                     (ert-fail "remote image open must use atomic acquisition"))))
          (let ((appkit-media-open-file-function
                 (lambda (file) (setq opened file) :opened))
                (appkit-media-animate-gifs nil))
            (should-not
             (appkit-media-open-resource
              '((url . "https://example.invalid/opaque"))
              :kind 'image
              :cache-directory directory
              :cache-update-function
              (lambda (resource) (setq updated resource))
              :client-label "test"))
            (should-not opened)
            (should-not updated)
            (let* ((properties (nthcdr 2 plz-arguments))
                   (part (cadr (plist-get properties :as))))
              (with-temp-file part
                (set-buffer-multibyte nil)
                (insert "\x89PNG\r\n\x1a\nimage bytes"))
              (funcall (plist-get properties :then) part))
            (should (string-suffix-p ".png" opened))
            (should (file-exists-p opened))
            (should (equal opened (alist-get 'file updated)))))
      (delete-directory directory t))))

(ert-deftest appkit-media-video-streams-and-reuses-complete-playback-cache ()
  (let ((directory (make-temp-file "appkit-video-cache" t))
        viewers
        opened-sources
        cache-files
        cache-callbacks
        closed-players
        updated)
    (unwind-protect
        (cl-letf (((symbol-function 'video-session-create)
                   (lambda (source &rest arguments)
                     (push source opened-sources)
                     (push (plist-get arguments :cache-file) cache-files)
                     (push (plist-get arguments :cache-complete-function)
                           cache-callbacks)
                     (list 'session source)))
                  ((symbol-function 'video-session-live-p)
                   (lambda (session)
                     (and session (not (memq session closed-players)))))
                  ((symbol-function 'video-session-close)
                   (lambda (session)
                     (push session closed-players)))
                  ((symbol-function 'appkit-media-video-session-player)
                   (lambda (_session) 'player))
                  ((symbol-function 'video-player-play) #'ignore)
                  ((symbol-function 'video-session-present)
                   (lambda (_session &rest arguments)
                     (let ((viewer (plist-get arguments :buffer)))
                       (push viewer viewers)
                       viewer)))
                  ((symbol-function
                    'appkit-media-copy-or-download-resource-async)
                   (lambda (&rest _)
                     (ert-fail
                      "video playback must not start an explicit download")))
                  ((symbol-function 'browse-url)
                   (lambda (&rest _)
                     (ert-fail "video playback must not use a browser"))))
          (let ((viewer
                 (appkit-media-play-video-source
                  "https://example.invalid/movie.mp4"
                  "test-client"
                  :cache-key "stable-movie"
                  :cache-directory directory
                  :cache-update-function
                  (lambda (resource)
                    (setq updated resource)))))
            (should (buffer-live-p viewer))
            (with-current-buffer viewer
              (should (eq video-quit-function #'kill-current-buffer))))
          (let ((target (car cache-files))
                (complete (car cache-callbacks)))
            (should
             (equal (car opened-sources)
                    "https://example.invalid/movie.mp4"))
            (should (stringp target))
            (should (functionp complete))
            (make-directory (file-name-directory target) t)
            (with-temp-file target
              (insert "complete video"))
            (funcall complete 'player target)
            (should (equal target (alist-get 'file updated)))
            (kill-buffer (car viewers))
            (let ((viewer
                   (appkit-media-play-video-source
                    "https://example.invalid/rotated.mp4"
                    "test-client"
                    :cache-key "stable-movie"
                    :cache-directory directory)))
              (should (buffer-live-p viewer)))
            (should (equal (car opened-sources) target))
            (should-not (car cache-files))
            (should-not (car cache-callbacks))
            (kill-buffer (car viewers))))
      (dolist (viewer viewers)
        (when (buffer-live-p viewer)
          (kill-buffer viewer)))
      (delete-directory directory t))))

(ert-deftest appkit-media-video-cache-policy-none-streams-without-destination ()
  (let (opened-source cache-file session-arguments closed-players)
    (cl-letf (((symbol-function 'video-session-create)
               (lambda (source &rest arguments)
                 (setq opened-source source
                       session-arguments arguments
                       cache-file (plist-get arguments :cache-file))
                 'session))
              ((symbol-function 'video-session-live-p)
               (lambda (session)
                 (and session (not (memq session closed-players)))))
              ((symbol-function 'video-session-close)
               (lambda (session)
                 (push session closed-players)))
              ((symbol-function 'appkit-media-video-session-player)
               (lambda (_session) 'player))
              ((symbol-function 'video-player-play) #'ignore)
              ((symbol-function 'video-session-present)
               (lambda (_session &rest arguments)
                 (plist-get arguments :buffer))))
      (let ((viewer
             (appkit-media-play-video-source
              "https://example.invalid/movie.mp4" "test"
              :cache-policy 'none :live t
              :request-headers
              '(("Referer" . "https://example.invalid/")))))
        (unwind-protect
            (progn
              (should (buffer-live-p viewer))
              (should
               (equal opened-source
                      "https://example.invalid/movie.mp4"))
              (should-not cache-file)
              (should (eq (plist-get session-arguments :live) t))
              (should
               (equal
                (plist-get session-arguments :request-headers)
                '(("Referer" . "https://example.invalid/")))))
          (when (buffer-live-p viewer)
            (kill-buffer viewer)))))))

(ert-deftest appkit-media-inline-and-dedicated-surfaces-share-one-player ()
  (let ((player (list :position 23.5 :desired-state 'playing))
        (create-count 0)
        inline-player
        presented-player
        viewer
        closed
        callback-surface
        (callback-count 0))
    (cl-letf (((symbol-function 'video-session-create)
               (lambda (&rest _)
                 (cl-incf create-count)
                 'video-session))
              ((symbol-function 'video-session-live-p)
               (lambda (candidate)
                 (and (eq candidate 'video-session) (not closed))))
              ((symbol-function 'video-session-close)
               (lambda (candidate)
                 (should (eq candidate 'video-session))
                 (setq closed t)))
              ((symbol-function 'video-session-player)
               (lambda (candidate)
                 (should (eq candidate 'video-session))
                 player))
              ((symbol-function 'video-player-play) #'ignore)
              ((symbol-function 'video-session-inline-create)
               (lambda (candidate _width _height &rest arguments)
                 (should (eq candidate 'video-session))
                 (setq inline-player player)
                 (list :closed nil
                       :close-function
                       (plist-get arguments :close-function))))
              ((symbol-function 'video-inline-p) #'listp)
              ((symbol-function 'video-inline-closed)
               (lambda (inline) (plist-get inline :closed)))
              ((symbol-function 'appkit-media-video-inline-closed-p)
               (lambda (surface)
                 (plist-get
                  (appkit-media-video-inline-inline surface) :closed)))
              ((symbol-function 'video-inline-close)
               (lambda (inline)
                 (unless (plist-get inline :closed)
                   (setf (plist-get inline :closed) t)
                   (funcall (plist-get inline :close-function) inline))))
              ((symbol-function 'video-session-present)
               (lambda (candidate &rest arguments)
                 (should (eq candidate 'video-session))
                 (setq presented-player player
                       viewer (plist-get arguments :buffer))
                 (with-current-buffer viewer
                   (add-hook
                    'kill-buffer-hook
                    (lambda () (video-session-close candidate)) nil t))
                 viewer)))
      (let* ((session
              (appkit-media-video-session-create
               '((url . "https://example.invalid/shared.mp4"))
               "test" :cache-policy 'none))
             (surface
              (appkit-media-video-inline-create
               session 320 180
               :close-function
               (lambda (closed-surface)
                 (setq callback-surface closed-surface)
                 (cl-incf callback-count)))))
        (setq viewer
              (appkit-media-present-video-session
               session "test" :start nil))
        (should (= create-count 1))
        (should (eq inline-player player))
        (should (eq presented-player player))
        (should (= (plist-get presented-player :position) 23.5))
        (appkit-media-video-inline-close surface)
        (should (eq callback-surface surface))
        (should (= callback-count 1))
        (appkit-media-video-inline-close surface)
        (should (= callback-count 1))
        (should-not closed)
        (kill-buffer viewer)
        (should closed)
        (should-not (appkit-media-video-session-live-p session))))))

(ert-deftest appkit-media-owned-video-buffer-stops-with-app ()
  (let ((app (appkit-app-start 'appkit-media-test :id 'owned
                               :shutdown #'ignore))
        viewer
        closed)
    (unwind-protect
        (cl-letf (((symbol-function 'video-session-create)
                   (lambda (&rest _) 'session))
                  ((symbol-function 'video-session-live-p)
                   (lambda (session) (and session (not closed))))
                  ((symbol-function 'video-session-close)
                   (lambda (_session) (setq closed t)))
                  ((symbol-function 'appkit-media-video-session-player)
                   (lambda (_session) 'player))
                  ((symbol-function 'video-player-play) #'ignore)
                  ((symbol-function 'video-session-present)
                   (lambda (session &rest arguments)
                     (setq viewer (plist-get arguments :buffer))
                     (with-current-buffer viewer
                       (add-hook
                        'kill-buffer-hook
                        (lambda () (video-session-close session)) nil t))
                     viewer)))
          (let ((result
                 (appkit-media-play-video-source
                  "https://example.invalid/owned.mp4" "test" :owner app)))
            (should (eq result viewer)))
          (should (buffer-live-p viewer))
          (should (= 1 (length (appkit-app-handles app))))
          (appkit-app-close app)
          (should-not (buffer-live-p viewer))
          (should closed)
          (should-not (appkit-app-handles app)))
      (when (appkit-app-live-p app)
        (appkit-app-close app))
      (when (buffer-live-p viewer)
        (kill-buffer viewer)))))

(ert-deftest appkit-media-owned-video-buffer-stops-with-view ()
  (let ((app (appkit-app-start 'appkit-media-test :id 'view
                               :shutdown #'ignore))
        (buffer (generate-new-buffer " *appkit-media-view-owner*"))
        viewer
        view
        closed)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq view
                  (appkit-attach-view
                   :app app :id 'video :mode major-mode)))
          (cl-letf (((symbol-function 'video-session-create)
                     (lambda (&rest _) 'session))
                    ((symbol-function 'video-session-live-p)
                     (lambda (session) (and session (not closed))))
                    ((symbol-function 'video-session-close)
                     (lambda (_session) (setq closed t)))
                    ((symbol-function 'appkit-media-video-session-player)
                     (lambda (_session) 'player))
                    ((symbol-function 'video-player-play) #'ignore)
                    ((symbol-function 'video-session-present)
                     (lambda (session &rest arguments)
                       (setq viewer (plist-get arguments :buffer))
                       (with-current-buffer viewer
                         (add-hook
                          'kill-buffer-hook
                          (lambda () (video-session-close session)) nil t))
                       viewer)))
            (let ((result
                   (appkit-media-play-video-source
                    "https://example.invalid/view.mp4" "test" :owner view)))
              (should (eq result viewer)))
            (should (= 1 (length (appkit-view-handles view))))
            (appkit-kill-view view)
            (should-not (buffer-live-p viewer))
            (should closed)
            (should-not (appkit-view-handles view))
            (should-not (appkit-view-live-p view))))
      (when (appkit-app-live-p app)
        (appkit-app-close app))
      (dolist (candidate (list buffer viewer))
        (when (buffer-live-p candidate)
          (kill-buffer candidate))))))

(ert-deftest appkit-media-video-buffer-kill-retires-owner ()
  (let ((app (appkit-app-start 'appkit-media-test :id 'buffer-kill
                               :shutdown #'ignore))
        viewer
        closed)
    (unwind-protect
        (cl-letf (((symbol-function 'video-session-create)
                   (lambda (&rest _) 'session))
                  ((symbol-function 'video-session-live-p)
                   (lambda (session) (and session (not closed))))
                  ((symbol-function 'video-session-close)
                   (lambda (_session) (setq closed t)))
                  ((symbol-function 'appkit-media-video-session-player)
                   (lambda (_session) 'player))
                  ((symbol-function 'video-player-play) #'ignore)
                  ((symbol-function 'video-session-present)
                   (lambda (session &rest arguments)
                     (setq viewer (plist-get arguments :buffer))
                     (with-current-buffer viewer
                       (add-hook
                        'kill-buffer-hook
                        (lambda () (video-session-close session)) nil t))
                     viewer)))
          (appkit-media-play-video-source
           "https://example.invalid/kill.mp4" "test" :owner app)
          (should (= 1 (length (appkit-app-handles app))))
          (with-current-buffer viewer
            (funcall video-quit-function))
          (should-not (buffer-live-p viewer))
          (should closed)
          (should-not (appkit-app-handles app))
          (should (appkit-app-live-p app)))
      (when (appkit-app-live-p app)
        (appkit-app-close app))
      (when (buffer-live-p viewer)
        (kill-buffer viewer)))))

(ert-deftest appkit-media-video-constructor-error-retires-owner ()
  (let ((app (appkit-app-start 'appkit-media-test :id 'constructor-error
                               :shutdown #'ignore))
        viewer
        closed)
    (unwind-protect
        (cl-letf (((symbol-function 'video-session-create)
                   (lambda (&rest _) 'session))
                  ((symbol-function 'video-session-live-p)
                   (lambda (session) (and session (not closed))))
                  ((symbol-function 'video-session-close)
                   (lambda (_session) (setq closed t)))
                  ((symbol-function 'appkit-media-video-session-player)
                   (lambda (_session) 'player))
                  ((symbol-function 'video-player-play) #'ignore)
                  ((symbol-function 'video-session-present)
                   (lambda (_session &rest arguments)
                     (setq viewer (plist-get arguments :buffer))
                     (error "video constructor failed"))))
          (let ((condition
                 (should-error
                  (appkit-media-play-video-source
                   "https://example.invalid/error.mp4" "test" :owner app))))
            (should
             (equal (error-message-string condition)
                    "video constructor failed")))
          (should closed)
          (should-not (buffer-live-p viewer))
          (should-not (appkit-app-handles app)))
      (when (appkit-app-live-p app)
        (appkit-app-close app)))))

(ert-deftest appkit-media-video-constructor-throw-retires-owner ()
  (let ((app (appkit-app-start 'appkit-media-test :id 'constructor-throw
                               :shutdown #'ignore))
        viewer
        closed)
    (unwind-protect
        (cl-letf (((symbol-function 'video-session-create)
                   (lambda (&rest _) 'session))
                  ((symbol-function 'video-session-live-p)
                   (lambda (session) (and session (not closed))))
                  ((symbol-function 'video-session-close)
                   (lambda (_session) (setq closed t)))
                  ((symbol-function 'appkit-media-video-session-player)
                   (lambda (_session) 'player))
                  ((symbol-function 'video-player-play) #'ignore)
                  ((symbol-function 'video-session-present)
                   (lambda (_session &rest arguments)
                     (setq viewer (plist-get arguments :buffer))
                     (throw 'appkit-media-constructor-exit :escaped))))
          (should
           (eq :escaped
               (catch 'appkit-media-constructor-exit
                 (appkit-media-play-video-source
                  "https://example.invalid/throw.mp4" "test" :owner app)
                 :returned)))
          (should closed)
          (should-not (buffer-live-p viewer))
          (should-not (appkit-app-handles app)))
      (when (appkit-app-live-p app)
        (appkit-app-close app)))))

(ert-deftest appkit-media-video-stop-during-open-kills-viewer ()
  (let ((app (appkit-app-start 'appkit-media-test :id 'reentrant
                               :shutdown #'ignore))
        viewer
        closed)
    (cl-letf (((symbol-function 'video-session-create)
               (lambda (&rest _) 'session))
              ((symbol-function 'video-session-live-p)
               (lambda (session) (and session (not closed))))
              ((symbol-function 'video-session-close)
               (lambda (_session) (setq closed t)))
              ((symbol-function 'appkit-media-video-session-player)
               (lambda (_session) 'player))
              ((symbol-function 'video-player-play) #'ignore)
              ((symbol-function 'video-session-present)
               (lambda (_session &rest arguments)
                 (setq viewer (plist-get arguments :buffer))
                 (appkit-app-close app)
                 viewer)))
      (should-error
       (appkit-media-play-video-source
        "https://example.invalid/late.mp4" "test" :owner app))
      (should closed)
      (should-not (buffer-live-p viewer))
      (should-not (appkit-app-handles app)))))

(ert-deftest appkit-media-video-dead-owner-never-opens ()
  (let ((app (appkit-app-start 'appkit-media-test :id 'dead
                               :shutdown #'ignore))
        opened)
    (appkit-app-close app)
    (cl-letf (((symbol-function 'video-session-create)
               (lambda (&rest _)
                 (setq opened t))))
      (should-error
       (appkit-media-play-video-source
        "https://example.invalid/dead.mp4" "test" :owner app))
      (should-not opened))))

(ert-deftest appkit-media-video-entrypoints-forward-exact-owner ()
  (let ((app (appkit-app-start 'appkit-media-test :id 'forward
                               :shutdown #'ignore))
        calls)
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-media-play-video-source)
                   (lambda (source &optional label &rest keys)
                     (push (list source label
                                 (plist-get keys :owner)
                                 (plist-get keys :live)
                                 (plist-get keys :request-headers))
                           calls)
                     :played))
                  ((symbol-function 'appkit-media--play-video-resource)
                   (lambda (resource label &rest keys)
                     (push (list (alist-get 'url resource)
                                 label
                                 (plist-get keys :owner)
                                 (plist-get keys :live)
                                 (plist-get keys :request-headers))
                           calls)
                     :played)))
          (should
           (eq :played
               (appkit-media-play-video-url
                "https://example.invalid/url.mp4" "url" :owner app
                :live t :request-headers '(("Referer" . "url")))))
          (should
           (eq :played
               (appkit-media-open-resource
                '((url . "https://example.invalid/resource.mp4"))
                :kind 'video :client-label "resource" :owner app)))
          (with-temp-buffer
            (insert "video")
            (appkit-media-add-play-video-properties
             (point-min) (point-max) "https://example.invalid/action.mp4"
             "action" :owner app :live t
             :request-headers '(("Referer" . "action")))
            (let* ((map (get-text-property (point-min) 'keymap))
                   (command (lookup-key map (kbd "RET"))))
              (should (commandp command))
              (funcall command)))
          (should
           (equal
            (nreverse calls)
            `(("https://example.invalid/url.mp4" "url" ,app t
               (("Referer" . "url")))
              ("https://example.invalid/resource.mp4" "resource" ,app nil nil)
              ("https://example.invalid/action.mp4" "action" ,app t
               (("Referer" . "action")))))))
      (when (appkit-app-live-p app)
        (appkit-app-close app)))))

(ert-deftest appkit-media-video-rejects-invalid-sources ()
  (should-error
   (appkit-media-play-video-source nil "client")
   :type 'user-error)
  (should-error
   (appkit-media-play-video-file
    "/definitely/missing/appkit-video.mp4" "client")
   :type 'user-error))


(ert-deftest appkit-media-remote-transfer-rejects-unsafe-url-before-dispatch ()
  (let* ((directory (make-temp-file "appkit-media-url-scheme" t))
         (target (expand-file-name "nested/report.pdf" directory))
         dispatched
         failure)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest _)
                     (setq dispatched t))))
          (dolist
              (url
               (list
                "http://example.invalid/report.pdf"
                "https://user@example.invalid/report.pdf"
                (concat "https://example.invalid/report.pdf\""
                        "\n--output \"/tmp/appkit-injected")))
            (setq failure nil)
            (should-not
             (appkit-media-copy-or-download-resource-async
              `((url . ,url))
              target #'ignore (lambda (reason) (setq failure reason))))
            (should (string-match-p "must use HTTPS" failure)))
          (should-not dispatched)
          (should-not (file-directory-p (file-name-directory target))))
      (delete-directory directory t))))

(ert-deftest appkit-media-remote-transfer-rejects-unsafe-headers-before-dispatch ()
  (let* ((directory (make-temp-file "appkit-media-header-safety" t))
         (target (expand-file-name "report.pdf" directory))
         dispatched)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest _)
                     (setq dispatched t))))
          (dolist
              (headers
               (list
                `(("X-Test" . ,(concat "safe\"" "\n--output /tmp/injected")))
                `((,(concat "X-Test\"" "\n--output") . "value"))))
            (should-error
             (appkit-media-copy-or-download-resource-async
              '((url . "https://example.invalid/report.pdf"))
              target #'ignore #'ignore :headers headers)))
          (should-not dispatched))
      (delete-directory directory t))))

(ert-deftest appkit-media-video-rejects-option-and-unsafe-url-sources ()
  (let (opened)
    (cl-letf (((symbol-function 'video-open)
               (lambda (&rest _)
                 (setq opened t))))
      (dolist (source (list "--script=/tmp/evil.lua"
                            "file:///tmp/movie.mp4"
                            "http://example.invalid/movie.mp4"
                            "gopher://example.invalid/movie.mp4"
                            "https://user@example.invalid/movie.mp4"
                            (concat "https://example.invalid/movie.mp4\""
                                    "\n--output /tmp/injected")))
        (should-error
         (appkit-media-play-video-source source "test")
         :type 'user-error))
      (should-not opened))))

(ert-deftest appkit-media-owned-file-open-cancels-and-ignores-late-success ()
  (let* ((directory (make-temp-file "appkit-media-owned-open" t))
         (app (appkit-app-start 'appkit-media-test :id 'owned-open
                                :shutdown #'ignore))
         (buffer (generate-new-buffer " *appkit-media-owned-open*"))
         view
         success
         (cancel-count 0)
         opened)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq view
                  (appkit-attach-view
                   :app app :id 'file-open :mode major-mode)))
          (cl-letf
              (((symbol-function 'appkit-media--cache-file-resource-for-open)
                (lambda (_resource _key _directory callback _errback)
                  (setq success callback)
                  :transfer))
               ((symbol-function 'appkit-media-transfer-p)
                (lambda (object) (eq object :transfer)))
               ((symbol-function 'appkit-media-cancel-transfer)
                (lambda (object)
                  (should (eq object :transfer))
                  (cl-incf cancel-count)))
               ((symbol-function 'appkit-media-open-file)
                (lambda (file) (setq opened file))))
            (appkit-media-open-resource
             '((url . "https://example.invalid/report.pdf")
               (name . "report.pdf")
               (mime-type . "application/pdf"))
             :kind 'file :cache-directory directory :owner view)
            (should (functionp success))
            (should (= 1 (length (appkit-view-handles view))))
            (appkit-kill-view view)
            (should (= 1 cancel-count))
            (funcall success "/tmp/late-report.pdf")
            (should-not opened)))
      (when (appkit-app-live-p app)
        (appkit-app-close app))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))
(provide 'appkit-media-resource-test)

;;; appkit-media-resource-test.el ends here
