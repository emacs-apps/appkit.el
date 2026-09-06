;;; appkit-position-test.el --- Tests for semantic position preservation -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'appkit-position)

(defun appkit-position-test--insert-entry (key line-count)
  "Insert LINE-COUNT lines carrying stable KEY."
  (let ((start (point)))
    (dotimes (index line-count)
      (insert (format "%s-%d\n" key index)))
    (put-text-property start (point) 'appkit-test-key key)))

(defun appkit-position-test--entry-position (key line-offset column)
  "Return KEY's position at LINE-OFFSET and COLUMN."
  (save-excursion
    (goto-char (appkit-position-find-property-value
                (point-min) (point-max) 'appkit-test-key key))
    (forward-line line-offset)
    (move-to-column column)
    (point)))

(defun appkit-position-test--should-window-location
    (window position-function key line-offset column)
  "Assert WINDOW's POSITION-FUNCTION location in KEY at LINE-OFFSET and COLUMN."
  (let ((position (funcall position-function window)))
    (should (eq key (get-text-property position 'appkit-test-key)))
    (should
     (= line-offset
        (- (line-number-at-pos position)
           (line-number-at-pos
            (appkit-position-find-property-value
             (point-min) (point-max) 'appkit-test-key key)))))
    (should
     (= column
        (save-excursion
          (goto-char position)
          (current-column))))))

(ert-deftest appkit-position-restores-semantic-window-anchor ()
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (appkit-position-test--insert-entry 'a 4)
      (appkit-position-test--insert-entry 'b 3)
      (appkit-position-test--insert-entry 'c 2)
      (appkit-position-test--insert-entry 'tail 50)
      (goto-char (appkit-position-find-property-value
                  (point-min) (point-max) 'appkit-test-key 'c))
      (let ((b-start (appkit-position-find-property-value
                      (point-min) (point-max) 'appkit-test-key 'b)))
        (save-excursion
          (goto-char b-start)
          (forward-line 1)
          (set-window-start (selected-window) (point) 'noforce)))
      (let ((snapshot
             (appkit-position-capture
              :anchor-property 'appkit-test-key
              :preserve-window-start t)))
        (erase-buffer)
        (appkit-position-test--insert-entry 'new-prefix 8)
        (appkit-position-test--insert-entry 'a 4)
        (appkit-position-test--insert-entry 'b 3)
        (appkit-position-test--insert-entry 'c 2)
        (appkit-position-test--insert-entry 'tail 50)
        (appkit-position-restore snapshot)
        (should (eq 'c (get-text-property (point) 'appkit-test-key)))
        (let ((window-start (window-start (selected-window))))
          (should (eq 'b (get-text-property window-start 'appkit-test-key)))
          (should (= 1
                     (- (line-number-at-pos window-start)
                        (line-number-at-pos
                         (appkit-position-find-property-value
                          (point-min) (point-max)
                          'appkit-test-key 'b))))))))))

(ert-deftest appkit-position-window-anchor-follows-key-promotion ()
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (appkit-position-test--insert-entry 'local 2)
      (appkit-position-test--insert-entry 'tail 50)
      (goto-char (point-min))
      (set-window-start (selected-window) (point) 'noforce)
      (let ((snapshot
             (appkit-position-capture
              :anchor-property 'appkit-test-key
              :preserve-window-start t)))
        (erase-buffer)
        (appkit-position-test--insert-entry 'prefix 5)
        (appkit-position-test--insert-entry 'server 2)
        (appkit-position-test--insert-entry 'tail 50)
        (appkit-position-restore snapshot '((local . server)))
        (should (eq 'server
                    (get-text-property
                     (window-start (selected-window))
                     'appkit-test-key)))))))

(ert-deftest appkit-position-restores-each-window-point-and-start ()
  (save-window-excursion
    (with-temp-buffer
      (delete-other-windows)
      (let* ((first-window (selected-window))
             (second-window (split-window first-window nil 'right)))
        (set-window-buffer first-window (current-buffer))
        (set-window-buffer second-window (current-buffer))
        (appkit-position-test--insert-entry 'a 4)
        (appkit-position-test--insert-entry 'b 4)
        (appkit-position-test--insert-entry 'local 4)
        (appkit-position-test--insert-entry 'd 4)
        (appkit-position-test--insert-entry 'tail 60)
        (let ((first-point
               (appkit-position-test--entry-position 'local 2 3))
              (first-start
               (appkit-position-test--entry-position 'b 1 2))
              (second-point
               (appkit-position-test--entry-position 'd 2 2))
              (second-start
               (appkit-position-test--entry-position 'a 1 1)))
          (select-window first-window)
          (goto-char first-point)
          (set-window-start first-window first-start 'noforce)
          (set-window-point second-window second-point)
          (set-window-start second-window second-start 'noforce)
          (let ((snapshot
                 (appkit-position-capture
                  :anchor-property 'appkit-test-key
                  :preserve-window-start t)))
            (erase-buffer)
            (appkit-position-test--insert-entry 'new-prefix 6)
            (appkit-position-test--insert-entry 'a 4)
            (appkit-position-test--insert-entry 'b 4)
            ;; Reorder D before the promoted optimistic entry.
            (appkit-position-test--insert-entry 'd 4)
            (appkit-position-test--insert-entry 'server 4)
            (appkit-position-test--insert-entry 'tail 60)
            (appkit-position-restore snapshot '((local . server)))
            (appkit-position-test--should-window-location
             first-window #'window-point 'server 2 3)
            (appkit-position-test--should-window-location
             first-window #'window-start 'b 1 2)
            (appkit-position-test--should-window-location
             second-window #'window-point 'd 2 2)
            (appkit-position-test--should-window-location
             second-window #'window-start 'a 1 1)))))))

(ert-deftest appkit-position-legacy-window-start-keeps-long-line-offset ()
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (setq-local truncate-lines nil)
      (let ((long-start (point)))
        (insert (make-string 240 ?x) "\n")
        (put-text-property
         long-start (point) 'appkit-test-key 'long-entry))
      (appkit-position-test--insert-entry 'tail 60)
      (let* ((entry-start
              (appkit-position-find-property-value
               (point-min) (point-max) 'appkit-test-key 'long-entry))
             (start-offset 100)
             (point-offset 150))
        (goto-char (+ entry-start point-offset))
        (set-window-start
         (selected-window) (+ entry-start start-offset) 'noforce)
        (should (= (+ entry-start start-offset)
                   (window-start (selected-window))))
        (let ((snapshot
               (appkit-position-capture
                :anchor-property 'appkit-test-key
                :preserve-window-start t)))
          ;; Exercise the compatibility fields used by pre-multi-window
          ;; snapshots rather than the new per-window collection.
          (setf (appkit-position-snapshot-window-snapshots snapshot) nil)
          (erase-buffer)
          (appkit-position-test--insert-entry 'new-prefix 5)
          (let ((new-long-start (point)))
            (insert (make-string 240 ?x) "\n")
            (put-text-property
             new-long-start (point) 'appkit-test-key 'long-entry))
          (appkit-position-test--insert-entry 'tail 60)
          (appkit-position-restore snapshot)
          (let* ((new-entry-start
                  (appkit-position-find-property-value
                   (point-min) (point-max)
                   'appkit-test-key 'long-entry))
                 (restored-start (window-start (selected-window))))
            (should (eq 'long-entry
                        (get-text-property
                         restored-start 'appkit-test-key)))
            (should (= start-offset (- restored-start new-entry-start)))
            (should (= start-offset
                       (save-excursion
                         (goto-char restored-start)
                         (current-column))))))))))

(ert-deftest appkit-position-window-locations-use-absolute-fallbacks ()
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (appkit-position-test--insert-entry 'old-start 3)
      (appkit-position-test--insert-entry 'old-point 3)
      (appkit-position-test--insert-entry 'tail 60)
      (goto-char (appkit-position-test--entry-position 'old-point 1 2))
      (set-window-start
       (selected-window)
       (appkit-position-test--entry-position 'old-start 1 1)
       'noforce)
      (let ((point-line (line-number-at-pos (window-point)))
            (start-line (line-number-at-pos (window-start)))
            (snapshot
             (appkit-position-capture
              :anchor-property 'appkit-test-key
              :preserve-window-start t)))
        (erase-buffer)
        (appkit-position-test--insert-entry 'replacement 20)
        (appkit-position-test--insert-entry 'tail 60)
        (appkit-position-restore snapshot)
        (should (= point-line (line-number-at-pos (window-point))))
        (should (= 2
                   (save-excursion
                     (goto-char (window-point))
                     (current-column))))
        (should (= start-line (line-number-at-pos (window-start))))
        (should (= 1
                   (save-excursion
                     (goto-char (window-start))
                     (current-column))))))))

(ert-deftest appkit-position-restores-point-and-window-point-after-final-row ()
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (appkit-position-test--insert-entry 'entry 2)
      (goto-char (point-max))
      (let ((snapshot
             (appkit-position-capture
              :anchor-property 'appkit-test-key
              :preserve-window-start t)))
        (erase-buffer)
        (appkit-position-test--insert-entry 'prefix 3)
        (appkit-position-test--insert-entry 'entry 4)
        (goto-char (point-min))
        (appkit-position-restore snapshot)
        (should (= (point) (point-max)))
        (should (= (window-point (selected-window)) (point-max)))))))

(ert-deftest appkit-position-restores-promoted-key-after-run-boundary ()
  (with-temp-buffer
    (appkit-position-test--insert-entry 'local 2)
    (let ((boundary (point)))
      (insert "unkeyed suffix")
      (goto-char boundary))
    (let ((snapshot
           (appkit-position-capture :anchor-property 'appkit-test-key)))
      (erase-buffer)
      (appkit-position-test--insert-entry 'prefix 3)
      (appkit-position-test--insert-entry 'server 4)
      (let ((expected (point)))
        (insert "replacement suffix")
        (goto-char (point-min))
        (appkit-position-restore snapshot '((local . server)))
        (should (= (point) expected))
        (should (eq 'server
                    (get-text-property (1- (point)) 'appkit-test-key)))
        (should-not (get-text-property (point) 'appkit-test-key))))))

(ert-deftest appkit-position-equal-string-property-segments-form-one-run ()
  (with-temp-buffer
    (insert "abcdef\n")
    (put-text-property 1 4 'appkit-test-key (copy-sequence "opaque-id"))
    (put-text-property 4 (point-max)
                       'appkit-test-key (copy-sequence "opaque-id"))
    (goto-char 6)
    (let ((snapshot
           (appkit-position-capture :anchor-property 'appkit-test-key)))
      (erase-buffer)
      (insert "abcdef\n")
      (put-text-property 1 4 'appkit-test-key (copy-sequence "opaque-id"))
      (put-text-property 4 (point-max)
                         'appkit-test-key (copy-sequence "opaque-id"))
      (goto-char (point-min))
      (appkit-position-restore snapshot)
      (should (= (point) 6)))))

(provide 'appkit-position-test)

;;; appkit-position-test.el ends here
