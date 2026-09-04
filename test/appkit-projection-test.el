;;; appkit-projection-test.el --- Renderer projection tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'appkit-projection)
(require 'appkit-test-helper)

(defun appkit-projection-test--printer (prints)
  "Return a row printer recording render counts in PRINTS."
  (lambda (row)
    (let* ((key (appkit-projection-row-key row))
           (payload (appkit-projection-row-payload row))
           (context (appkit-projection-row-context row))
           (start (point)))
      (puthash key (1+ (gethash key prints 0)) prints)
      (insert (format "%s:%s:%s\n"
                      key payload (or (plist-get context :layout) "plain")))
      (add-text-properties start (point) (list 'test-projection-key key)))))

(defun appkit-projection-test--row
    (key payload &optional context dependencies)
  "Create one test row from KEY, PAYLOAD, CONTEXT, and DEPENDENCIES."
  (appkit-projection-row-create
   :key key :payload payload :context context :dependencies dependencies))

(cl-defun appkit-projection-test--create
    (surface prints &key header footer no-separator-p)
  "Create a Renderer projection for SURFACE recording into PRINTS."
  (appkit-with-content-update surface
    (erase-buffer)
    (appkit-projection-create
     (appkit-projection-test--printer prints)
     'test-projection-key
     :header header :footer footer :no-separator-p no-separator-p)))

(ert-deftest appkit-projection-projects-context-and-dependencies ()
  (let ((rows
         (appkit-projection-project
          '((a . "one") (b . "two"))
          #'car
          :context-function
          (lambda (previous current)
            (list :previous (car-safe previous) :current (car current)))
          :dependencies-function
          (lambda (entry) (list (list :source (cdr entry)))))))
    (should (equal '(a b) (mapcar #'appkit-projection-row-key rows)))
    (should (equal '(:previous a :current b)
                   (appkit-projection-row-context (cadr rows))))
    (should (equal '((:source "two"))
                   (appkit-projection-row-dependencies (cadr rows))))))

(ert-deftest appkit-projection-sync-is-keyed-and-context-sensitive ()
  (appkit-test-with-surface
    (let* ((surface (appkit-current-surface))
           (prints (make-hash-table :test #'equal))
           (projection (appkit-projection-test--create surface prints)))
      (appkit-projection-sync
       surface projection
       (list (appkit-projection-test--row 'a "A")
             (appkit-projection-test--row 'b "B")))
      (let ((a-node (appkit-projection-node projection 'a))
            (b-node (appkit-projection-node projection 'b)))
        (appkit-projection-sync
         surface projection
         (list (appkit-projection-test--row 'a "A")
               (appkit-projection-test--row 'b "B" '(:layout compact))))
        (should (eq a-node (appkit-projection-node projection 'a)))
        (should (eq b-node (appkit-projection-node projection 'b)))
        (should (= 1 (gethash 'a prints)))
        (should (= 2 (gethash 'b prints)))
        (should (equal '(a b) (appkit-projection-keys projection)))))))

(ert-deftest appkit-projection-redraws-changed-dependency-rows ()
  (appkit-test-with-surface
    (let* ((surface (appkit-current-surface))
           (prints (make-hash-table :test #'equal))
           (resource '(:image "avatar"))
           (projection (appkit-projection-test--create surface prints))
           (rows
            (list (appkit-projection-test--row 'a "A")
                  (appkit-projection-test--row 'b "B" nil (list resource)))))
      (appkit-projection-sync surface projection rows)
      (let ((a-node (appkit-projection-node projection 'a))
            (b-node (appkit-projection-node projection 'b)))
        (appkit-projection-sync
         surface projection rows :changed-dependencies (list resource))
        (should (eq a-node (appkit-projection-node projection 'a)))
        (should (eq b-node (appkit-projection-node projection 'b)))
        (should (= 1 (gethash 'a prints)))
        (should (= 2 (gethash 'b prints)))
        (should (equal '(b)
                       (appkit-projection-dependent-keys
                        projection (list resource))))))))

(ert-deftest appkit-projection-updates-frame-and-moves-to-first-row ()
  (appkit-test-with-surface
    (let* ((surface (appkit-current-surface))
           (prints (make-hash-table :test #'equal))
           (projection
            (appkit-projection-test--create
             surface prints :header "Loading\n" :no-separator-p t)))
      (appkit-projection-sync
       surface projection (list (appkit-projection-test--row 'a "A"))
       :header "Ready\n" :position 'first)
      (should (eq 'a (get-text-property (point) 'test-projection-key)))
      (should (string-prefix-p "Ready\n" (buffer-string)))
      (appkit-projection-sync
       surface projection nil :header "Settled\n" :reconcile-p nil)
      (should (= 1 (gethash 'a prints)))
      (should (equal '(a) (appkit-projection-keys projection)))
      (should (string-prefix-p "Settled\n" (buffer-string))))))

(ert-deftest appkit-projection-invalid-printer-keeps-buffer-content ()
  (appkit-test-with-surface
    (let ((inhibit-read-only t))
      (insert "retained")
      (should-error (appkit-projection-create nil 'test-projection-key))
      (should (equal "retained" (buffer-string))))))

(ert-deftest appkit-projection-preserves-each-window-position ()
  (save-window-excursion
    (appkit-test-with-surface
      (delete-other-windows)
      (let* ((surface (appkit-current-surface))
             (buffer (current-buffer))
             (prints (make-hash-table :test #'equal))
             (projection (appkit-projection-test--create surface prints))
             (first-window (selected-window))
             (second-window (split-window first-window nil 'right)))
        (set-window-buffer first-window buffer)
        (set-window-buffer second-window buffer)
        (appkit-projection-sync
         surface projection
         (mapcar (lambda (key) (appkit-projection-test--row key key))
                 '(a b c d)))
        (let ((b-position (appkit-position-find-property-value
                           (point-min) (point-max) 'test-projection-key 'b))
              (c-position (appkit-position-find-property-value
                           (point-min) (point-max) 'test-projection-key 'c))
              (d-position (appkit-position-find-property-value
                           (point-min) (point-max) 'test-projection-key 'd)))
          (goto-char c-position)
          (set-window-start first-window b-position 'noforce)
          (set-window-point second-window d-position)
          (set-window-start second-window c-position 'noforce))
        (appkit-projection-sync
         surface projection
         (mapcar (lambda (key) (appkit-projection-test--row key key))
                 '(prefix a b c d)))
        (should (eq 'c (get-text-property
                        (window-point first-window) 'test-projection-key)))
        (should (eq 'b (get-text-property
                        (window-start first-window) 'test-projection-key)))
        (should (eq 'd (get-text-property
                        (window-point second-window) 'test-projection-key)))
        (should (eq 'c (get-text-property
                        (window-start second-window) 'test-projection-key)))))))

(ert-deftest appkit-projection-rejects-duplicate-row-keys ()
  (appkit-test-with-surface
    (let* ((surface (appkit-current-surface))
           (prints (make-hash-table :test #'equal))
           (projection (appkit-projection-test--create surface prints)))
      (should-error
       (appkit-projection-sync
        surface projection
        (list (appkit-projection-test--row 'same "A")
              (appkit-projection-test--row 'same "B")))))))

(ert-deftest appkit-projection-change-merge-preserves-non-source-work ()
  (let ((merged
         (appkit-projection-change-merge
          (appkit-projection-change-create
           :keys '(a) :resources '(avatar)
           :position 'first)
          (appkit-projection-change-create
           :full-p t :keys '(b) :resources '(cover)
           :frame-p t :position 'preserve))))
    (should (appkit-projection-change-full-p merged))
    (should-not (appkit-projection-change-keys merged))
    (should (equal '(avatar cover)
                   (appkit-projection-change-resources merged)))
    (should (appkit-projection-change-frame-p merged))
    (should (eq 'first (appkit-projection-change-position merged)))))

(ert-deftest appkit-projection-renderer-resource-redraw-skips-project-all ()
  (appkit-test-with-surface
    (let ((surface (appkit-current-surface))
          (projects 0)
          (prints 0)
          renderer)
      (setq renderer
            (appkit-projection-renderer-create
             :project-all
             (lambda (_surface _app-read-view model)
               (setq projects (1+ projects))
               (list
                (appkit-projection-row-create
                 :key 'row :payload model :dependencies '(avatar))))
             :printer
             (lambda (_surface _app-read-view row)
               (setq prints (1+ prints))
               (insert (format "%s\n" (appkit-projection-row-payload row))))
             :anchor-property 'test-projection-key
             :no-separator-p t))
      (funcall (appkit-generated-renderer-mount renderer)
               surface 'read-view 'initial)
      (funcall (appkit-generated-renderer-render renderer)
               surface 'read-view 'initial
               (appkit-projection-change-create :full-p t))
      (should (= 1 projects))
      (should (= 1 prints))
      (funcall (appkit-generated-renderer-render renderer)
               surface 'read-view 'initial
               (appkit-projection-change-create :resources '(avatar)))
      (should (= 1 projects))
      (should (= 2 prints))
      (funcall (appkit-generated-renderer-unmount renderer) surface))))

(provide 'appkit-projection-test)

;;; appkit-projection-test.el ends here
