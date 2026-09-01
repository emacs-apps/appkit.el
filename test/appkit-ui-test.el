;;; appkit-ui-test.el --- Shared presentation tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit-ui)
(require 'mouse)

(defun appkit-ui-test--primary-click (window position)
  "Return a real primary-click event pair in WINDOW at POSITION."
  (let ((posn (list window position '(0 . 0) 0 nil position)))
    (vector (list 'down-mouse-1 posn)
            (list 'mouse-1 posn))))

(ert-deftest appkit-ui-progress-bar-fills-from-ratio-and-glyph ()
  (should (equal (appkit-ui-progress-bar 0 10) "          "))
  (should (equal (appkit-ui-progress-bar 0.5 10) "====>     "))
  (should (equal (appkit-ui-progress-bar 0.5 10 '(?+ . ?>)) "++++>     "))
  (should (equal (length (appkit-ui-progress-bar 1 10)) 10)))

(ert-deftest appkit-ui-add-action-activates-from-ret-and-point ()
  (with-temp-buffer
    (let ((called 0))
      (insert "before ")
      (let ((start (point)))
        (insert "open")
        (appkit-ui-add-action
         start (point)
         (lambda () (setq called (1+ called)))
         :help-echo "Open profile"))
      (insert " after")
      (goto-char (point-min))
      (search-forward "open")
      (goto-char (match-beginning 0))
      (should (functionp (get-text-property (point) appkit-ui-action-property)))
      (should (functionp (appkit-ui-action-at)))
      (should (equal (get-text-property (point) 'help-echo) "Open profile"))
      (should (eq (lookup-key (get-text-property (point) 'keymap) (kbd "RET"))
                  #'appkit-ui-activate))
      (should (appkit-ui-activate-at))
      (should (= called 1))
      (goto-char (point-min))
      (should-not (appkit-ui-activate-at)))))

(ert-deftest appkit-ui-action-row-dispatches-ret-and-exact-mouse-position ()
  (with-temp-buffer
    (let* ((buffer (current-buffer))
           (first-object (list :id 'first))
           (second-object (list :id 'second))
           calls
           (first-action (lambda (object)
                           (push (list 'first object) calls)))
           (second-action (lambda (object)
                            (push (list 'second object) calls)))
           first-button
           second-button)
      (let ((start (point)))
        (insert "first\n")
        (setq first-button
              (appkit-ui-make-action-row
               start (point) first-object first-action)))
      (let ((start (point)))
        (insert "second\n")
        (setq second-button
              (appkit-ui-make-action-row
               start (point) second-object second-action)))
      (should first-button)
      (should second-button)
      (should (eq #'push-button
                  (lookup-key appkit-ui-action-row-map (kbd "RET"))))
      (should (eq #'push-button
                  (lookup-key appkit-ui-action-row-map [mouse-1])))
      (should-not (keymap-parent appkit-ui-action-row-map))
      (should (eq first-object
                  (button-get first-button 'appkit-ui-action-row-object)))
      (should (eq first-action
                  (button-get first-button 'appkit-ui-action-row-action)))
      (should (eq second-object
                  (button-get second-button 'appkit-ui-action-row-object)))
      (should (eq second-action
                  (button-get second-button 'appkit-ui-action-row-action)))
      (save-window-excursion
        (switch-to-buffer buffer)
        (goto-char first-button)
        (execute-kbd-macro (kbd "RET"))
        (should (equal (pop calls) (list 'first first-object)))
        ;; Point deliberately remains on the first object.  Mouse activation
        ;; must use the exact event position on the adjacent second object.
        (goto-char first-button)
        (let ((mouse-1-click-follows-link 450))
          (execute-kbd-macro
           (appkit-ui-test--primary-click (selected-window) second-button)))
        (should (= (point) first-button)))
      (should (equal (pop calls) (list 'second second-object)))
      (should-not calls))))

(ert-deftest appkit-ui-action-row-excludes-eol-and-skips-empty-lines ()
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (called 0)
          button
          newline-position)
      (let ((start (point)))
        (insert "body\n")
        (setq newline-position (1- (point)))
        (setq button
              (appkit-ui-make-action-row
               start (point) 'body (lambda (_object) (setq called (1+ called))))))
      (should button)
      (should-not (button-at newline-position))
      (save-window-excursion
        (switch-to-buffer buffer)
        (goto-char button)
        (let ((mouse-1-click-follows-link 450))
          (execute-kbd-macro
           (appkit-ui-test--primary-click (selected-window) newline-position))))
      (should (= called 0))
      (let ((empty-start (point)))
        (insert "\n")
        (should-not
         (appkit-ui-make-action-row
          empty-start (point) 'empty
          (lambda (_object) (setq called (1+ called)))))
        (should-not (button-at empty-start)))
      (should (= called 0)))))

(ert-deftest appkit-ui-action-row-hover-is-explicit-and-never-a-follow-link ()
  (with-temp-buffer
    (let (plain-button highlighted-button)
      (let ((start (point)))
        (insert "plain\n")
        (setq plain-button
              (appkit-ui-make-action-row
               start (point) 'plain #'ignore :help-echo "Open plain")))
      (let ((start (point)))
        (insert (propertize "highlighted" 'face 'bold) "\n")
        (setq highlighted-button
              (appkit-ui-make-action-row
               start (point) 'highlighted #'ignore
               :help-echo "Open highlighted"
               :mouse-face 'highlight)))
      (should (equal "Open plain" (button-get plain-button 'help-echo)))
      (should-not (button-get plain-button 'mouse-face))
      (should-not (button-type-get 'appkit-ui-action-row-button 'face))
      (should (eq 'bold (button-get highlighted-button 'face)))
      (should (eq 'highlight (button-get highlighted-button 'mouse-face)))
      (should-not (button-get highlighted-button 'follow-link))
      (should-not (lookup-key (button-get highlighted-button 'keymap)
                              [follow-link]))
      (should-not (mouse-on-link-p highlighted-button)))))

(ert-deftest appkit-ui-action-row-rejects-invalid-action-and-empty-span ()
  (with-temp-buffer
    (insert "invalid\n")
    (should-not
     (appkit-ui-make-action-row (point-min) (point) 'object 'not-a-function))
    (should-not (button-at (point-min)))
    (goto-char (point-max))
    (should-not
     (appkit-ui-make-action-row (point) (point) 'object #'ignore))
    (should-not (button-at (point)))))

(ert-deftest appkit-ui-action-row-rejects-multiline-span ()
  (with-temp-buffer
    (let ((called nil)
          (start (point)))
      (insert "first\nsecond\n")
      (should-not
       (appkit-ui-make-action-row
        start (point) 'object (lambda (_object) (setq called t))))
      (should-not (button-at start))
      (should-not (button-at (line-beginning-position 0)))
      (should-not called))))

(ert-deftest appkit-ui-source-line-prefix-preserves-source-and-columns ()
  (with-temp-buffer
    (setq-local filter-buffer-substring-function
                #'appkit-ui-buffer-substring-filter)
    (insert ">> quoted\n")
    (put-text-property
     (point-min) (point-max) 'line-prefix "outer ")
    (put-text-property
     (point-min) (point-max) 'wrap-prefix "outer ")
    (appkit-ui-apply-source-line-prefix
     (point-min) (point-max) (point-min) (+ (point-min) 3) "││ ")
    (should (equal ">> quoted\n"
                   (buffer-substring-no-properties (point-min) (point-max))))
    (let ((copied (filter-buffer-substring (point-min) (point-max))))
      (should (equal ">> quoted\n" (substring-no-properties copied)))
      (should-not
       (text-property-not-all 0 (length copied) 'display nil copied)))
    (should (equal "│" (get-text-property (point-min) 'display)))
    (should (get-text-property (point-min) 'appkit-ui-source-line-marker))
    ;; Source characters retain one visual column each.  A zero-width hidden
    ;; marker would leave all these positions at column zero and lets modal
    ;; left-motion appear to cross into the preceding line.
    (dotimes (offset 4)
      (goto-char (+ (point-min) offset))
      (should (= offset (current-column))))
    (should (equal "outer "
                   (get-text-property (+ (point-min) 3) 'line-prefix)))
    (should (equal "outer ││ "
                   (get-text-property (+ (point-min) 3) 'wrap-prefix))))
  ;; Graphical bars are represented by a character carrying an image display
  ;; property.  The source character must receive the image itself, not a
  ;; replacement string containing a nested display property (which Emacs does
  ;; not render recursively).
  (with-temp-buffer
    (insert ">")
    (let* ((image '(image :type svg :data "fake"))
           (presentation (propertize " " 'display image)))
      (appkit-ui-apply-source-line-prefix
       (point-min) (point-max) (point-min) (point-max) presentation)
      (should (equal image (get-text-property (point-min) 'display))))))

(ert-deftest appkit-ui-prefix-state-consumes-first-prefix-once ()
  (let ((state (appkit-ui-make-prefix-state "first> " "rest> ")))
    (should (equal "raw> "
                   (appkit-ui-prefix-string "raw> " nil "fallback> ")))
    (should (equal "first> "
                   (appkit-ui-prefix-string state t "fallback> ")))
    (should (equal "rest> "
                   (appkit-ui-prefix-string state nil "fallback> ")))))

(ert-deftest appkit-ui-prefixed-lines-apply-first-and-rest-prefixes ()
  (with-temp-buffer
    (let ((state (appkit-ui-make-prefix-state "A " "B ")))
      (appkit-ui-insert-prefixed-lines state "first\nsecond")
      (goto-char (point-min))
      (should (equal "A " (get-text-property (point) 'line-prefix)))
      (forward-line 1)
      (should (equal "B " (get-text-property (point) 'line-prefix))))))

(ert-deftest appkit-ui-one-line-preview-normalizes-and-styles-text ()
  (let* ((preview
          (appkit-ui-one-line-preview-create
           :text "hello"
           :label "Alice"
           :separator ":"
           :label-face 'success))
         (rendered
          (appkit-ui-render-one-line-preview preview 20 :face 'shadow)))
    (should (equal "Alice: hello" (substring-no-properties rendered)))
    (should (memq 'shadow
                  (ensure-list (get-text-property 0 'face rendered))))
    (should (memq 'success
                  (ensure-list (get-text-property 0 'face rendered))))
    (should-not
     (memq 'success
           (ensure-list (get-text-property 7 'face rendered))))))

(ert-deftest appkit-ui-one-line-preview-reserves-graphical-visual-columns ()
  (let* ((image '(image :type png :data "bytes"))
         (visual (propertize "fallback" 'display image))
         (preview
          (appkit-ui-one-line-preview-create
           :text "hello"
           :visual visual
           :visual-columns 2))
         elide-width)
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'display-images-p) (lambda (&rest _) t)))
      (let ((rendered
             (appkit-ui-render-one-line-preview
              preview 6
              :elide-function
              (lambda (text width _face)
                (setq elide-width width)
                (substring text 0 (min width (length text)))))))
        (should (= 3 elide-width))
        (should (eq image (get-text-property 0 'display rendered)))
        (should (equal "fallback hel"
                       (substring-no-properties rendered)))))))

(ert-deftest appkit-ui-one-line-preview-composes-label-chrome-and-image ()
  (let* ((image '(image :type png :data "bytes"))
         (visual (propertize "fallback" 'display image))
         (preview
          (appkit-ui-one-line-preview-create
           :text "hello"
           :label "Alice"
           :separator ":"
           :visual visual
           :visual-columns 2
           :label-face 'success)))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'display-images-p) (lambda (&rest _) t)))
      (let* ((rendered
              (appkit-ui-render-one-line-preview
               preview 24 :face 'shadow))
             (image-position
              (text-property-any
               0 (length rendered) 'display image rendered)))
        (should (equal "Alice: fallback hello"
                       (substring-no-properties rendered)))
        (should (= 7 image-position))
        (should
         (eq 'success
             (car (ensure-list
                   (get-text-property 0 'face rendered)))))
        (should-not
         (memq 'success
               (ensure-list
                (get-text-property 5 'face rendered))))
        (should-not
         (memq 'success
               (ensure-list
                (get-text-property
                 (1- (length rendered)) 'face rendered))))))))

(ert-deftest appkit-ui-one-line-preview-bounds-terminal-fallback ()
  (let* ((visual
          (propertize "[image]" 'display '(image :type png :data "bytes")))
         (preview
          (appkit-ui-one-line-preview-create
           :text "caption"
           :visual visual
           :visual-columns 2)))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
      (let ((rendered (appkit-ui-render-one-line-preview preview 11)))
        (should (<= (string-width rendered) 11))
        (should (string-prefix-p "[image] " rendered))))))

(ert-deftest appkit-ui-one-line-preview-drops-oversized-visual-for-text ()
  (let* ((visual
          (propertize "[image]" 'display '(image :type png :data "bytes")))
         (preview
          (appkit-ui-one-line-preview-create
           :text "caption"
           :visual visual
           :visual-columns 9)))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'display-images-p) (lambda (&rest _) t)))
      (should
       (equal "cap…"
              (substring-no-properties
               (appkit-ui-render-one-line-preview preview 4)))))))

(ert-deftest appkit-ui-one-line-preview-requires-width-for-display-visual ()
  (let ((preview
         (appkit-ui-one-line-preview-create
          :visual (propertize "[image]" 'display '(image :type png)))))
    (should-error
     (appkit-ui-render-one-line-preview preview 20))))

(provide 'appkit-ui-test)

;;; appkit-ui-test.el ends here
