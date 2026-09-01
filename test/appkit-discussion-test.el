;;; appkit-discussion-test.el --- Tests for discussion rows -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-discussion)

(ert-deftest appkit-discussion-entry-owns-thread-layout-and-properties ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'appkit-chat-avatar-prefixes)
               (lambda (&rest _arguments)
                 '(:header "H " :first-body "B " :rest-body "R "))))
      (appkit-discussion-insert-entry
       (appkit-discussion-entry-create
        :key "reply-2"
        :parent-key "comment-1"
        :depth 1
        :heading "Author replies"
        :time "12:34"
        :body-inserter
        (lambda (prefix properties)
          (appkit-ui-insert-prefixed-lines
           prefix "first\nsecond" :properties properties))
        :footer "3/5 replies"
        :properties '(client-entry "reply-2"
                      rear-nonsticky (client-entry)))
       :width 50
       :indent-width 3))
    (should (string-match-p "Author replies" (buffer-string)))
    (should (string-match-p "first\nsecond\n3/5 replies" (buffer-string)))
    (goto-char (point-min))
    (search-forward "12:34")
    (should (equal (get-text-property (1- (match-beginning 0)) 'display)
                   '(space :align-to (- right (5 . width)))))
    (goto-char (point-min))
    (should (equal "reply-2" (appkit-discussion-key-at-point)))
    (should (equal "comment-1"
                   (get-text-property
                    (point) appkit-discussion-parent-key-property)))
    (should (= 1 (get-text-property
                  (point) appkit-discussion-depth-property)))
    (should (equal "reply-2" (get-text-property (point) 'client-entry)))
    (should
     (equal
      (sort (copy-sequence (get-text-property (point) 'rear-nonsticky))
            (lambda (left right)
              (string-lessp (symbol-name left) (symbol-name right))))
      (sort (list 'client-entry
                  appkit-discussion-key-property
                  appkit-discussion-parent-key-property
                  appkit-discussion-depth-property)
            (lambda (left right)
              (string-lessp (symbol-name left) (symbol-name right))))))
    (should (string-prefix-p
             "   H " (get-text-property (point) 'line-prefix)))
    (forward-line 1)
    (should (string-prefix-p
             "   B " (get-text-property (point) 'line-prefix)))
    (forward-line 1)
    (should (string-prefix-p
             "   R " (get-text-property (point) 'line-prefix)))))

(ert-deftest appkit-discussion-context-precedes-avatar-heading ()
  "Context should occupy its own prefixed line above the avatar heading."
  (with-temp-buffer
    (let ((appkit-discussion-connector-style 'text))
      (cl-letf (((symbol-function 'appkit-chat-avatar-prefixes)
                 (lambda (&rest _arguments)
                   '(:header "H " :first-body "B " :rest-body "R "))))
        (appkit-discussion-insert-entry
         (appkit-discussion-entry-create
          :key "renote"
          :parent-key "root"
          :depth 1
          :connector 'end
          :context-inserter
          (lambda () (insert (propertize "renoted by Alice" 'user-id "alice")))
          :context-face 'shadow
          :heading "Original author"
          :body-inserter
          (lambda (prefix properties)
            (appkit-ui-insert-prefixed-lines
             prefix "body" :properties properties)))
         :indent-width 3)))
    (should (equal (buffer-string)
                   "renoted by Alice\nOriginal author\nbody\n\n"))
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'shadow))
    (should (equal (get-text-property (point) 'user-id) "alice"))
    (should (equal (get-text-property (point) 'line-prefix) "│    "))
    (forward-line 1)
    (should (equal (get-text-property (point) 'line-prefix) "│    H "))
    (forward-line 1)
    (should (equal (get-text-property (point) 'line-prefix) "│    B "))))

(ert-deftest appkit-discussion-empty-context-inserter-adds-no-line ()
  "An empty context inserter should leave the heading at the first line."
  (with-temp-buffer
    (appkit-discussion-insert-entry
     (appkit-discussion-entry-create
      :key "plain"
      :context-inserter #'ignore
      :heading "Author"
      :body-inserter
      (lambda (prefix properties)
        (appkit-ui-insert-prefixed-lines
         prefix "body" :properties properties)))
     :avatar-p nil
     :separate-p nil)
    (should (equal (buffer-string) "Author\nbody\n"))))

(ert-deftest appkit-discussion-long-heading-elides-before-time ()
  "A long heading must not separate the two avatar rows."
  (with-temp-buffer
    (cl-letf (((symbol-function 'appkit-chat-avatar-prefixes)
               (lambda (&rest _arguments)
                 '(:header "H " :first-body "B " :rest-body "R "))))
      (appkit-discussion-insert-entry
       (appkit-discussion-entry-create
        :key "long"
        :heading
        "Alice Extremely Long Identity renoted Bob Equally Long Identity"
        :time "12:34"
        :body-inserter
        (lambda (prefix properties)
          (appkit-ui-insert-prefixed-lines
           prefix "body" :properties properties)))
       :width 30))
    (goto-char (point-min))
    (search-forward "12:34")
    (should (= 1 (line-number-at-pos)))
    (goto-char (line-beginning-position))
    (should (search-forward "…" (line-end-position) t))
    (should-not (get-text-property (1- (point)) 'display))
    (should-not (search-forward "Equally" (line-end-position) t))
    (forward-line 1)
    (should (equal "body"
                   (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))))
    (should (string-prefix-p
             "B " (get-text-property (point) 'line-prefix)))))

(ert-deftest appkit-discussion-entry-can-omit-avatar-prefix ()
  "An entry without a shared avatar reserves only nesting indentation."
  (with-temp-buffer
    (appkit-discussion-insert-entry
     (appkit-discussion-entry-create
      :key "plain"
      :parent-key "root"
      :depth 1
      :heading "Plain"
      :body-inserter
      (lambda (prefix properties)
        (appkit-ui-insert-prefixed-lines
         prefix "body" :properties properties)))
     :avatar-p nil
     :indent-width 3)
    (goto-char (point-min))
    (should (equal "Plain" (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position))))
    (should (equal "   " (get-text-property (point) 'line-prefix)))
    (forward-line 1)
    (should (equal "body" (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
    (should (equal "   " (get-text-property (point) 'line-prefix)))))

(ert-deftest appkit-discussion-entry-draws-a-chain-connector-in-the-prefix ()
  "A continue connector should occupy the prefix instead of indenting."
  (with-temp-buffer
    (let ((appkit-discussion-connector-style 'text))
      (appkit-discussion-insert-entry
       (appkit-discussion-entry-create
        :key "ancestor"
        :parent-key "root"
        :depth 0
        :connector 'continue
        :heading "Ancestor"
        :body-inserter
        (lambda (prefix properties)
          (appkit-ui-insert-prefixed-lines
           prefix "body" :properties properties)))
       :avatar-p nil))
    (goto-char (point-min))
    (should (string-prefix-p "│ " (get-text-property (point) 'line-prefix)))
    (forward-line 1)
    (should (string-prefix-p "│ " (get-text-property (point) 'line-prefix)))
    (forward-line 1)
    (should (string-prefix-p "│ " (get-text-property (point) 'line-prefix)))))

(ert-deftest appkit-discussion-fringe-connector-reserves-no-text-column ()
  "A graphical fringe connector should not shift discussion content."
  (with-temp-buffer
    (let ((appkit-discussion-connector-style 'fringe))
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda (&optional _display) t))
                ((symbol-function 'window-fringes)
                 (lambda (&optional _window) '(4 0 nil nil))))
        (appkit-discussion-insert-entry
         (appkit-discussion-entry-create
          :key "ancestor"
          :connector 'continue
          :heading "Ancestor"
          :body-inserter
          (lambda (prefix properties)
            (appkit-ui-insert-prefixed-lines
             prefix "body" :properties properties)))
         :avatar-p nil
         :separate-p nil)))
    (let* ((prefix (get-text-property (point-min) 'line-prefix))
           (display (get-text-property 0 'display prefix))
           (spec (car display)))
      (should (= 1 (length prefix)))
      (should (= 0 (appkit-discussion--prefix-width prefix)))
      (should (get-text-property
               0 'appkit-discussion-fringe-marker prefix))
      (should (eq 'left-fringe (car spec)))
      (should (fringe-bitmap-p (nth 1 spec)))
      (should (eq 'appkit-discussion--connector-4-2-outer
                  (nth 1 spec)))
      (should (eq 'appkit-discussion-fringe-connector (nth 2 spec))))))

(ert-deftest appkit-discussion-fringe-connector-allows-custom-geometry ()
  "Generated geometry and a caller-defined bitmap should both be usable."
  (let ((appkit-discussion--fringe-spec-cache
         (make-hash-table :test #'eq))
        (appkit-discussion-fringe-bar-width 3)
        (appkit-discussion-fringe-bar-position 'inner)
        (appkit-discussion-fringe-bitmap nil))
    (let* ((marker (appkit-discussion--fringe-connector 8))
           (spec (car (get-text-property 0 'display marker))))
      (should (eq 'appkit-discussion--connector-8-3-inner
                  (nth 1 spec)))
      (should (eq 'appkit-discussion-fringe-connector (nth 2 spec)))))
  (define-fringe-bitmap
    'appkit-discussion-test-fringe-bitmap
    [#b10101010] 1 8 '(top t))
  (let ((appkit-discussion--fringe-spec-cache
         (make-hash-table :test #'eq))
        (appkit-discussion-fringe-bar-width 0)
        (appkit-discussion-fringe-bar-position 'invalid)
        (appkit-discussion-fringe-bitmap
         'appkit-discussion-test-fringe-bitmap))
    (let* ((marker (appkit-discussion--fringe-connector 8))
           (spec (car (get-text-property 0 'display marker))))
      (should (eq 'appkit-discussion-test-fringe-bitmap
                  (nth 1 spec))))))

(ert-deftest appkit-discussion-fringe-connector-validates-generated-geometry ()
  (let ((appkit-discussion-fringe-bitmap nil)
        (appkit-discussion-fringe-bar-width 0))
    (should-error (appkit-discussion--fringe-connector 8)
                  :type 'error))
  (let ((appkit-discussion-fringe-bitmap nil)
        (appkit-discussion-fringe-bar-width 2)
        (appkit-discussion-fringe-bar-position 'invalid))
    (should-error (appkit-discussion--fringe-connector 8)
                  :type 'error))
  (let ((appkit-discussion-fringe-bitmap
         'appkit-discussion-undefined-fringe-bitmap))
    (should-error (appkit-discussion--fringe-connector 8)
                  :type 'error)))

(ert-deftest appkit-discussion-connector-style-selects-visible-fallbacks ()
  "`fringe', `text', and `none' should have explicit geometry."
  (let ((appkit-discussion-connector-style 'text))
    (should (eq 'text (appkit-discussion--connector-presentation)))
    (should
     (equal "│ "
            (substring-no-properties
             (appkit-discussion--connector-column
              'continue 'header 'text))))
    (should
     (equal "  "
            (appkit-discussion--connector-column 'end 'rest-body 'text))))
  (let ((appkit-discussion-connector-style 'none))
    (should (eq 'none (appkit-discussion--connector-presentation)))
    (should
     (equal ""
            (appkit-discussion--connector-column
             'continue 'header 'none))))
  (let ((appkit-discussion-connector-style 'fringe))
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _display) nil)))
      (should (eq 'text (appkit-discussion--connector-presentation))))))

(ert-deftest appkit-discussion-entry-requires-parent-for-nesting ()
  (with-temp-buffer
    (should-error
     (appkit-discussion-insert-entry
      (appkit-discussion-entry-create :key "orphan" :depth 1))
     :type 'error)))

(ert-deftest appkit-discussion-avatar-adapts-a-plain-callback-to-a-command ()
  (with-temp-buffer
    (let (called)
      (cl-letf (((symbol-function 'appkit-chat-avatar-prefixes)
                 (lambda (&rest _arguments)
                   '(:header "A " :first-body "B " :rest-body "  "))))
        (appkit-discussion-insert-entry
         (appkit-discussion-entry-create
          :key "action"
          :heading "Action"
          :avatar-action (lambda () (setq called t))))
        (let* ((prefix (get-text-property (point-min) 'line-prefix))
               (map (get-text-property 0 'keymap prefix))
               (command (lookup-key map (kbd "RET"))))
          (should (commandp command))
          (call-interactively command)
          (should called))))))

(ert-deftest appkit-discussion-navigation-follows-stable-entry-spans ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'appkit-chat-avatar-prefixes)
               (lambda (&rest _arguments)
                 '(:header "@ " :first-body "  " :rest-body "  "))))
      (dolist (key '("one" "two" "three"))
        (appkit-discussion-insert-entry
         (appkit-discussion-entry-create :key key :heading key))))
    (goto-char (point-min))
    (should (equal "one" (appkit-discussion-key-at-point)))
    (goto-char (appkit-discussion-next-position))
    (should (equal "two" (appkit-discussion-key-at-point)))
    (forward-line 1)
    (should (equal "one"
                   (save-excursion
                     (goto-char (appkit-discussion-previous-position))
                     (appkit-discussion-key-at-point))))
    (forward-line -1)
    (goto-char (appkit-discussion-next-position))
    (should (equal "three" (appkit-discussion-key-at-point)))
    (goto-char (appkit-discussion-previous-position))
    (should (equal "two" (appkit-discussion-key-at-point)))))

(provide 'appkit-discussion-test)

;;; appkit-discussion-test.el ends here
