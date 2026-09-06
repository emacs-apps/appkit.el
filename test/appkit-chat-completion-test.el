;;; appkit-chat-completion-test.el --- Tests for chat completion -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit-chat-completion)

(ert-deftest appkit-chat-completion-token-bounds-supports-unicode ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "你好 @徐天")
    (should (equal (list :start (- (point) 3)
                         :end (point)
                         :trigger ?@
                         :raw "@徐天"
                         :query "徐天")
                   (appkit-chat-completion-token-bounds ?@)))))

(ert-deftest appkit-chat-completion-token-bounds-rejects-email-address ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "mail@example")
    (should-not (appkit-chat-completion-token-bounds ?@))))

(ert-deftest appkit-chat-completion-token-bounds-keeps-repeated-trigger ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "@@admin")
    (let ((token (appkit-chat-completion-token-bounds ?@)))
      (should (equal "@@admin" (plist-get token :raw)))
      (should (equal "@admin" (plist-get token :query))))))

(ert-deftest appkit-chat-completion-capf-affixes-and-replaces ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "@gre")
    (let* ((candidate
            (appkit-chat-completion-candidate-create
             :label "@GreenKite" :insert "<@1356835185>"
             :prefix "[G] " :annotation " QQ 1356835185"))
           (capf (appkit-chat-completion-capf
                  (- (point) 4) (point) (list candidate) :suffix " "))
           (table (nth 2 capf))
           (affix (plist-get (nthcdr 3 capf) :affixation-function))
           (exit (plist-get (nthcdr 3 capf) :exit-function)))
      (should (equal '("@GreenKite")
                     (all-completions "@g" table)))
      (should (equal '(("@GreenKite" "[G] " " QQ 1356835185"))
                     (funcall affix '("@GreenKite"))))
      (delete-region (- (point) 4) (point))
      (insert "@GreenKite")
      (funcall exit "@GreenKite" 'finished)
      (should (equal "<@1356835185> " (appkit-chatbuf-input-string))))))

(ert-deftest appkit-chat-completion-only-commits-finished-candidate ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "@alice")
    (let* ((candidate (appkit-chat-completion-candidate-create
                       :label "@alice" :insert "<@1>"))
           (capf (appkit-chat-completion-capf
                  (- (point) 6) (point) (list candidate)))
           (exit (plist-get (nthcdr 3 capf) :exit-function)))
      (funcall exit "@alice" 'exact)
      (should (equal "@alice" (appkit-chatbuf-input-string)))
      (funcall exit "@alice" 'finished)
      (should (equal "<@1>" (appkit-chatbuf-input-string))))))

(ert-deftest appkit-chat-completion-default-commit-syncs-canonical-state ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "@old")
    (appkit-chatbuf-input-state-sync)
    (let ((candidate (appkit-chat-completion-candidate-create
                      :label "@new" :insert "@new")))
      (delete-region (- (point) 4) (point))
      (insert "@new")
      (appkit-chat-completion-apply-candidate
       "@new" candidate :suffix " ")
      (should (equal "@new " (appkit-chatbuf-input-string)))
      (should (equal "@new " (appkit-chatbuf-input-state))))))

(ert-deftest appkit-chat-completion-rolls-back-failed-rich-insertion ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "@chosen")
    (let ((candidate (appkit-chat-completion-candidate-create
                      :label "@chosen")))
      (should-error
       (appkit-chat-completion-apply-candidate
        "@chosen" candidate
        :insert-function (lambda (_candidate)
                           (insert "PART")
                           (error "broken insert"))))
      (should (equal "@chosen" (appkit-chatbuf-input-string)))
      (should (equal "@chosen" (appkit-chatbuf-input-state))))))

(ert-deftest appkit-chat-completion-suffix-does-not-duplicate-following-space ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "@old rest")
    (goto-char (+ (appkit-chatbuf-input-start-position) 4))
    (let ((candidate (appkit-chat-completion-candidate-create
                      :label "@new" :insert "<@1>")))
      (delete-region (- (point) 4) (point))
      (insert "@new")
      (appkit-chat-completion-apply-candidate
       "@new" candidate :suffix " ")
      (should (equal "<@1> rest" (appkit-chatbuf-input-string))))))

(ert-deftest appkit-chat-completion-decorations-are-lazy ()
  (let* ((calls 0)
         (candidate
          (appkit-chat-completion-candidate-create
           :label "@user"
           :annotation (lambda (_candidate)
                         (cl-incf calls)
                         " details"))))
    (let ((map (appkit-chat-completion--candidate-map (list candidate))))
      (should (= calls 0))
      (should (equal '(("@user" "" " details"))
                     (appkit-chat-completion-affixation '("@user") map)))
      (should (= calls 1)))))

(ert-deftest appkit-chat-completion-searches-candidate-aliases ()
  (let* ((candidate
          (appkit-chat-completion-candidate-create
           :label "@徐天天"
           :search-terms '("GreenKite" "1356835185")))
         (capf (appkit-chat-completion-capf 1 1 (list candidate)))
         (table (nth 2 capf)))
    (should (equal '("@徐天天") (all-completions "@green" table)))
    (should (equal '("@徐天天") (all-completions "@135683" table)))))

(ert-deftest appkit-chat-completion-alias-matching-is-syntax-independent ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (let* ((candidate
            (appkit-chat-completion-candidate-create
             :label "@徐天天" :search-terms '("GreenKite")))
           (capf (appkit-chat-completion-capf 1 1 (list candidate)))
           (table (nth 2 capf)))
      (should (equal '("@徐天天") (all-completions "@@green" table))))))

(ert-deftest appkit-chat-completion-alias-respects-case-option ()
  (let* ((appkit-chat-completion-ignore-case nil)
         (candidate
          (appkit-chat-completion-candidate-create
           :label "@user" :search-terms '("GreenKite")))
         (capf (appkit-chat-completion-capf 1 1 (list candidate)))
         (table (nth 2 capf)))
    (should-not (all-completions "@green" table))
    (should (equal '("@user") (all-completions "@Green" table)))))

(ert-deftest appkit-chat-completion-never-deletes-before-input-marker ()
  (with-temp-buffer
    (insert "timeline\n")
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "x")
    (let ((candidate (appkit-chat-completion-candidate-create
                      :label "very-long-label" :insert "bad")))
      (should-not
       (appkit-chat-completion-apply-candidate "very-long-label" candidate))
      (should (string-prefix-p "timeline\n>>> " (buffer-string))))))

(ert-deftest appkit-chat-completion-capf-supports-structured-insertion ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "@徐天天")
    (let* ((candidate
            (appkit-chat-completion-candidate-create
             :label "@徐天天" :value '((user-id . "1356835185"))))
           (capf
            (appkit-chat-completion-capf
             (- (point) 4) (point) (list candidate)
             :insert-function
             (lambda (selected)
               (appkit-chatbuf-input-insert
                "@徐天天"
                :object (appkit-chat-completion-candidate-value selected)))))
           (exit (plist-get (nthcdr 3 capf) :exit-function)))
      (funcall exit "@徐天天" 'finished)
      (goto-char (appkit-chatbuf-input-start-position))
      (should (equal '((user-id . "1356835185"))
                     (appkit-chatbuf-input-object-at-point))))))

(ert-deftest appkit-chat-completion-dispatch-stops-after-handler ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert ":wave:")
    (let (calls)
      (setq-local appkit-chat-completion-functions
                  (list (lambda () (push 'first calls) nil)
                        (lambda () (push 'second calls) t)
                        (lambda () (push 'third calls) t)))
      (should (appkit-chat-completion-complete))
      (should (equal '(second first) calls)))))

(ert-deftest appkit-chat-completion-capf-exposes-candidate-groups ()
  (let* ((candidate
          (appkit-chat-completion-candidate-create
           :label ":wave:"
           :group "Favorites"))
         (table (nth 2 (appkit-chat-completion-capf 1 1 (list candidate))))
         (group-function
          (completion-metadata-get
           (completion-metadata "" table nil)
           'group-function)))
    (should (equal "Favorites" (funcall group-function ":wave:" nil)))
    (should (equal ":wave:" (funcall group-function ":wave:" t)))))

(ert-deftest appkit-chat-completion-group-preserves-existing-slot-layout ()
  (let ((candidate
         (appkit-chat-completion-candidate-create
          :label ":rocket:"
          :search-terms '("rocket")
          :value 'payload
          :group "Unicode · Travel & Places")))
    ;; Existing byte-compiled clients inline these vector offsets.
    (should (equal '("rocket") (aref candidate 5)))
    (should (eq 'payload (aref candidate 6)))
    (should (equal "Unicode · Travel & Places" (aref candidate 7)))))

(ert-deftest appkit-chat-completion-visual-reader-uses-native-alist-table ()
  (let* ((preview '(image :type png :file "face.png"))
         (candidate
          (appkit-chat-completion-candidate-create
           :label "/惊讶 (0)"
           :prefix (concat (propertize " " 'display preview) " ")
           :search-terms '("surprised" "zero")
           :group (lambda (_candidate) "QQ Faces")
           :value 'face-zero))
         seen-title
         seen-default
         seen-group
         seen-prefix)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt table _predicate _require _initial _history
                                default)
                 (setq seen-default default)
                 (let* ((metadata (completion-metadata "" table nil))
                        (group-function
                         (completion-metadata-get metadata 'group-function))
                        (affixation-function
                         (completion-metadata-get metadata
                                                  'affixation-function))
                        (group-title
                         (car (completion-all-completions "" table nil 0))))
                   (should
                    (eq (completion-metadata-get metadata 'category)
                        'appkit-chat))
                   (setq seen-group
                         (funcall group-function group-title nil))
                   (should
                    (equal group-title
                           (funcall group-function group-title t)))
                   (let* ((matches
                           (completion-all-completions
                            "SURPRISED" table nil 9))
                          (title (car matches))
                          (row
                           (car
                            (funcall
                             affixation-function
                             (list (substring-no-properties title))))))
                     (should (equal (cdr matches) 0))
                     (should
                      (equal
                       (get-text-property (1- (length title)) 'display title)
                       ""))
                     (setq seen-prefix (cadr row)
                           seen-title title)
                     (substring-no-properties title))))))
      (should
       (eq (appkit-chat-completion-read-visual
            "Visual: " (list candidate) :default-candidate candidate)
           candidate))
      (should (string-match-p "surprised" seen-title))
      (should (equal (get-text-property 0 'display seen-prefix) preview))
      (should (equal seen-group "QQ Faces"))
      (should
       (equal (substring-no-properties seen-default)
              (substring-no-properties seen-title))))))

(ert-deftest appkit-chat-completion-visual-affixation-evaluates-prefix-function ()
  (let* ((calls 0)
         (preview '(image :type png :file "face.png"))
         (candidate
          (appkit-chat-completion-candidate-create
           :label ":dance:"
           :search-terms '("party")
           :prefix
           (lambda (_candidate)
             (cl-incf calls)
             (concat (propertize " " 'display preview) " "))))
         (title
          (caar
           (appkit-chat-completion--visual-choices (list candidate))))
         (candidate-map (make-hash-table :test #'equal)))
    (puthash title candidate candidate-map)
    (let* ((row
            (car
             (appkit-chat-completion--visual-affixation
              (list (substring-no-properties title)) candidate-map)))
           (prefix (cadr row)))
      (should (= 1 calls))
      (should (equal preview (get-text-property 0 'display prefix))))))

(provide 'appkit-chat-completion-test)

;;; appkit-chat-completion-test.el ends here
