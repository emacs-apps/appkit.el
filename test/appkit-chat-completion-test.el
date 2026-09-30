;;; appkit-chat-completion-test.el --- Tests for chat completion -*- lexical-binding: t; -*-

(require 'ert)
(require 'icomplete)
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
             :label "@GreenKite"
             :insert "<@1356835185>"
             :prefix "[G] "
             :annotation " QQ 1356835185"))
           (capf (appkit-chat-completion-capf
                  (- (point) 4) (point) (list candidate)
                  :suffix " "))
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
                       :label "@alice"
                       :insert "<@1>"))
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
                      :label "@new"
                      :insert "@new")))
      (delete-region (- (point) 4) (point))
      (insert "@new")
      (appkit-chat-completion-apply-candidate
       "@new" candidate
       :suffix " ")
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
                      :label "@new"
                      :insert "<@1>")))
      (delete-region (- (point) 4) (point))
      (insert "@new")
      (appkit-chat-completion-apply-candidate
       "@new" candidate
       :suffix " ")
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
             :label "@徐天天"
             :search-terms '("GreenKite")))
           (capf (appkit-chat-completion-capf 1 1 (list candidate)))
           (table (nth 2 capf)))
      (should (equal '("@徐天天") (all-completions "@@green" table))))))

(ert-deftest appkit-chat-completion-alias-respects-case-option ()
  (let* ((appkit-chat-completion-ignore-case nil)
         (candidate
          (appkit-chat-completion-candidate-create
           :label "@user"
           :search-terms '("GreenKite")))
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
                      :label "very-long-label"
                      :insert "bad")))
      (should-not
       (appkit-chat-completion-apply-candidate "very-long-label" candidate))
      (should (string-prefix-p "timeline\n>>> " (buffer-string))))))

(ert-deftest appkit-chat-completion-capf-supports-structured-insertion ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "@徐天天")
    (let* ((candidate
            (appkit-chat-completion-candidate-create
             :label "@徐天天"
             :value '((user-id . "1356835185"))))
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

(ert-deftest appkit-chat-completion-visual-reader-matches-aliases-and-default ()
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
                     (should
                      (equal
                       (get-text-property (1- (length title)) 'display title)
                       ""))
                     (setq seen-prefix (cadr row)
                           seen-title title)
                     (substring-no-properties title))))))
      (should
       (eq (appkit-chat-completion-read-visual
            "Visual: " (list candidate)
            :default-candidate candidate)
           candidate))
      (should (string-match-p "surprised" seen-title))
      (should (equal (get-text-property 0 'display seen-prefix) preview))
      (should (equal seen-group "QQ Faces"))
      (should
       (equal (substring-no-properties seen-default)
              (substring-no-properties seen-title))))))

(defmacro appkit-chat-completion-test--with-minibuffer (interaction &rest body)
  "Run BODY with a temporary reader whose INTERACTION receives its table."
  (declare (indent 1) (debug (form body)))
  `(cl-letf (((symbol-function 'completing-read)
              (lambda (_prompt table predicate _require initial _history default)
                (with-temp-buffer
                  (setq-local minibuffer-completion-table table
                              minibuffer-completion-predicate predicate
                              minibuffer-default default)
                  (when initial (insert initial))
                  (run-hooks 'minibuffer-setup-hook)
                  (funcall ,interaction table)))))
     ,@body))

(ert-deftest appkit-chat-completion-visual-reader-replaces-live-catalog ()
  (let* ((old (appkit-chat-completion-candidate-create
               :label "cat" :search-terms '("feline") :prefix "old "))
         (new (appkit-chat-completion-candidate-create
               :label "cat" :search-terms '("feline") :prefix "new "))
         (dog (appkit-chat-completion-candidate-create :label "dog"))
         publish loading-overlay
         (detached 0))
    (appkit-chat-completion-test--with-minibuffer
        (lambda (table)
          (should-not (all-completions "" table))
          (setq loading-overlay
                (seq-find
                 (lambda (overlay)
                   (string-match-p
                    "Loading faces" (or (overlay-get overlay 'before-string) "")))
                 (append (car (overlay-lists)) (cdr (overlay-lists)))))
          (should loading-overlay)
          (should-not (all-completions "Loading" table))
          (insert "feli")
          (goto-char (+ (point-min) 2))
          (let ((input (buffer-string))
                (position (point)))
            (funcall publish (list old dog))
            (should (equal input (buffer-string)))
            (should (= position (point)))
            (should-not (overlay-get loading-overlay 'before-string))
            (let* ((metadata (completion-metadata "" table nil))
                   (affix (completion-metadata-get metadata
                                                   'affixation-function))
                   (title (car (completion-all-completions "FELI" table nil 4))))
              (should (string-prefix-p "cat" title))
              (should (equal "old " (cadar (funcall affix (list title)))))
              (funcall publish (list new) "Ready")
              (should-not (all-completions "dog" table))
              (should-not (all-completions "Ready" table))
              ;; Metadata already handed to a frontend must see the new object.
              (should (equal "new " (cadar (funcall affix (list title)))))
              (should (equal input (buffer-string)))
              (should (= position (point)))
              (substring-no-properties title))))
      (should
       (eq new
           (appkit-chat-completion-read-visual
            "Face: " nil
            :subscribe
            (lambda (callback)
              (setq publish callback)
              (funcall callback nil "Loading faces")
              (lambda () (cl-incf detached)))))))
    (should (= detached 1))
    (should-not (overlay-buffer loading-overlay))))

(ert-deftest appkit-chat-completion-visual-reader-refreshes-lazy-images ()
  (let* ((calls 0)
         (image nil)
         (candidate
          (appkit-chat-completion-candidate-create
           :label ":dance:"
           :prefix (lambda (_candidate)
                     (cl-incf calls)
                     (if image (propertize " " 'display image) ""))))
         (candidates (list candidate))
         publish)
    (appkit-chat-completion-test--with-minibuffer
        (lambda (table)
          (let* ((title (car (all-completions "" table)))
                 (affix
                  (completion-metadata-get
                   (completion-metadata "" table nil) 'affixation-function)))
            (should (= calls 0))
            (should (equal "" (cadar (funcall affix (list title)))))
            (should (= calls 1))
            (setq image '(image :type png :file "face.png"))
            (funcall publish candidates)
            (should (= calls 1))
            (let ((prefix (cadar (funcall affix (list title)))))
              (should (equal image (get-text-property 0 'display prefix))))
            title))
      (should
       (eq candidate
           (appkit-chat-completion-read-visual
            "Face: " candidates
            :subscribe (lambda (callback) (setq publish callback) nil)))))))

(ert-deftest appkit-chat-completion-visual-reader-keeps-filtered-selection ()
  (let* ((cat (appkit-chat-completion-candidate-create :label "cat"))
         (cap (appkit-chat-completion-candidate-create :label "cap"))
         (car (appkit-chat-completion-candidate-create :label "car"))
         (dog (appkit-chat-completion-candidate-create :label "dog"))
         (icomplete-mode t)
         (icomplete--scrolled-completions nil)
         (icomplete--scrolled-past nil)
         publish)
    (appkit-chat-completion-test--with-minibuffer
        (lambda (_table)
          (insert "ca")
          ;; Icomplete selects the first entry of its rotated native cache.
          (completion--cache-all-sorted-completions
           (point-min) (point-max) '("cap" "cat" . 0))
          (funcall publish (list car cat cap dog))
          (appkit-chat-completion--visual-refresh)
          (should (equal '("cap" "car" "cat" . 0)
                         completion-all-sorted-completions))
          (should (equal "ca" (buffer-string)))
          (should (= (point-max) (point)))
          (funcall publish (list cat dog))
          (appkit-chat-completion--visual-refresh)
          (should (equal '("cat" . 0) completion-all-sorted-completions))
          "cat")
      (should
       (eq cat
           (appkit-chat-completion-read-visual
            "Face: " (list cat cap)
            :subscribe (lambda (callback) (setq publish callback) nil)))))))

(ert-deftest appkit-chat-completion-visual-reader-detaches-on-all-exits ()
  (dolist (outcome '(return error quit))
    (with-temp-buffer
      (let* ((owner (current-buffer))
             (candidate (appkit-chat-completion-candidate-create :label "face"))
             (detached 0)
             publish table status-overlay result)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _)
                     (setq table collection)
                     (with-current-buffer owner
                       (run-hooks 'minibuffer-setup-hook)
                       (setq status-overlay
                             (car (append (car (overlay-lists))
                                          (cdr (overlay-lists)))))
                       (pcase outcome
                         ('return "face")
                         ('error (error "Reader failed"))
                         ('quit (signal 'quit nil)))))))
          (setq result
                (condition-case error-data
                    (appkit-chat-completion-read-visual
                     "Face: " (list candidate)
                     :subscribe
                     (lambda (callback)
                       (setq publish callback)
                       (funcall callback (list candidate) "Loading")
                       (lambda ()
                         (cl-incf detached)
                         ;; Unsubscription itself can deliver one final event.
                         (funcall callback nil "Too late"))))
                  ((error quit) (car error-data)))))
        (should (eq result (if (eq outcome 'return) candidate outcome)))
        (should (= detached 1))
        (should status-overlay)
        (should-not (overlay-buffer status-overlay))
        ;; Keep the owner alive to check lifetime fencing, not just buffer death.
        (funcall publish nil "Late event")
        (should (equal '("face") (all-completions "" table)))
        (should-not (append (car (overlay-lists)) (cdr (overlay-lists))))))))

(ert-deftest appkit-chat-completion-visual-reader-fences-reused-minibuffer ()
  (with-temp-buffer
    (let* ((owner (current-buffer))
           (old (appkit-chat-completion-candidate-create :label "old"))
           (new (appkit-chat-completion-candidate-create :label "new"))
           old-publish
           (second-reader nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (with-current-buffer owner
                     (run-hooks 'minibuffer-setup-hook)
                     (when second-reader
                       (funcall old-publish (list old) "Stale status")
                       (should (equal '("new") (all-completions "" table)))
                       (dolist (overlay
                                (append (car (overlay-lists))
                                        (cdr (overlay-lists))))
                         (should-not
                          (string-match-p
                           "Stale" (or (overlay-get overlay 'before-string) "")))))
                     (car (all-completions "" table))))))
        (should
         (eq old
             (appkit-chat-completion-read-visual
              "Old: " (list old)
              :subscribe (lambda (publish) (setq old-publish publish) nil))))
        (setq second-reader t)
        (should
         (eq new
             (appkit-chat-completion-read-visual
              "New: " nil
              :subscribe (lambda (publish)
                           (funcall publish (list new) "Current")
                           nil))))))))

(provide 'appkit-chat-completion-test)

;;; appkit-chat-completion-test.el ends here
