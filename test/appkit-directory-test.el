;;; appkit-directory-test.el --- Tests for directory surfaces -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-directory)

(defmacro appkit-directory-test--with-surface (&rest body)
  "Run BODY in one initialized temporary directory surface."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (appkit-directory-mode)
     (let ((surface (appkit-directory-surface)))
       ,@body)))

(defun appkit-directory-test--item-inserter (_surface entry)
  "Insert synthetic item ENTRY."
  (insert (appkit-directory-entry-label entry) "\n"))

(defun appkit-directory-test--entries (&optional first-expanded-p)
  "Return a section, group, two items, note, and spacer projection."
  (list
   (appkit-directory-entry-create
    :key '(section . "s")
    :role 'section
    :label "Scope"
    :foldable-p t
    :fold-key '(section . "s")
    :fold-default-expanded-p t
    :expanded-p t)
   (appkit-directory-entry-create
    :key '(group . "g")
    :role 'group
    :section-key '(section . "s")
    :label "Group"
    :indent 2
    :foldable-p t
    :fold-key '(group . "g")
    :fold-default-expanded-p t
    :expanded-p (and first-expanded-p t))
   (appkit-directory-entry-create
    :key '(item . "one")
    :role 'item
    :section-key '(section . "s")
    :group-key '(group . "g")
    :label "One"
    :indent 4
    :item-p t
    :unread-p t
    :payload 'one)
   (appkit-directory-entry-create
    :key '(item . "two")
    :role 'item
    :section-key '(section . "s")
    :group-key '(group . "g")
    :label "Two"
    :indent 4
    :item-p t
    :payload 'two)
   (appkit-directory-entry-create
    :key 'note
    :role 'note
    :label "Note"
    :indent 2
    :face 'shadow)
   (appkit-directory-entry-create :key 'gap :role 'spacer)))

(ert-deftest appkit-directory-renders-flat-roles-with-stable-properties ()
  (appkit-directory-test--with-surface
    (appkit-directory-configure
     surface
     :item-inserter #'appkit-directory-test--item-inserter)
    (appkit-directory-reconcile
     surface (appkit-directory-test--entries t))
    (should (equal "▾ Scope\n▾ Group\nOne\nTwo\nNote\n\n"
                   (buffer-string)))
    (goto-char (point-min))
    (search-forward "One")
    (should-not (button-at (1- (point))))
    (should (equal '(item . "one")
                   (appkit-directory-key-at-point (1- (point)))))
    (should (get-text-property
             (1- (point)) appkit-directory-item-property))
    (should (get-text-property
             (1- (point)) appkit-directory-unread-property))))

(ert-deftest appkit-directory-entry-inserter-handles-any-entry-after-prefix ()
  (appkit-directory-test--with-surface
    (let (item-fallback-called)
      (appkit-directory-configure
       surface
       :entry-inserter
       (lambda (_surface entry)
         (pcase (appkit-directory-entry-role entry)
           ('section
            (insert "Custom section\n")
            t)
           ('item
            (insert "Handled\n")
            t)))
       :item-inserter
       (lambda (&rest _arguments)
         (setq item-fallback-called t)
         (insert "Fallback\n")))
      (appkit-directory-reconcile
       surface
       (list
        (appkit-directory-entry-create
         :key 'section
         :role 'section
         :label "Section")
        (appkit-directory-entry-create
         :key 'item
         :role 'item
         :section-key 'section
         :indent 2
         :item-p t)))
      (should (equal "Custom section\nHandled\n" (buffer-string)))
      (should-not item-fallback-called))))

(ert-deftest appkit-directory-entry-inserter-nil-falls-back ()
  (appkit-directory-test--with-surface
    (let (seen)
      (appkit-directory-configure
       surface
       :entry-inserter
       (lambda (_surface entry)
         (push (appkit-directory-entry-key entry) seen)
         nil)
       :item-inserter #'appkit-directory-test--item-inserter)
      (appkit-directory-reconcile
       surface
       (list
        (appkit-directory-entry-create
         :key 'section
         :role 'section
         :label "Section")
        (appkit-directory-entry-create
         :key 'item
         :role 'item
         :section-key 'section
         :label "Item"
         :item-p t)))
      (should (equal "Section\nItem\n" (buffer-string)))
      (should (equal '(item section) seen)))))

(ert-deftest appkit-directory-entry-inserter-remains-single-row ()
  (appkit-directory-test--with-surface
    (appkit-directory-configure
     surface
     :entry-inserter
     (lambda (&rest _arguments)
       (insert "First\nSecond\n")
       t))
    (should-error
     (appkit-directory-reconcile
      surface
      (list
       (appkit-directory-entry-create
        :key 'note
        :role 'note
        :label "Ignored"))))))

(ert-deftest appkit-directory-item-may-belong-directly-to-section ()
  (appkit-directory-test--with-surface
    (let ((entry
           (appkit-directory-entry-create
            :key 'direct
            :role 'item
            :section-key 'section
            :label "Direct"
            :item-p t)))
      (appkit-directory-reconcile surface (list entry))
      (should (equal "Direct\n" (buffer-string)))
      (should (equal 'section
                     (get-text-property
                      (point-min) appkit-directory-section-property)))
      (should-not
       (get-text-property (point-min) appkit-directory-group-property)))))

(ert-deftest appkit-directory-action-row-excludes-the-terminating-newline ()
  (appkit-directory-test--with-surface
    (appkit-directory-configure
     surface
     :item-inserter #'appkit-directory-test--item-inserter
     :action-rows-p t)
    (appkit-directory-reconcile
     surface (appkit-directory-test--entries t))
    (goto-char (point-min))
    (search-forward "One")
    (let* ((button (button-at (1- (point))))
           (end (button-end button)))
      (should button)
      (should (equal '(item . "one")
                     (appkit-directory-entry-key
                      (cadr (button-get button 'appkit-ui-button-action)))))
      (should (eq (char-after end) ?\n))
      (should-not (button-at end)))))

(ert-deftest appkit-directory-fold-state-keeps-defaults-and-forced-expansion-distinct ()
  (appkit-directory-test--with-surface
    (let ((key '(category . "stable")))
      (should (appkit-directory-fold-expanded-p surface key t))
      (should-not (appkit-directory-fold-expanded-p surface key nil))
      (appkit-directory-set-fold-expanded surface key nil)
      (should-not (appkit-directory-fold-expanded-p surface key t))
      (should (appkit-directory-fold-expanded-p surface key t t))
      (should-not (appkit-directory-fold-expanded-p surface key t))
      (appkit-directory-clear-fold-state surface key)
      (should (appkit-directory-fold-expanded-p surface key t)))))

(ert-deftest appkit-directory-initialize-can-reuse-fold-state ()
  (with-temp-buffer
    (should-not (appkit-directory-current-surface))
    (let ((fold-state (make-hash-table :test #'equal)))
      (puthash '(group . "stable") nil fold-state)
      (let ((surface (appkit-directory-initialize :fold-state fold-state)))
        (should (eq surface (appkit-directory-current-surface)))
        (should (eq fold-state
                    (appkit-directory-surface-fold-state surface)))
        (should-not
         (appkit-directory-fold-expanded-p
          surface '(group . "stable") t))
        (should (eq fold-state (appkit-directory-retire)))
        (should-not (appkit-directory-current-surface))
        (should-not (appkit-directory-retire))
        (should-error (appkit-directory-surface))))))

(ert-deftest appkit-directory-reconcile-retains-and-targets-ewoc-nodes ()
  (appkit-directory-test--with-surface
    (let ((prints (make-hash-table :test #'equal)))
      (appkit-directory-configure
       surface
       :item-inserter
       (lambda (_surface entry)
         (cl-incf (gethash (appkit-directory-entry-key entry) prints 0))
         (insert (appkit-directory-entry-label entry) "\n")))
      (let* ((entries (appkit-directory-test--entries t))
             (_ (appkit-directory-reconcile surface entries))
             (key '(item . "one"))
             (node (gethash key
                            (appkit-directory-surface-node-table surface))))
        (appkit-directory-reconcile surface (copy-tree entries))
        (should (eq node
                    (gethash key
                             (appkit-directory-surface-node-table surface))))
        (should (= 1 (gethash key prints)))
        (appkit-directory-reconcile
         surface entries
         :force-keys (list key))
        (should (= 2 (gethash key prints)))))))

(ert-deftest appkit-directory-navigation-and-activation-ignore-passive-rows ()
  (appkit-directory-test--with-surface
    (let (activated folded)
      (appkit-directory-configure
       surface
       :item-inserter #'appkit-directory-test--item-inserter
       :activate-function
       (lambda (_surface entry)
         (setq activated (appkit-directory-entry-payload entry)))
       :fold-function
       (lambda (_surface entry expanded-p)
         (setq folded (list (appkit-directory-entry-fold-key entry)
                            expanded-p))))
      (appkit-directory-reconcile
       surface (appkit-directory-test--entries t))
      (goto-char (point-min))
      (appkit-directory-next-item)
      (should (equal '(item . "one")
                     (appkit-directory-key-at-point)))
      (appkit-directory-activate)
      (should (eq activated 'one))
      (appkit-directory-next-item)
      (should (equal '(item . "two")
                     (appkit-directory-key-at-point)))
      (appkit-directory-previous-item)
      (should (equal '(item . "one")
                     (appkit-directory-key-at-point)))
      (goto-char (point-min))
      (appkit-directory-activate)
      (should (equal folded '((section . "s") nil))))))

(ert-deftest appkit-directory-foldable-item-separates-primary-activation-and-fold ()
  (appkit-directory-test--with-surface
    (let (activated folded)
      (appkit-directory-configure
       surface
       :activate-function
       (lambda (_surface entry)
         (setq activated (appkit-directory-entry-payload entry)))
       :fold-function
       (lambda (_surface entry expanded-p)
         (setq folded
               (list (appkit-directory-entry-fold-key entry) expanded-p))))
      (let* ((key '(channel . "general"))
             (fold-key '(threads . "general"))
             (entry
              (appkit-directory-entry-create
               :key key
               :role 'item
               :section-key 'channels
               :item-p t
               :payload 'general
               :foldable-p t
               :fold-key fold-key
               :fold-default-expanded-p nil
               :expanded-p nil
               :primary-action 'item)))
        (appkit-directory-reconcile surface (list entry))
        (goto-char (point-min))
        (appkit-directory-activate)
        (should (eq activated 'general))
        (should-not folded)
        (should-not (appkit-directory-fold-expanded-p surface fold-key nil))
        (appkit-directory-tab-dwim)
        (should (equal folded (list fold-key t)))
        (should (appkit-directory-fold-expanded-p surface fold-key nil))))))

(ert-deftest appkit-directory-next-unread-wraps-over-items-only ()
  (appkit-directory-test--with-surface
    (appkit-directory-configure
     surface
     :item-inserter #'appkit-directory-test--item-inserter)
    (appkit-directory-reconcile
     surface (appkit-directory-test--entries t))
    (goto-char (point-max))
    (appkit-directory-next-unread)
    (should (equal '(item . "one")
                   (appkit-directory-key-at-point)))))

(ert-deftest appkit-directory-rolls-back-fold-state-when-adapter-fails ()
  (appkit-directory-test--with-surface
    (appkit-directory-configure
     surface
     :fold-function
     (lambda (&rest _arguments)
       (error "synthetic fold failure")))
    (let ((entry (car (appkit-directory-test--entries t)))
          (key '(section . "s")))
      (should (appkit-directory-fold-expanded-p surface key t))
      (should-error (appkit-directory-toggle-entry-fold surface entry))
      (should (appkit-directory-fold-expanded-p surface key t)))))

(ert-deftest appkit-directory-rejects-invalid-or-duplicate-projections ()
  (appkit-directory-test--with-surface
    (should-error
     (appkit-directory-reconcile
      surface
      (list (appkit-directory-entry-create :key "x" :role 'tree))))
    (should-error
     (appkit-directory-reconcile
      surface
      (list (appkit-directory-entry-create :key "x" :role 'note)
            (appkit-directory-entry-create :key "x" :role 'note))))
    (should-error
     (appkit-directory-reconcile
      surface
      (list (appkit-directory-entry-create
             :key "group"
             :role 'group
             :label "orphan"))))
    (should-error
     (appkit-directory-reconcile
      surface
      (list (appkit-directory-entry-create
             :key "item"
             :role 'item
             :item-p t))))))

(ert-deftest appkit-directory-primary-action-call-needs-no-application-dispatcher ()
  (appkit-directory-test--with-surface
    (let* ((result (list nil))
           (entry (appkit-directory-entry-create
                   :key 'retry :role 'item :section-key 'requests :item-p t
                   :label "Retry" :primary-action (list #'setcar result 'retried))))
      (appkit-directory-configure
       surface :action-rows-p t
       :activate-function (lambda (&rest _) (ert-fail "Unexpected item adapter")))
      (appkit-directory-reconcile surface (list entry))
      (should-not (car result))
      (goto-char (point-min))
      (appkit-directory-activate)
      (should (eq (car result) 'retried))
      (setcar result nil)
      (should (appkit-ui-activate-at))
      (should (eq (car result) 'retried)))
    (should-error
     (appkit-directory-reconcile
      surface (list (appkit-directory-entry-create
                     :key 'note :role 'note :primary-action '(ignore)))))))

(ert-deftest appkit-directory-rejects-invalid-primary-actions ()
  (appkit-directory-test--with-surface
    (should-error
     (appkit-directory-reconcile
      surface
      (list
       (appkit-directory-entry-create
        :key "not-foldable"
        :role 'item
        :section-key 'section
        :item-p t
        :primary-action 'fold))))
    (should-error
     (appkit-directory-reconcile
      surface
      (list
       (appkit-directory-entry-create
        :key "not-item"
        :role 'note
        :primary-action 'item))))))

(ert-deftest appkit-directory-read-actions-target-only-current-item ()
  "Read actions ignore marks and region, including for unmarkable items."
  (appkit-directory-test--with-surface
    (let ((entries (appkit-directory-test--markable-entries))
          seen)
      (setf (appkit-directory-entry-markable-p (nth 2 entries)) nil)
      (appkit-directory-configure
       surface :operation-function
       (lambda (_surface operation targets)
         (push (list operation targets) seen)))
      (appkit-directory-reconcile surface entries)
      (appkit-directory-mark-all)
      (let ((marks (appkit-directory-selected-keys surface)))
        (goto-char
         (ewoc-location
          (gethash '(item . "one")
                   (appkit-directory-surface-node-table surface))))
        (let ((transient-mark-mode t))
          (push-mark (point-max) t t)
          (appkit-directory-mark-read)
          (appkit-directory-toggle-read))
        (should
         (equal '((toggle-read ((item . "one")))
                  (mark-read ((item . "one"))))
                seen))
        (should (equal marks (appkit-directory-selected-keys surface)))
        (should (equal '(item . "one") (appkit-directory-key-at-point)))))))

(ert-deftest appkit-directory-read-actions-reject-passive-or-unsupported-rows ()
  (appkit-directory-test--with-surface
    (let (seen)
      (appkit-directory-configure
       surface :operation-function
       (lambda (&rest _) (setq seen t)))
      (appkit-directory-reconcile surface (appkit-directory-test--entries t))
      (goto-char (point-min))
      (should-error (appkit-directory-mark-read) :type 'user-error)
      (should-not seen)
      (appkit-directory-next-item)
      (appkit-directory-configure surface :operation-function nil)
      (should-error (appkit-directory-toggle-read) :type 'user-error))))

(ert-deftest appkit-directory-indent-is-display-only-and-composes-with-adapters ()
  "Rows expose content at BOL while preserving the adapter's display prefix."
  (appkit-directory-test--with-surface
    (appkit-directory-configure
     surface
     :entry-inserter
     (lambda (_surface entry)
       (when (eq (appkit-directory-entry-role entry) 'item)
         (let ((start (point)))
           (insert (appkit-directory-entry-label entry) "\n")
           (appkit-ui-apply-line-prefix start (point) "icon "))
         t)))
    (let ((entries (appkit-directory-test--entries t)))
      (appkit-directory-reconcile surface entries)
      (goto-char (point-min))
      (forward-line 1)
      (should (eq (char-after) ?▾))
      (should (equal "  " (get-text-property (point) 'line-prefix)))
      (appkit-directory-next-item)
      (should (eq (char-after) ?O))
      (should (equal "    icon " (get-text-property (point) 'line-prefix)))
      (should (equal "    icon " (get-text-property (point) 'wrap-prefix)))
      (forward-char 1)
      (appkit-directory-reconcile
       surface entries :force-keys '((item . "one")))
      (should (eq (char-after) ?n))
      (beginning-of-line)
      (should (eq (char-after) ?O))
      (should (equal '(item . "one") (appkit-directory-key-at-point)))
      (should-not (string-match-p "^ +" (buffer-string))))))

(ert-deftest appkit-directory-display-indent-preserves-aligned-row-width ()
  "Presentation adapters measure display-only indentation before rendering."
  (require 'appkit-presentation)
  (dolist (preview (list "Short" (make-string 90 ?b)))
    (let ((row (appkit-presentation-one-line-row-create
                :context (make-string 60 ?A)
                :preview (appkit-ui-one-line-preview-create :text preview)
                :time "12:30"))
          baseline)
      (cl-labels
          ((insert-row ()
             (appkit-presentation-insert-one-line-row
              row :width 70 :icon-slot-width 0
              :context-width-spec '(0.45 20) :time-slot-width 6))
           (layout (start)
             (list (buffer-substring-no-properties start (point-max))
                   (let (displays)
                     (while (< start (point-max))
                       (when-let* ((display (get-text-property start 'display)))
                         (push display displays))
                       (setq start
                             (next-single-property-change
                              start 'display nil (point-max))))
                     (nreverse displays)))))
        (with-temp-buffer
          (insert (make-string 8 ?\s))
          (let ((start (point)))
            (insert-row)
            (setq baseline (layout start))))
        (appkit-directory-test--with-surface
          (appkit-directory-configure
           surface :entry-inserter
           (lambda (_surface _entry)
             (should (= 8 (appkit-geometry-current-column)))
             (insert-row)
             t))
          (appkit-directory-reconcile
           surface
           (list (appkit-directory-entry-create
                  :key 'row :role 'note :indent 8)))
          (should (equal baseline (layout (point-min))))
          (goto-char (point-min))
          (should (eq (char-after) ?\[))
          (should (equal (make-string 8 ?\s)
                         (get-text-property (point) 'line-prefix))))))))

(provide 'appkit-directory-test)

;;; appkit-directory-test.el ends here

(defun appkit-directory-test--markable-entries ()
  "Return a projection with markable group and item rows."
  (list
   (appkit-directory-entry-create
    :key '(section . "s")
    :role 'section
    :label "Scope")
   (appkit-directory-entry-create
    :key '(group . "g")
    :role 'group
    :section-key '(section . "s")
    :label "Folder"
    :markable-p t)
   (appkit-directory-entry-create
    :key '(item . "one")
    :role 'item
    :section-key '(section . "s")
    :group-key '(group . "g")
    :label "One"
    :item-p t
    :markable-p t)
   (appkit-directory-entry-create
    :key '(item . "two")
    :role 'item
    :section-key '(section . "s")
    :group-key '(group . "g")
    :label "Two"
    :item-p t
    :markable-p t)))

(ert-deftest appkit-directory-markable-rows-exclude-sections ()
  (appkit-directory-test--with-surface
    (appkit-directory-reconcile
     surface (appkit-directory-test--markable-entries))
    (should (equal '((group . "g") (item . "one") (item . "two"))
                   (appkit-directory--keys-at-positions
                    (appkit-directory--markable-positions surface))))
    (should-error
     (appkit-directory-reconcile
      surface
      (list (appkit-directory-entry-create
             :key 'section
             :role 'section
             :label "Section"
             :markable-p t))))))

(ert-deftest appkit-directory-mark-shows-indicator-and-advances ()
  (appkit-directory-test--with-surface
    (appkit-directory-reconcile
     surface (appkit-directory-test--markable-entries))
    (goto-char (point-min))
    (search-forward "One")
    (beginning-of-line)
    (appkit-directory-mark)
    (should (equal '((item . "one"))
                   (appkit-directory-selected-keys surface)))
    (should (string-match-p "\\* One" (buffer-string)))
    (should (equal '(item . "two")
                   (appkit-directory-key-at-point)))
    (should (appkit-directory-marked-p '(item . "one") surface))
    (should-not (appkit-directory-marked-p '(item . "two") surface))))

(ert-deftest appkit-directory-region-marking-and-target-precedence ()
  (appkit-directory-test--with-surface
    (appkit-directory-reconcile
     surface (appkit-directory-test--markable-entries))
    (goto-char (point-min))
    (search-forward "One")
    (beginning-of-line)
    (set-mark (point))
    (goto-char (point-max))
    (activate-mark)
    (appkit-directory-mark)
    (should (equal '((item . "one") (item . "two"))
                   (appkit-directory-selected-keys surface)))
    (should (equal '((item . "one") (item . "two"))
                   (appkit-directory-target-keys)))
    (deactivate-mark)
    (should (equal '("captured")
                   (appkit-directory-target-keys
                    :captured '("captured")
                    :region '("region") :marked '("marked") :point "point")))
    (should-not
     (appkit-directory-target-keys
      :region nil :marked '("marked") :point "point"))
    (should (equal '("marked")
                   (appkit-directory-target-keys
                    :marked '("marked") :point "point")))))

(ert-deftest appkit-directory-mark-all-toggle-and-clear ()
  (appkit-directory-test--with-surface
    (appkit-directory-reconcile
     surface (appkit-directory-test--markable-entries))
    (appkit-directory-mark-all)
    (should (equal '((group . "g") (item . "one") (item . "two"))
                   (appkit-directory-selected-keys surface)))
    (appkit-directory-toggle-all-marks)
    (should-not (appkit-directory-selected-keys surface))
    (appkit-directory-toggle-all-marks)
    (should (= 3 (length (appkit-directory-selected-keys surface))))
    (appkit-directory-clear-marks)
    (should-not (appkit-directory-selected-keys surface))))

(ert-deftest appkit-directory-marks-survive-reconcile-and-forget ()
  (appkit-directory-test--with-surface
    (let ((entries (appkit-directory-test--markable-entries)))
      (appkit-directory-reconcile surface entries)
      (appkit-directory-mark-all)
      (appkit-directory-reconcile surface entries)
      (should (= 3 (length (appkit-directory-selected-keys surface))))
      (should (string-match-p "\\* Folder" (buffer-string)))
      (appkit-directory-selection-forget '((item . "one")) surface)
      (should (equal '((group . "g") (item . "two"))
                     (appkit-directory-selected-keys surface)))
      (should-not (string-match-p "\\* One" (buffer-string))))))

(ert-deftest appkit-directory-operation-receives-captured-targets ()
  (appkit-directory-test--with-surface
    (let (seen)
      (appkit-directory-configure
       surface
       :operation-function
       (lambda (_surface operation targets)
         (setq seen (list operation targets))))
      (appkit-directory-reconcile
       surface (appkit-directory-test--markable-entries))
      (appkit-directory-mark-all)
      (appkit-directory-run-operation 'delete)
      (should (equal '(delete ((group . "g") (item . "one") (item . "two")))
                     seen)))))

(ert-deftest appkit-directory-mark-mode-is-opt-in ()
  (appkit-directory-test--with-surface
    (should-not (bound-and-true-p appkit-directory-mark-mode))
    (should-not (eq (key-binding (kbd "m")) #'appkit-directory-mark))
    (appkit-directory-mark-mode 1)
    (should (bound-and-true-p appkit-directory-mark-mode))
    (should (eq (key-binding (kbd "m")) #'appkit-directory-mark))
    (should (eq (key-binding (kbd "M-n")) #'appkit-directory-next-marked))))
