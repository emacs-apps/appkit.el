;;; appkit-fontify-test.el --- Native Font Lock projection contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'outline)
(require 'appkit-fontify)

(defvar appkit-fontify-test--buffers nil)
(defvar appkit-fontify-test--initialize nil)

(define-derived-mode appkit-fontify-test-mode emacs-lisp-mode "Fontify-Test"
  "Capture disposable buffers and optionally exercise mode initialization."
  (push (current-buffer) appkit-fontify-test--buffers)
  (when appkit-fontify-test--initialize
    (funcall appkit-fontify-test--initialize)))

(ert-deftest appkit-fontify-isolates-hooks-source-and-caller ()
  (with-temp-buffer
    (insert "caller buffer")
    (narrow-to-region 2 8)
    (goto-char 4)
    (let* ((caller (current-buffer))
           (tick (buffer-modified-tick))
           (appkit-fontify-test--buffers nil)
           (hook-runs 0)
           (count-hook (list (lambda () (cl-incf hook-runs))))
           (emacs-lisp-mode-hook count-hook)
           (appkit-fontify-test-mode-hook count-hook)
           (change-major-mode-hook count-hook)
           (after-change-major-mode-hook count-hook)
           (outline-minor-mode-hook count-hook)
           (font-lock-mode-hook count-hook)
           (appkit-fontify-test--initialize
            (lambda ()
              (should untrusted-content)
              (outline-minor-mode 1)
              (font-lock-mode 1)
              (setq-local kill-buffer-query-functions
                          (list (lambda () (error "Unexpected kill query"))))))
           (source (propertize "(let ((值 1)) 值)\r\n" 'read-only t 'face 'error))
           (snapshot (copy-sequence source)))
      (string-match "b" "abc")
      (let* ((matches (match-data))
             (result (appkit-fontify-string source 'appkit-fontify-test-mode)))
        (should (equal matches (match-data)))
        (should (equal (substring-no-properties result)
                       (substring-no-properties source)))
        (should (get-text-property 1 'face result))
        (should-not (get-text-property 1 'read-only result))
        (should (equal-including-properties source snapshot)))
      (should (equal "" (appkit-fontify-string "" 'appkit-fontify-test-mode)))
      (should (zerop hook-runs))
      (should (= 2 (length appkit-fontify-test--buffers)))
      (should-not (cl-some #'buffer-live-p appkit-fontify-test--buffers))
      (should (eq caller (current-buffer)))
      (should (eq major-mode 'fundamental-mode))
      (should (= 4 (point)))
      (should (= 2 (point-min)))
      (should (= 8 (point-max)))
      (should (= tick (buffer-modified-tick))))))

(ert-deftest appkit-fontify-projects-only-detached-faces ()
  (let ((fontify (symbol-function 'font-lock-ensure))
        (native-face (list :weight 'bold))
        result)
    (cl-letf (((symbol-function 'font-lock-ensure)
               (lambda (beg end)
                 (funcall fontify beg end)
                 (add-text-properties
                  beg end
                  (list 'face native-face 'font-lock-face 'italic
                        'display "replacement" 'invisible t
                        'keymap (make-sparse-keymap) 'help-echo "help"
                        'syntax-table '(1) 'read-only t
                        'appkit-ui-action #'ignore))
                 (overlay-put (make-overlay beg end) 'face 'warning))))
      (setq result (appkit-fontify-string "text" 'emacs-lisp-mode)))
    (should (equal (substring-no-properties result) "text"))
    (should (equal (get-text-property 0 'face result)
                   '((:weight bold) italic)))
    (dotimes (position (length result))
      (should (equal (text-properties-at position result)
                     '(face ((:weight bold) italic)))))
    (setcar (cdr (car (get-text-property 0 'face result))) 'light)
    (should (equal native-face '(:weight bold)))))

(ert-deftest appkit-fontify-failure-and-quit-dispose-scratch ()
  (let ((appkit-fontify-test--buffers nil))
    (let ((appkit-fontify-test--initialize
           (lambda () (error "Synthetic initialization failure"))))
      (should-not (appkit-fontify-string "source" 'appkit-fontify-test-mode)))
    (cl-letf (((symbol-function 'font-lock-ensure)
               (lambda (&rest _) (error "Synthetic fontification failure"))))
      (should-not (appkit-fontify-string "source" 'appkit-fontify-test-mode)))
    (let ((appkit-fontify-test--initialize (lambda () (signal 'quit nil))))
      (should (eq 'propagated
                  (condition-case nil
                      (appkit-fontify-string "source" 'appkit-fontify-test-mode)
                    (quit 'propagated)))))
    (should (= 3 (length appkit-fontify-test--buffers)))
    (should-not (cl-some #'buffer-live-p appkit-fontify-test--buffers)))
  (should-not (appkit-fontify-string "source" 'appkit-fontify-test-missing-mode))
  (should-error (appkit-fontify-string nil 'emacs-lisp-mode)
                :type 'wrong-type-argument)
  (should-error (appkit-fontify-string "source" "emacs-lisp-mode")
                :type 'wrong-type-argument))

(ert-deftest appkit-fontify-rejects-source-rewriting-even-when-narrowed ()
  (let ((appkit-fontify-test--buffers nil))
    (cl-letf (((symbol-function 'font-lock-ensure)
               (lambda (&rest _)
                 (goto-char (point-max))
                 (insert "extra")
                 (narrow-to-region (point-min) (- (point-max) 5)))))
      (should-not (appkit-fontify-string "source" 'appkit-fontify-test-mode)))
    (should-not (cl-some #'buffer-live-p appkit-fontify-test--buffers))))

(ert-deftest appkit-fontify-never-installs-missing-grammars ()
  (let ((treesit-auto-install-grammar 'always)
        (attempts 0)
        (appkit-fontify-test--buffers nil)
        (appkit-fontify-test--initialize
         (lambda () (treesit-ensure-installed 'appkit-fontify-test-missing))))
    (cl-letf (((symbol-function 'treesit-available-p) (lambda () t))
              ((symbol-function 'treesit-language-available-p)
               (lambda (&rest _) nil))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (&rest _) (cl-incf attempts)))
              ((symbol-function 'y-or-n-p)
               (lambda (&rest _) (cl-incf attempts))))
      (should (appkit-fontify-string "(let ())" 'appkit-fontify-test-mode)))
    (should (zerop attempts))
    (should (eq 'always treesit-auto-install-grammar))
    (should-not (cl-some #'buffer-live-p appkit-fontify-test--buffers))))

(ert-deftest appkit-fontify-markdown-retains-source-markers ()
  (skip-unless (and (treesit-available-p)
                    (treesit-language-available-p 'markdown)
                    (treesit-language-available-p 'markdown-inline)
                    (require 'markdown-ts-mode nil t)))
  (let* ((source "# 标题\n\n**bold** and `code`\n\n```elisp\n(let ((x 1)) x)\n```")
         (result (appkit-fontify-string source 'markdown-ts-mode)))
    (should (equal source (substring-no-properties result)))
    (should (get-text-property 2 'face result))
    (should (get-text-property (string-match "bold" source) 'face result))))

(provide 'appkit-fontify-test)

;;; appkit-fontify-test.el ends here
