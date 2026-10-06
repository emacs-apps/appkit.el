;;; appkit-bindings-test.el --- Semantic theme behavior -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit-bindings)

(ert-deftest appkit-bindings-theme-moves-action-across-adapters-and-restores-parents ()
  "A shared remap changes real editing in both adapters, retiring old local keys."
  (let ((appkit-bindings--adapters (make-hash-table :test #'eq))
        (appkit-binding-overrides nil)
        (one (make-symbol "appkit-bindings-one"))
        (two (make-symbol "appkit-bindings-two"))
        (parent (make-sparse-keymap)))
    (define-key parent (kbd "RET") #'backward-char)
    (unwind-protect
        (progn
          (dolist (symbol (list one two))
            (set symbol (make-sparse-keymap))
            (set-keymap-parent (symbol-value symbol) parent)
            (appkit-bindings-define symbol 'input 'return #'newline))
          (dolist (symbol (list one two))
            (with-temp-buffer
              (use-local-map (symbol-value symbol))
              (insert "draft")
              (call-interactively (key-binding (kbd "RET")))
              (should (equal (buffer-string) "draft\n"))))
          (setq appkit-binding-overrides '((return :native ("<f8>"))))
          (dolist (symbol (list one two))
            (appkit-bindings-refresh symbol)
            (with-temp-buffer
              (use-local-map (symbol-value symbol))
              (insert "draft")
              (call-interactively (key-binding (kbd "RET")))
              (should (= (point) 5))
              (call-interactively (key-binding (kbd "<f8>")))
              (should (equal (buffer-string) "draf\nt"))))
          ;; Explicit nil disables the action rather than reusing the default.
          (setq appkit-binding-overrides '((return :enabled nil)))
          (dolist (symbol (list one two))
            (appkit-bindings-refresh symbol)
            (with-temp-buffer
              (use-local-map (symbol-value symbol))
              (insert "draft")
              (call-interactively (key-binding (kbd "RET")))
              (should (= (point) 5))
              (should (equal (buffer-string) "draft")))))
      (makunbound one)
      (makunbound two))))

(ert-deftest appkit-bindings-refresh-preserves-custom-edits-and-unrelated-actions ()
  "A user replacement survives repeated theme refreshes without pinning peers."
  (let ((appkit-bindings--adapters (make-hash-table :test #'eq))
        (appkit-binding-overrides '((return :native ("<f8>"))))
        (one (make-symbol "appkit-bindings-custom"))
        (two (make-symbol "appkit-bindings-peer")))
    (unwind-protect
        (progn
          (dolist (symbol (list one two))
            (set symbol (make-sparse-keymap))
            (define-key (symbol-value symbol) (kbd "<f10>") #'forward-char)
            (appkit-bindings-define symbol 'input 'return #'newline))
          (define-key (symbol-value one) (kbd "<f8>") #'delete-char)
          (dotimes (_ 2)
            (appkit-bindings-refresh one)
            (appkit-bindings-refresh two))
          (with-temp-buffer
            (use-local-map (symbol-value one))
            (insert "abc") (goto-char (point-min))
            (call-interactively (key-binding (kbd "<f8>")))
            (should (equal (buffer-string) "bc"))
            (call-interactively (key-binding (kbd "<f10>")))
            (should (= (point) 2)))
          (with-temp-buffer
            (use-local-map (symbol-value two))
            (insert "abc") (goto-char (point-min))
            (call-interactively (key-binding (kbd "<f8>")))
            (should (equal (buffer-string) "\nabc"))))
      (makunbound one)
      (makunbound two))))

(ert-deftest appkit-bindings-managed-text-composer-keeps-evil-substitution ()
  "A declared composer and its child map retain editing, not Snipe's s motion."
  (require 'evil-snipe)
  (let ((appkit-bindings--adapters (make-hash-table :test #'eq))
        (appkit-bindings--managed-maps (make-hash-table :test #'eq :weakness 'key))
        (appkit-binding-overrides nil)
        (evil-snipe-mode t)
        (evil-snipe-override-mode t)
        (evil-snipe-disabled-modes nil)
        (parent (make-symbol "appkit-bindings-composer")))
    (unwind-protect
        (save-window-excursion
          (with-temp-buffer
            (text-mode) (evil-local-mode 1) (evil-normal-state)
            (turn-on-evil-snipe-mode) (turn-on-evil-snipe-override-mode)
            (should evil-snipe-local-mode)
            (set parent (make-sparse-keymap))
            (appkit-bindings-define parent 'input 'return #'newline)
            (let ((child (make-sparse-keymap))
                  (buffer (current-buffer)))
              (set-keymap-parent child (symbol-value parent))
              (use-local-map child)
              (cl-letf (((symbol-function 'buffer-list) (lambda (&optional _frame) (list buffer))))
                (appkit-bindings-refresh parent)))
            (should-not evil-snipe-local-mode)
            (should-not evil-snipe-override-local-mode)
            (switch-to-buffer (current-buffer))
            (insert "hello") (goto-char (point-min))
            (execute-kbd-macro (kbd "s"))
            (should (equal (buffer-string) "ello"))
            (should (eq evil-state 'insert))))
      (makunbound parent))))

(provide 'appkit-bindings-test)
;;; appkit-bindings-test.el ends here
