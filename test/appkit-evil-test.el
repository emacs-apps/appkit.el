;;; appkit-evil-test.el --- Tests for Appkit Evil bindings -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit)
(require 'evil)

(ert-deftest appkit-evil-defers-state-bindings-until-keymap-exists ()
  (let ((symbol 'appkit-evil-test-deferred-mode-map))
    (when (boundp symbol)
      (makunbound symbol))
    (appkit-evil-define-keys 'normal symbol
      "RET" #'ignore)
    (should (assoc symbol
                   (mapcar (lambda (entry)
                             (cons (nth 1 entry) entry))
                           appkit-evil--deferred-bindings)))
    (set symbol (make-sparse-keymap))
    (appkit-evil--after-load "appkit-evil-test-deferred")
    (with-temp-buffer
      (use-local-map (symbol-value symbol))
      (evil-normal-state)
      (should (eq (key-binding (kbd "RET")) #'ignore))
      (should (eq (key-binding (kbd "g g"))
                  #'evil-goto-first-line)))))

(defvar appkit-evil-test-dynamic-mode-map (make-sparse-keymap))

(define-minor-mode appkit-evil-test-dynamic-mode
  "Test-only dynamic application mode."
  :keymap appkit-evil-test-dynamic-mode-map)

(appkit-evil-define-keys 'normal 'appkit-evil-test-dynamic-mode-map
  "RET" #'ignore)
(add-hook 'appkit-evil-test-dynamic-mode-hook
          #'appkit-evil-normalize-keymaps)

(ert-deftest appkit-evil-dynamic-minor-mode-refreshes-state-map ()
  (with-temp-buffer
    (evil-normal-state)
    (should-not (eq (key-binding (kbd "RET")) #'ignore))
    (appkit-evil-test-dynamic-mode 1)
    (should (eq (key-binding (kbd "RET")) #'ignore))
    (appkit-evil-test-dynamic-mode -1)
    (should-not (eq (key-binding (kbd "RET")) #'ignore))))

(ert-deftest appkit-evil-map-groups-maps-and-state-shorthands ()
  (let ((map (make-sparse-keymap)))
    (set 'appkit-evil-test-string-mode-map map)
    (unwind-protect
        (progn
          (appkit-evil-map
            (:map appkit-evil-test-string-mode-map
             :nm
             "g r" #'ignore
             :n
             "D" #'ignore))
          (with-temp-buffer
            (use-local-map map)
            (evil-normal-state)
            (should (eq (key-binding (kbd "g r")) #'ignore))
            (should (eq (key-binding (kbd "D")) #'ignore))
            (should (eq (key-binding (kbd "g g"))
                        #'evil-goto-first-line))
            (evil-motion-state)
            (should (eq (key-binding (kbd "g r")) #'ignore))
            (should-not (eq (key-binding (kbd "D")) #'ignore))))
      (makunbound 'appkit-evil-test-string-mode-map))))

(ert-deftest appkit-evil-chatbuf-enters-input-in-one-command ()
  (let (focused inserted)
    (cl-letf (((symbol-function 'appkit-chatbuf-focus-input)
               (lambda () (setq focused t)))
              ((symbol-function 'evil-insert-state)
               (lambda () (setq inserted t))))
      (appkit-evil-chatbuf-enter-input)
      (should focused)
      (should inserted))))

(provide 'appkit-evil-test)

;;; appkit-evil-test.el ends here
