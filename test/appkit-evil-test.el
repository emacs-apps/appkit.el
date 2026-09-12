;;; appkit-evil-test.el --- Tests for Appkit Evil bindings -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit)
(require 'evil)
(require 'appkit-evil)

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
  (let ((map (make-sparse-keymap))
        (other-map (make-sparse-keymap)))
    (set 'appkit-evil-test-string-mode-map map)
    (set 'appkit-evil-test-other-mode-map other-map)
    (unwind-protect
        (progn
          (appkit-evil-map
            :map appkit-evil-test-string-mode-map
            :nm
            "g r" #'ignore
            "g o" #'beginning-of-buffer
            :n
            "D" #'ignore
            :map appkit-evil-test-other-mode-map
            :i
            "g r" #'forward-char)
          (with-temp-buffer
            (use-local-map map)
            (evil-normal-state)
            (should (eq (key-binding (kbd "g r")) #'ignore))
            (should (eq (key-binding (kbd "g o")) #'beginning-of-buffer))
            (should (eq (key-binding (kbd "D")) #'ignore))
            (should (eq (key-binding (kbd "g g"))
                        #'evil-goto-first-line))
            (evil-motion-state)
            (should (eq (key-binding (kbd "g r")) #'ignore))
            (should (eq (key-binding (kbd "g o")) #'beginning-of-buffer))
            (should-not (eq (key-binding (kbd "D")) #'ignore)))
          (with-temp-buffer
            (use-local-map other-map)
            (evil-normal-state)
            (should-not (eq (key-binding (kbd "D")) #'ignore))
            (should-not (eq (key-binding (kbd "g r")) #'forward-char))
            (evil-motion-state)
            (should-not (eq (key-binding (kbd "g r")) #'ignore))
            (evil-insert-state)
            (should (eq (key-binding (kbd "g r")) #'forward-char))))
      (makunbound 'appkit-evil-test-string-mode-map)
      (makunbound 'appkit-evil-test-other-mode-map))))

(ert-deftest appkit-evil-map-requires-state-after-each-map ()
  (should-error
   (macroexpand
    '(appkit-evil-map
       :map appkit-evil-test-string-mode-map
       :nm "g r" #'ignore
       :map appkit-evil-test-other-mode-map
       "D" #'ignore))))

(ert-deftest appkit-evil-map-rejects-malformed-bindings ()
  (dolist (args '((:map)
                  (:map (make-sparse-keymap) :n "D" ignore)
                  (:n "D" ignore)
                  (:map example-mode-map "D" ignore)
                  (:map example-mode-map :n "D")
                  (:map example-mode-map :n "D" :m "x" ignore)
                  ((:map example-mode-map :n "D" ignore))))
    (should-error (macroexpand (cons 'appkit-evil-map args)))))

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

(ert-deftest
    appkit-evil-normalizes-live-descendants-after-parent-bindings nil
  (let*
      ((parent (make-symbol "appkit-test-parent"))
       (child (make-symbol "appkit-test-child"))
       (parent-map (make-sparse-keymap))
       (child-map (make-sparse-keymap)))
    (put child 'derived-mode-parent parent)
    (set-keymap-parent child-map parent-map)
    (with-temp-buffer
      (setq major-mode child) (use-local-map child-map)
      (evil-local-mode 1) (evil-normal-state)
      (should-not (eq (key-binding (kbd "z Z")) #'ignore))
      (evil-define-key* 'normal parent-map (kbd "z Z") #'ignore)
      (should-not (eq (key-binding (kbd "z Z")) #'ignore))
      (appkit-evil-normalize-buffers (list parent))
      (should (eq (key-binding (kbd "z Z")) #'ignore)))))
