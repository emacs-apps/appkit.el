;;; appkit-name-color-test.el --- Tests for Appkit name colors -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'appkit-name-color)

(ert-deftest appkit-name-color-default-palette-defines-faces ()
  (should (= 8 (length appkit-name-color-palette)))
  (dolist (face appkit-name-color-palette)
    (should (facep face))))

(ert-deftest appkit-name-color-is-stable-and-palette-bound ()
  (let* ((palette [face-a face-b face-c])
         (first (appkit-name-color-face "user:144115219000000001" palette)))
    (should (memq first (append palette nil)))
    (should
     (eq first
         (appkit-name-color-face "user:144115219000000001" palette)))
    (should
     (eq (appkit-name-color-face "用户:一" palette)
         (appkit-name-color-face "用户:一" palette)))))

(ert-deftest appkit-name-color-handles-unavailable-input ()
  (should-not (appkit-name-color-face nil))
  (should-not (appkit-name-color-face ""))
  (should-not (appkit-name-color-face "user:1" []))
  (should-error (appkit-name-color-face 1))
  (should-error (appkit-name-color-face "user:1" "not-a-palette")))

(provide 'appkit-name-color-test)

;;; appkit-name-color-test.el ends here
