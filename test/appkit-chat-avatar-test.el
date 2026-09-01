;;; appkit-chat-avatar-test.el --- Tests for shared chat avatars -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-chat-avatar)

(ert-deftest appkit-chat-avatar-prefixes-have-stable-text-placeholder ()
  (let ((prefixes (appkit-chat-avatar-prefixes nil "@")))
    (should (equal (plist-get prefixes :header) "@ "))
    (should (equal (plist-get prefixes :first-body) "  "))
    (should (equal (plist-get prefixes :rest-body) "  "))))

(ert-deftest appkit-chat-avatar-placeholder-reserves-loaded-image-width ()
  (cl-letf (((symbol-function 'appkit-chat-avatar-column-pixel-width)
             (lambda () 8)))
    (let ((prefixes
           (appkit-chat-avatar-prefixes nil "A" :pixel-size 32)))
      (should (equal (plist-get prefixes :header) "A    "))
      (should (equal (plist-get prefixes :first-body) "     "))
      (should (equal (plist-get prefixes :rest-body) "     ")))))

(ert-deftest appkit-chat-avatar-prefixes-slice-image-across-two-lines ()
  (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
             (lambda (image) (consp image)))
            ((symbol-function 'image-size)
             (lambda (&rest _args) (cons 32 32)))
            ((symbol-function 'appkit-chat-avatar-column-pixel-width)
             (lambda () 8))
            ((symbol-function 'display-graphic-p)
             (lambda (&optional _frame) t)))
    (let* ((prefixes
            (appkit-chat-avatar-prefixes
             '(image :type png :data "avatar") "A"
             :pixel-size 32
             :resize t))
           (header (plist-get prefixes :header))
           (first-body (plist-get prefixes :first-body))
           (rest-body (plist-get prefixes :rest-body))
           (header-display (get-text-property 0 'display header))
           (body-display (get-text-property 0 'display first-body))
           (rest-display (get-text-property 0 'display rest-body))
           (prepared-image (cadr header-display)))
      (should (= (string-width header) (string-width first-body)))
      (should (= (string-width header) (string-width rest-body)))
      (should (equal (car header-display) '(slice 0 0 1.0 16)))
      (should (equal (car body-display) '(slice 0 16 1.0 16)))
      (should (equal rest-display '(space :width (32))))
      (should (= 32 (plist-get (cdr prepared-image) :width)))
      (should (= 32 (plist-get (cdr prepared-image) :height))))))

(ert-deftest appkit-chat-avatar-line-height-uses-remapped-font-metrics-once ()
  (let ((line-spacing 7)
        (text-scale-mode-amount 3)
        (text-scale-mode-step 1.2))
    (cl-letf (((symbol-function 'appkit-geometry-display-window)
               (lambda (&rest _arguments) nil))
              ((symbol-function 'default-line-height) (lambda () 35))
              ((symbol-function 'line-pixel-height) (lambda () 99)))
      (should (= 35 (appkit-chat-avatar-line-pixel-height))))))

(ert-deftest appkit-chat-avatar-geometry-preserves-render-point ()
  (with-temp-buffer
    (insert "timeline row still being rendered")
    (goto-char 7)
    (let ((origin (point)))
      (cl-letf (((symbol-function 'appkit-geometry-display-window)
                 (lambda (&rest _arguments) 'chat-window))
                ((symbol-function 'window-font-height)
                 (lambda (&rest _args)
                   (goto-char (point-max))
                   20))
                ((symbol-function 'window-font-width)
                 (lambda (&rest _args)
                   (goto-char (point-max))
                   10))
                ((symbol-function 'window-frame) (lambda (_window) nil))
                ((symbol-function 'frame-parameter) (lambda (&rest _args) nil)))
        (should (= 20 (appkit-chat-avatar-line-pixel-height)))
        (should (= origin (point)))
        (should (= 10 (appkit-chat-avatar-column-pixel-width)))
        (should (= origin (point)))))))

(ert-deftest appkit-chat-avatar-pixel-spacer-keeps-tty-fallback ()
  (cl-letf (((symbol-function 'display-graphic-p)
             (lambda (&optional _frame) nil)))
    (let ((spacer (appkit-chat-avatar--pixel-spacer-string 4 32)))
      (should (= 4 (string-width spacer)))
      (should-not (get-text-property 0 'display spacer)))))

(ert-deftest appkit-chat-avatar-image-width-uses-remapped-column-metrics ()
  (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
             (lambda (_image) t))
            ((symbol-function 'image-size)
             (lambda (&rest _args) (cons 60 60)))
            ((symbol-function 'appkit-chat-avatar-column-pixel-width)
             (lambda () 13)))
    (should (= 5
               (appkit-chat-avatar-image-char-width
                '(image :type png :data "avatar"))))))


(ert-deftest appkit-chat-avatar-resize-normalizes-both-axes ()
  (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
             (lambda (_image) t)))
    (let ((image (appkit-chat-avatar-resize-image
                  '(image :type png :width 80 :height 40) 32)))
      (should (= 32 (plist-get (cdr image) :width)))
      (should (= 32 (plist-get (cdr image) :height))))))

(provide 'appkit-chat-avatar-test)
;;; appkit-chat-avatar-test.el ends here
