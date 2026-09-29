;;; appkit-media-inline-test.el --- Buffer-owned media occurrence tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-media-inline)

(defun appkit-media-inline-test--poster ()
  "Return a two-row inert poster for an inline host."
  '(image :type png :width 80 :height 20 :appkit-media-nslices 2))

(defun appkit-media-inline-test--insert (kind &rest keys)
  "Insert a two-slice poster and attach a KIND host with KEYS."
  (let ((start (point))
        (poster (appkit-media-inline-test--poster)))
    (insert (car (appkit-media-image-slice-rows poster)) "\n"
            (cadr (appkit-media-image-slice-rows poster)))
    (apply #'appkit-media-inline-host-attach
           start (point) poster '((url . "https://example.invalid/media"))
           :kind kind keys)))

(ert-deftest appkit-media-inline-styling-preserves-host-until-text-replacement ()
  (with-temp-buffer
    (let* ((poster (appkit-media-inline-test--poster))
           (start (point))
           host)
      (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
                 (lambda (&rest _) t))
                ((symbol-function 'appkit-media--char-pixel-height)
                 (lambda () 10)))
        (insert (car (appkit-media-image-slice-rows poster)) "\n"
                (cadr (appkit-media-image-slice-rows poster)))
        (setq host (appkit-media-inline-host-attach
                    start (point) poster nil))
        (should (eq host (appkit-media-inline-host-at-point start)))
        (should (eq host (appkit-media-inline-host-at-point (+ start 2))))
        (add-text-properties start (point) '(face shadow message-id "42"))
        (should (appkit-media-inline-host-live-p host))
        (goto-char (+ start 2))
        (delete-char 1)
        (insert "x")
        (should-not (appkit-media-inline-host-live-p host))
        (should-not (appkit-media-inline-host-at-point (+ start 2)))
        (should-not (appkit-media-inline-host-ranges host))))))

(ert-deftest appkit-media-inline-canvas-slices-preserve-row-geometry ()
  (with-temp-buffer
    (let ((poster (appkit-media-inline-test--poster)) host)
      (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
                 (lambda (&rest _) t))
                ((symbol-function 'appkit-media--char-pixel-height)
                 (lambda () 10)))
        (let ((start (point)))
          (insert (car (appkit-media-image-slice-rows poster)) "\n"
                  (cadr (appkit-media-image-slice-rows poster)))
          (setq host (appkit-media-inline-host-attach
                      start (point) poster nil)))
        (let ((canvas '(image :type canvas :width 80 :height 20)))
          (appkit-media--inline-show-canvas host nil canvas)
          (let* ((ranges (appkit-media-inline-host-ranges host))
                 (first (get-text-property (caar ranges) 'display))
                 (second (get-text-property (caadr ranges) 'display)))
            (should (eq (cadr first) canvas))
            (should (eq (cadr second) canvas))
            (should (equal (car first) '(slice 0 0 1.0 10)))
            (should (equal (car second) '(slice 0 10 1.0 10)))
            (should (= (plist-get (cdr canvas) :height) 20))
            (should (appkit-media-inline-host-live-p host))))))))

(ert-deftest appkit-media-inline-unsliced-canvas-keeps-full-image-height ()
  (with-temp-buffer
    (let* ((poster '(image :type png :width 64 :height 64))
           (canvas '(image :type canvas :width 64 :height 64))
           (start (point)))
      (insert (propertize "[face]" 'display poster))
      (let ((host (appkit-media-inline-host-attach start (point) poster nil)))
        (appkit-media--inline-show-canvas host nil canvas)
        (should (eq (get-text-property start 'display) canvas))
        (should (= (plist-get (cdr canvas) :height) 64))))))

(ert-deftest appkit-media-inline-promotion-retains-exact-session-and-releases-lease ()
  (with-temp-buffer
    (let (host session inline presented closed)
      (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
                 (lambda (&rest _) t))
                ((symbol-function 'appkit-media--char-pixel-height)
                 (lambda () 10))
                ((symbol-function 'appkit-media--inline-canvas-p)
                 (lambda (_host) t))
                ((symbol-function 'appkit-media-video-session-create)
                 (lambda (&rest _) (setq session (list 'session))))
                ((symbol-function 'appkit-media-video-inline-create)
                 (lambda (candidate &rest _)
                   (should (eq candidate session))
                   (setq inline (list 'inline))))
                ((symbol-function 'appkit-media-video-inline-closed-p)
                 (lambda (_inline) nil))
                ((symbol-function 'appkit-media-video-inline-bind-controls)
                 (lambda (&rest _) nil))
                ((symbol-function 'appkit-media-present-video-inline)
                 (lambda (candidate &rest _)
                   (setq presented candidate) 'viewer))
                ((symbol-function 'appkit-media-video-inline-close)
                 (lambda (candidate) (setq closed candidate))))
        (setq host (appkit-media-inline-test--insert 'image))
        (should (eq (appkit-media-inline-host-activate host 'dedicated)
                    'viewer))
        (should (eq presented inline))
        (delete-region (point-min) (point-max))
        (should (eq closed inline))
        (should-not (appkit-media-inline-host-live-p host))))))

(ert-deftest appkit-media-inline-adjacent-edits-preserve-exact-ownership ()
  (with-temp-buffer
    (let ((poster '(image :type png :width 10 :height 10)))
      (insert (propertize " " 'display poster))
      (let ((host (appkit-media-inline-host-attach 1 2 poster nil)))
        (goto-char 1)
        (insert "prefix\n")
        (should (appkit-media-inline-host-live-p host))
        (should (= (caar (appkit-media-inline-host-ranges host)) 8))
        (goto-char (point-max))
        (insert "\ndraft")
        (should (appkit-media-inline-host-live-p host))
        (should (= (cdar (appkit-media-inline-host-ranges host)) 9))
        (remove-text-properties 8 9 '(appkit-media-inline-token nil))
        (should-not (appkit-media-inline-host-live-p host))
        (should-not appkit-media--inline-hosts)
        (should-not (appkit-media-inline-host-ranges host))))))


(ert-deftest appkit-media-inline-stale-resolution-is-cancelled-and-ignored ()
  (with-temp-buffer
    (let (host success failure cancelled started)
      (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
                 (lambda (&rest _) t))
                ((symbol-function 'appkit-media--char-pixel-height)
                 (lambda () 10))
                ((symbol-function 'appkit-media--inline-canvas-p)
                 (lambda (_host) t))
                ((symbol-function 'appkit-media--inline-ensure)
                 (lambda (&rest _)
                   (setq started t))))
        (let* ((poster (appkit-media-inline-test--poster))
               (begin (point)))
          (insert (car (appkit-media-image-slice-rows poster)) "\n"
                  (cadr (appkit-media-image-slice-rows poster)))
          (setq host
                (appkit-media-inline-host-attach
                 begin (point) poster nil
                 :resolve-function
                 (lambda (ready reject)
                   (setq success ready failure reject)
                   (lambda () (setq cancelled t)))))
          (appkit-media-inline-host-activate host)
          (should success)
          (delete-region begin (point-max))
          (should cancelled)
          (funcall success '((url . "https://example.invalid/stale")))
          (funcall failure "late error")
          (should-not started)
          (should-not (appkit-media-inline-host-resource host)))))))

(ert-deftest appkit-media-inline-owner-retirement-cancels-before-resolver-callback ()
  (with-temp-buffer
    (let (host release success started
          (fake-handle (appkit-handle--create :alive-p t)))
      (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
                 (lambda (&rest _) t))
                ((symbol-function 'appkit-media--char-pixel-height)
                 (lambda () 10))
                ((symbol-function 'appkit-media--inline-canvas-p)
                 (lambda (_host) t))
                ((symbol-function 'appkit-surface-live-p)
                 (lambda (surface) (eq surface 'owner)))
                ((symbol-function 'appkit-current-surface)
                 (lambda () 'owner))
                ((symbol-function 'appkit-register-handle)
                 (lambda (_owner _type callback)
                   (setq release callback) fake-handle))
                ((symbol-function 'appkit-media--inline-ensure)
                 (lambda (&rest _) (setq started t))))
        (let* ((poster (appkit-media-inline-test--poster))
               (start (point)))
          (insert (car (appkit-media-image-slice-rows poster)) "\n"
                  (cadr (appkit-media-image-slice-rows poster)))
          (setq host
                (appkit-media-inline-host-attach
                 start (point) poster nil :owner 'owner
                 :resolve-function
                 (lambda (ready _failure)
                   (setq success ready)
                   (lambda ()
                     (funcall ready
                              '((url . "https://example.invalid/late")))))))
          (appkit-media-inline-host-activate host)
          (should success)
          (funcall release)
          (should-not (appkit-handle-alive-p fake-handle))
          (should-not (appkit-media-inline-host-live-p host))
          (should-not (appkit-media-inline-host-resource host))
          (should-not started))))))

(provide 'appkit-media-inline-test)
;;; appkit-media-inline-test.el ends here
