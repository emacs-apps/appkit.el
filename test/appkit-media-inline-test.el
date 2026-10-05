;;; appkit-media-inline-test.el --- Buffer-owned media occurrence tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-media-inline)
(require 'button)
(require 'appkit-chat-ins)
(require 'appkit-projection)
(require 'appkit-app)

(defun appkit-media-inline-test--image (&rest options)
  "Prepare an inert poster through the public image API with OPTIONS."
  (apply #'appkit-media-image-with-resource
         '(image :type png :width 80 :height 20 :appkit-media-nslices 2)
         nil options))

(defun appkit-media-inline-test--insert (image &optional slices)
  "Insert prepared IMAGE, optionally as SLICES, and return its mounted host."
  (let ((start (point)))
    (if slices
        (appkit-media-insert-image-slices image)
      (insert (appkit-media-image-display-string image "[image]")))
    (appkit-media-inline-mount start (point))
    (appkit-media-inline-host-at-point start)))

(defconst appkit-media-inline-test--surface-type
  (appkit-surface-type-create
   :name 'appkit-inline-test :mode #'fundamental-mode
   :init (lambda (_context input)
           (appkit-next :model input
                        :render (appkit-projection-change-create :full-p t :frame-p t)))
   :update (lambda (_context _model message)
             (appkit-next :model message
                          :render (appkit-projection-change-create :full-p t :frame-p t)))
   :renderer-factory
   (lambda (_surface)
     (appkit-projection-renderer-create
      :project-all
      (lambda (_surface _app model)
        (when-let* ((body (plist-get model :body)))
          (list (appkit-projection-row-create :key 'row :payload body))))
      :project-frame
      (lambda (_surface _app model)
        (cons (or (plist-get model :header) "")
              (or (plist-get model :footer) "")))
      :printer (lambda (_surface _app row)
                 (insert (appkit-projection-row-payload row)))
      :anchor-property 'row :no-separator-p t))))

(defmacro appkit-media-inline-test--with-surface (model &rest body)
  "Run BODY with a real app-owned `surface' displaying MODEL."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (let ((app (appkit-app-start
                 (appkit-app-type-create
                  :name 'appkit-inline-test
                  :init (lambda (_context _input)
                          (appkit-next :model nil :render appkit-render-none))
                  :update (lambda (_context _model _message)
                            (appkit-next-reject 'unsupported))
                  :shutdown #'ignore)
                 :identity (make-symbol "inline-test")))
           surface)
       (unwind-protect
           (progn
             (setq surface
                   (appkit-open-generated-surface
                    appkit-media-inline-test--surface-type
                    :app app :identity 'media :input ,model
                    :buffer (current-buffer)))
             ,@body)
         (when (and surface (appkit-surface-alive-p surface))
           (appkit-surface-stop surface))
         (appkit-app-close app)))))

(ert-deftest appkit-media-inline-passive-emoji-preserves-reaction-button ()
  (with-temp-buffer
    (let ((poster (appkit-media-inline-test--image :animated t :interaction nil))
          (reactions 1)
          (last-command-event ?\r))
      (appkit-chat-ins-insert-reaction-line
       '(wave)
       :label-function
       (lambda (_)
         (concat "[" (appkit-media-image-display-string poster ":wave:") " 1]"))
       :action-function (lambda (_) (cl-incf reactions)))
      (appkit-media-inline-mount (point-min) (point-max))
      (let ((host (appkit-media-inline-host-at-point 2))
            (source (buffer-string)))
        (should host)
        (goto-char 3)
        (call-interactively
         (lookup-key (get-char-property (point) 'keymap) (kbd "RET")))
        (should (= reactions 2))
        (appkit-media--inline-show-canvas
         host nil '(image :type canvas :width 80 :height 20))
        (call-interactively
         (lookup-key (get-char-property (point) 'keymap) (kbd "RET")))
        (should (= reactions 3))
        (should (equal-including-properties source (buffer-string)))))))

(ert-deftest appkit-media-inline-styling-preserves-host-until-text-replacement ()
  (with-temp-buffer
    (let* ((host (appkit-media-inline-test--insert
                  (appkit-media-inline-test--image)))
           (end (point)))
      (add-text-properties 1 end '(face shadow message-id "42"))
      (should (appkit-media-inline-host-live-p host))
      (goto-char 3)
      (delete-char 1)
      (insert "x")
      (should-not (appkit-media-inline-host-live-p host))
      (should-not (appkit-media-inline-host-at-point 1)))))

(ert-deftest appkit-media-inline-canvas-slices-preserve-row-geometry ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
               (lambda (&rest _) t))
              ((symbol-function 'appkit-media--char-pixel-height)
               (lambda () 10)))
      (let* ((rows (appkit-media-image-slice-rows
                    (appkit-media-inline-test--image)))
             (canvas '(image :type canvas :width 80 :height 20)))
        ;; A layout may put unrelated text between slices of the same image.
        (insert (car rows) " caption\n" (cadr rows))
        (let ((host (car (appkit-media-inline-mount 1 (point-max))))
              (source (buffer-string)))
          (should (eq host (appkit-media-inline-host-at-point (1- (point-max)))))
          (appkit-media--inline-show-canvas host nil canvas)
          (should (equal (get-char-property 1 'display)
                         (list '(slice 0 0 1.0 10) canvas)))
          (should (equal (get-char-property (1- (point-max)) 'display)
                         (list '(slice 0 10 1.0 10) canvas)))
          (should (equal-including-properties source (buffer-string)))
          (delete-region (1- (point-max)) (point-max))
          (should-not (appkit-media-inline-host-at-point 1)))))))

(ert-deftest appkit-media-inline-unsliced-canvas-keeps-full-image-height ()
  (with-temp-buffer
    (let* ((image (appkit-media-image-with-resource
                   '(image :type png :width 64 :height 64) nil))
           (host (appkit-media-inline-test--insert image))
           (canvas '(image :type canvas :width 64 :height 64)))
      (appkit-media--inline-show-canvas host nil canvas)
      (should (equal (get-char-property 1 'display)
                     (list '(slice 0 0 1.0 1.0) canvas))))))

(ert-deftest appkit-media-inline-visibility-rejects-hidden-and-iconified-frames ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let ((host (appkit-media-inline-test--insert
                   (appkit-media-inline-test--image :kind 'video)))
            (visibility t))
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'pos-visible-in-window-p) (lambda (&rest _) t))
                  ((symbol-function 'frame-visible-p) (lambda (_) visibility)))
          (should (eq (appkit-media--inline-visible-p host) (selected-window)))
          (setq visibility nil)
          (should-not (appkit-media--inline-visible-p host))
          (setq visibility 'icon)
          (should-not (appkit-media--inline-visible-p host)))))))

(ert-deftest appkit-media-inline-target-tracks-scaled-poster-geometry ()
  "Text-scaled slices and their native viewport retain the same aspect."
  (skip-unless (and (display-graphic-p)
                    (image-type-available-p 'canvas)
                    (image-type-available-p 'png)))
  (require 'face-remap)
  (let ((file (make-temp-file "appkit-inline-geometry-" nil ".png"))
        (original-players (copy-sequence video--players)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region
             (base64-decode-string
              "iVBORw0KGgoAAAANSUhEUgAAAAIAAAABCAIAAAB7QOjdAAAADUlEQVR4nGP4zwAE/wEHAAH/4iOeWQAAAABJRU5ErkJggg==")
             nil file nil 'silent))
          (save-window-excursion
            (with-temp-buffer
              (switch-to-buffer (current-buffer))
              (dolist (rows '(2 1))
                (let ((image (appkit-media-image-with-resource
                              (create-image file 'png nil
                                            :height (cons rows 'ch)
                                            :appkit-media-nslices rows :scale 1.0)
                              (appkit-media-resource-create :file file))))
                  (dolist (scale '(0 3 -2 0))
                    (text-scale-set scale)
                    (erase-buffer)
                    (let* ((height (* rows (appkit-media--char-pixel-height)))
                           (host (appkit-media-inline-test--insert image t))
                           (lease (appkit-media--inline-ensure host))
                           (inline (appkit-media-video-inline-inline lease)))
                      (video-inline-prepare inline)
                      (let ((target (video-inline-target inline)))
                        (should (= (video-target-width target) (* 2 height)))
                        (should (= (video-target-height target) height))
                        (should (equal (image-size (video-target-canvas target) t)
                                       (cons (* 2 height) height))))))))
              (erase-buffer)
              (text-scale-set 3)
              (let* ((image (appkit-media-image-with-resource
                             (create-image file 'png nil :height 64 :scale 1.0)
                             (appkit-media-resource-create :file file)))
                     (host (appkit-media-inline-test--insert image))
                     (lease (appkit-media--inline-ensure host))
                     (inline (appkit-media-video-inline-inline lease)))
                (video-inline-prepare inline)
                (should (equal
                         (image-size (video-target-canvas
                                      (video-inline-target inline)) t)
                         '(128 . 64)))))))
      (dolist (player (copy-sequence video--players))
        (unless (memq player original-players) (video-player-close player)))
      (delete-file file))))

(ert-deftest appkit-media-inline-adjacent-edits-preserve-exact-ownership ()
  (with-temp-buffer
    (let ((host (appkit-media-inline-test--insert
                 (appkit-media-inline-test--image))))
      (goto-char 1)
      (insert "prefix\n")
      (should (eq host (appkit-media-inline-host-at-point 8)))
      (should-not (appkit-media-inline-host-at-point 7))
      (goto-char (point-max))
      (insert "\ndraft")
      (should (appkit-media-inline-host-live-p host))
      (should-not (appkit-media-inline-host-at-point (- (point-max) 6)))
      (fundamental-mode)
      (should-not (appkit-media-inline-host-live-p host)))))

(ert-deftest appkit-media-inline-property-only-retirement-removes-old-actions ()
  (dolist (replacement '(removed passive))
    (with-temp-buffer
      (let ((host (appkit-media-inline-test--insert
                   (appkit-media-inline-test--image))))
        (with-silent-modifications
          (if (eq replacement 'removed)
              (remove-text-properties 1 (point-max) '(appkit-media-occurrence nil))
            (let ((text (appkit-media-image-display-string
                         (appkit-media-inline-test--image :interaction nil)
                         "[image]")))
              (set-text-properties 1 (point-max) (text-properties-at 0 text)))))
        (appkit-media-inline-mount 1 (point-max))
        (should-not (appkit-media-inline-host-live-p host))
        (should-not (appkit-media-inline-host-at-point 1))
        (should-not (get-char-property 1 'keymap))))))

(ert-deftest appkit-media-inline-stale-resolution-is-cancelled-and-ignored ()
  (with-temp-buffer
    (let (success failure cancelled)
      (let ((host
             (appkit-media-inline-test--insert
              (appkit-media-inline-test--image
               :resolve-function
               (lambda (ready reject)
                 (setq success ready failure reject)
                 (lambda () (setq cancelled t)))))))
        (cl-letf (((symbol-function 'appkit-media--inline-canvas-p) (lambda (_) t)))
          (appkit-media-inline-host-activate host))
        (delete-region 1 (point-max))
        (should cancelled)
        (funcall success '((url . "https://example.invalid/stale")))
        (funcall failure "late error")
        (should-not (appkit-media-inline-host-resource host))
        (should-not (appkit-media-inline-host-live-p host))))))

(ert-deftest appkit-media-inline-deferred-animation-directive-selects-presentation ()
  (pcase-dolist (`(,directive ,detected ,presentation ,expected)
                 '((t nil nil inline) (nil t nil viewer)
                   (auto t nil inline) (auto nil nil viewer)
                   (t nil inline inline)))
    (with-temp-buffer
      (let* (resolve actions
             (host
              (appkit-media-inline-test--insert
               (appkit-media-inline-test--image
                :animated directive
                :resolve-function (lambda (ready _reject) (setq resolve ready) nil))))
             (lease (appkit-media--video-inline-create :inline (video--make-inline))))
        ;; Isolate native presentation effects, not source or ownership logic.
        (cl-letf (((symbol-function 'appkit-media--file-animated-p)
                   (lambda (_) detected))
                  ((symbol-function 'appkit-media--inline-ensure) (lambda (_) lease))
                  ((symbol-function 'appkit-media-video-inline-play)
                   (lambda (_) (push 'inline actions)))
                  ((symbol-function 'appkit-media-present-video-inline)
                   (lambda (&rest _) (push 'viewer actions))))
          (appkit-media--inline-use host presentation)
          (funcall resolve '((file . "/resolved/media")))
          (should (equal actions (list expected))))))))

(ert-deftest appkit-media-inline-reused-string-has-independent-occurrences ()
  (with-temp-buffer
    (let ((text (appkit-media-image-display-string
                 (appkit-media-inline-test--image) ":x:")))
      (insert text text)
      (let ((hosts (appkit-media-inline-mount (point-min) (point-max))))
        (should (= (length hosts) 2))
        (should-not (eq (car hosts) (cadr hosts)))
        (appkit-media-inline-mount (point-min) (point-max))
        (should (eq (car hosts) (appkit-media-inline-host-at-point 1)))
        (delete-region 1 4)
        (should-not (appkit-media-inline-host-live-p (car hosts)))
        (should (eq (cadr hosts) (appkit-media-inline-host-at-point 1)))))))

(ert-deftest appkit-media-inline-copied-input-does-not-borrow-runtime-state ()
  (let ((first (generate-new-buffer " *appkit-media-source*"))
        (second (generate-new-buffer " *appkit-media-destination*"))
        original copied)
    (unwind-protect
        (let ((text (appkit-media-image-display-string
                     (appkit-media-inline-test--image) ":x:")))
          (with-current-buffer first
            (insert text)
            (setq original (car (appkit-media-inline-mount 1 (point-max))))
            (appkit-media--inline-show-canvas
             original nil '(image :type canvas :width 80 :height 20))
            (setq copied (buffer-string))
            (should (equal-including-properties text copied)))
          (with-current-buffer second
            (insert copied)
            (let ((destination (car (appkit-media-inline-mount 1 (point-max)))))
              (should-not (eq original destination))
              (kill-buffer first)
              (should-not (appkit-media-inline-host-live-p original))
              (should (appkit-media-inline-host-live-p destination)))))
      (when (buffer-live-p first) (kill-buffer first))
      (when (buffer-live-p second) (kill-buffer second)))))

(ert-deftest appkit-media-inline-silent-ewoc-deletion-cancels-resolution ()
  (let* (resolve cancelled
         (image (appkit-media-inline-test--image
                 :resolve-function
                 (lambda (ready _reject)
                   (setq resolve ready)
                   (lambda () (setq cancelled t)))))
         (text (appkit-media-image-display-string image "preview")))
    (appkit-media-inline-test--with-surface (list :body text)
      (let ((host (appkit-media-inline-host-at-point (point-min))))
        (cl-letf (((symbol-function 'appkit-media--inline-canvas-p) (lambda (_) t)))
          (appkit-media-inline-host-activate host))
        (appkit-surface-send surface nil)
        (should cancelled)
        (should-not (appkit-media-inline-host-live-p host))
        (should-not appkit-media--inline-hosts)
        (funcall resolve '((url . "https://example.invalid/late")))
        (should-not (appkit-media-inline-host-resource host))))))

(ert-deftest appkit-media-inline-initial-surface-mount-keeps-owner-fence ()
  (let ((text (appkit-media-image-display-string
               (appkit-media-inline-test--image) "[image]")))
    (appkit-media-inline-test--with-surface (list :body text)
      (let ((host (appkit-media-inline-host-at-point (point-min))))
        (should (appkit-media-inline-host-live-p host))
        (should (eq (appkit-media-inline-host-owner host) surface))
        (appkit-surface-stop surface)
        (should (buffer-live-p (current-buffer)))
        (should-not (appkit-media-inline-host-live-p host))))))

(ert-deftest appkit-media-inline-owner-retirement-fences-reentrant-resolution ()
  (let* (resolve
         (image (appkit-media-inline-test--image
                 :resolve-function
                 (lambda (ready _reject)
                   (setq resolve ready)
                   (lambda ()
                     (funcall ready '((url . "https://example.invalid/late"))))))))
    (appkit-media-inline-test--with-surface
        (list :body (appkit-media-image-display-string image "preview"))
      (let ((host (appkit-media-inline-host-at-point (point-min))))
        (cl-letf (((symbol-function 'appkit-media--inline-canvas-p) (lambda (_) t)))
          (appkit-media-inline-host-activate host))
        (should resolve)
        (appkit-surface-stop surface)
        (should-not (appkit-media-inline-host-live-p host))
        (should-not (appkit-media-inline-host-resource host))))))

(ert-deftest appkit-media-inline-header-only-render-is-mounted-and-retired ()
  (let ((text (appkit-media-image-display-string
               (appkit-media-inline-test--image) "[image]")))
    (appkit-media-inline-test--with-surface (list :header text)
      (let ((host (appkit-media-inline-host-at-point (point-min))))
        (should (appkit-media-inline-host-live-p host))
        (appkit-surface-send surface (list :footer text))
        (should-not (appkit-media-inline-host-live-p host))
        (should (appkit-media-inline-host-live-p
                 (appkit-media-inline-host-at-point (point-min))))))))

(ert-deftest appkit-media-inline-failed-initial-presentation-releases-handles ()
  (with-temp-buffer
    (let* ((app (appkit-app-start
                 (appkit-app-type-create
                  :name 'appkit-inline-test
                  :init (lambda (_context _input)
                          (appkit-next :model nil :render appkit-render-none))
                  :update (lambda (_context _model _message)
                            (appkit-next-reject 'unsupported))
                  :shutdown #'ignore)
                 :identity (make-symbol "inline-failure")))
           cancelled host
           (image (appkit-media-inline-test--image
                   :resolve-function
                   (lambda (_ready _reject)
                     (lambda () (setq cancelled t)))))
           (complete (symbol-function 'appkit-media--render-complete)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'appkit-media--render-complete)
                       (lambda (owner start end success)
                         (funcall complete owner start end success)
                         (when success
                           (setq host (appkit-media-inline-host-at-point start))
                           (appkit-media--inline-use host nil)
                           (error "Presentation failed after acquiring a handle")))))
              (should-error
               (appkit-open-generated-surface
                appkit-media-inline-test--surface-type
                :app app :identity 'media :buffer (current-buffer)
                :input (list :body (appkit-media-image-display-string image "[image]")))))
            ;; Check before buffer death or app shutdown could hide a leak.
            (should (buffer-live-p (current-buffer)))
            (should cancelled)
            (should-not (appkit-media-inline-host-live-p host))
            (should-not appkit-media--inline-hosts))
        (appkit-app-close app)))))

(provide 'appkit-media-inline-test)
;;; appkit-media-inline-test.el ends here
