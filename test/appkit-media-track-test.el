;;; appkit-media-track-test.el --- Prepared track behavior tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-media-track)
(require 'appkit-media-inline-test)

(defmacro appkit-media-track-test--with-images (&rest body)
  "Run BODY with real strip geometry and deterministic image decoding."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'image-type-available-p) (lambda (_) t))
             ((symbol-function 'file-readable-p) (lambda (_) t))
             ((symbol-function 'appkit-media-image-mime-type) (lambda (_) "image/png"))
             ((symbol-function 'svg-embed) #'ignore)
             ((symbol-function 'svg-image)
              (lambda (_svg &rest properties) (cons 'image (cons :type (cons 'svg properties)))))
             ((symbol-function 'appkit-media--char-pixel-height) (lambda () 10))
             ((symbol-function 'appkit-media--base-char-pixel-height) (lambda () 10)))
     ,@body))

(defun appkit-media-track-test--image (&rest options)
  "Prepare a three-item track through its public API with OPTIONS."
  (apply #'appkit-media-track-image
         '((:file "/preview-a.png" :display-width 40 :playable-p t)
           (:file "/preview-b.png" :display-width 70 :playable-p t)
           (:file "/preview-c.png" :display-width 30))
         20 3 options))

(defun appkit-media-track-test--insert (image)
  "Insert IMAGE's authored slices and return its explicitly mounted track."
  (let ((start (point)))
    (insert (mapconcat #'identity (appkit-media-image-slice-rows image) "\n"))
    (appkit-media-inline-mount start (point))
    (appkit-media-track-at-point start)))

(ert-deftest appkit-media-track-preparation-without-previews-is-inert ()
  (with-temp-buffer
    (should-not (appkit-media-track-image '((:playable-p t)) 20 3))
    (appkit-media-track-test--with-images
      (let* ((item (list :file "/preview.png" :display-width 40 :id 'foreign))
             (image (appkit-media-track-image (list item) 20 3)))
        (should image)
        (should-not appkit-media--inline-hosts)
        (should (eq (plist-get item :id) 'foreign))
        (should (eq (cadr (car (plist-get (cdr image) :map))) 'appkit-media-track-0))
        (should-not (get-text-property
                     0 'keymap (appkit-media-image-display-string image "preview")))))))

(ert-deftest appkit-media-track-repeated-cached-slices-select-independently ()
  (with-temp-buffer
    (appkit-media-track-test--with-images
      (let* ((image (appkit-media-track-test--image))
             (text (mapconcat #'identity (appkit-media-image-slice-rows image) "\n"))
             (snapshot (copy-tree image t)))
        (insert text "\n" text)
        (let* ((source (buffer-string))
               (hosts (appkit-media-inline-mount 1 (point-max)))
               (first (appkit-media-track-at-point 1))
               (second (appkit-media-track-at-point 5)))
          (should (= (length hosts) 2))
          (should (appkit-media-track-p first))
          (should-not (eq first second))
          (should (eq first (appkit-media-track-at-point 3)))
          (appkit-media-track-select first 1)
          (should (= (appkit-media-track-index first) 1))
          (should (= (appkit-media-track-index second) 0))
          (should (= (plist-get (cdr (cadr (get-char-property 1 'display)))
                                :appkit-media-strip-offset) 43))
          (should (= (plist-get (cdr (cadr (get-char-property 5 'display)))
                                :appkit-media-strip-offset) 0))
          (should (equal (car (get-char-property 3 'display)) '(slice 0 10 1.0 10)))
          (appkit-media-track-select first -2)
          (should (= (appkit-media-track-index first) 2))
          (should (equal-including-properties source (buffer-string)))
          (should (equal image snapshot))
          (delete-region 1 4)
          (should-not (appkit-media-inline-host-live-p (car hosts)))
          (should (= (appkit-media-track-index second) 0)))))))

(ert-deftest appkit-media-track-hotspot-uses-event-occurrence-not-point ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (appkit-media-track-test--with-images
        (let* (opened
               (image (appkit-media-track-test--image
                       :open-function
                       (lambda (index inline presentation)
                         (push (list index inline presentation) opened))))
               (first (appkit-media-track-test--insert image)))
          (insert "\n")
          (let* ((second (appkit-media-track-test--insert image))
                 (event (list 'mouse-1 (list (selected-window) 5 '(0 . 0) 0)))
                 (command (lookup-key (get-char-property 5 'keymap)
                                      [appkit-media-track-2 mouse-1])))
            (goto-char 1)
            (funcall command event)
            (should (equal opened '((2 nil nil))))
            (should (= (appkit-media-track-index first) 0))
            (should (= (appkit-media-track-index second) 2))
            (should (= (plist-get (cdr (cadr (get-char-property 5 'display)))
                                  :appkit-media-strip-offset) 0))))))))

(ert-deftest appkit-media-track-selection-failure-keeps-current-lease ()
  (with-temp-buffer
    (appkit-media-track-test--with-images
      (let* ((track (appkit-media-track-test--insert (appkit-media-track-test--image)))
             (host (appkit-media-inline-host-at-point 1))
             (display (get-char-property 1 'display))
             closed)
        (setf (appkit-media-inline-host-inline host) 'original-lease)
        (cl-letf (((symbol-function 'appkit-media-horizontal-strip-image) #'ignore)
                  ((symbol-function 'appkit-media-video-inline-close)
                   (lambda (lease) (push lease closed))))
          (should-error (appkit-media-track-select track 1))
          (should-not closed)
          (should (= (appkit-media-track-index track) 0))
          (should (eq display (get-char-property 1 'display)))
          (should (eq (appkit-media-inline-host-inline host) 'original-lease))
          (setf (appkit-media-inline-host-inline host) nil))))))

(ert-deftest appkit-media-track-policy-decline-falls-back-with-exact-selection ()
  (with-temp-buffer
    (appkit-media-track-test--with-images
      (let* (requested opened
             (track
              (appkit-media-track-test--insert
               (appkit-media-track-test--image
                :session-function
                (lambda (index muted) (push (list index muted) requested) nil)
                :open-function
                (lambda (index inline presentation)
                  (push (list index inline presentation) opened))))))
        (goto-char 1)
        (appkit-media-track-select track 1)
        (appkit-media-track-toggle-muted)
        (cl-letf (((symbol-function 'appkit-media--inline-canvas-p) (lambda (_) t)))
          (appkit-media-track-activate track)
          (appkit-media-track-activate track 'frame)
          (appkit-media-track-activate track 'external))
        (should (equal requested '((1 t) (1 t))))
        (should (equal (nreverse opened) '((1 nil nil) (1 nil frame) (1 nil external))))
        (should-not (appkit-media-track-inline track))))))

(ert-deftest appkit-media-track-surface-commit-and-silent-retirement ()
  (appkit-media-track-test--with-images
    (let* ((image (appkit-media-track-test--image))
           (text (appkit-media-image-display-string image "preview")))
      (appkit-media-inline-test--with-surface (list :body text)
        (let ((track (appkit-media-track-at-point 1))
              (host (appkit-media-inline-host-at-point 1)))
          (should track)
          (should (eq (appkit-media-inline-host-owner host) surface))
          (appkit-media-track-select track 1)
          (appkit-surface-send surface nil)
          (should-not (appkit-media-track-at-point 1))
          (should-not (appkit-media-inline-host-live-p host))
          (should-error (appkit-media-track-activate track))
          (appkit-surface-send surface (list :body text))
          (should (= (appkit-media-track-index (appkit-media-track-at-point 1)) 0))
          (appkit-surface-stop surface)
          (should-not appkit-media--inline-hosts))))))

(ert-deftest appkit-media-track-unclaimed-session-is-closed-on-scene-failure ()
  (with-temp-buffer
    (appkit-media-track-test--with-images
      (let* ((session (appkit-media--video-session-create :video-session 'native))
             (track (appkit-media-track-test--insert
                     (appkit-media-track-test--image
                      :session-function (lambda (_index _muted) session))))
             (live t) (closes 0))
        (cl-letf (((symbol-function 'appkit-media--inline-canvas-p) (lambda (_) t))
                  ((symbol-function 'video-canvas-create)
                   (lambda (&rest _) (error "Cannot allocate scene")))
                  ((symbol-function 'appkit-media-video-session-live-p) (lambda (_) live))
                  ((symbol-function 'video-session-presentation-count) (lambda (_) 0))
                  ((symbol-function 'appkit-media-video-session-close)
                   (lambda (_) (cl-incf closes) (setq live nil))))
          (should-error (appkit-media-track-activate track))
          (should (= closes 1))
          (should-not (appkit-media-track-inline track)))))))

(ert-deftest appkit-media-track-session-callback-retirement-fences-creation ()
  (with-temp-buffer
    (appkit-media-track-test--with-images
      (let* ((session (appkit-media--video-session-create :video-session 'native))
             (track (appkit-media-track-test--insert
                     (appkit-media-track-test--image
                      :session-function (lambda (_index _muted)
                                          (erase-buffer) session))))
             (closes 0))
        (cl-letf (((symbol-function 'appkit-media--inline-canvas-p) (lambda (_) t))
                  ((symbol-function 'appkit-media-video-session-live-p) (lambda (_) t))
                  ((symbol-function 'video-session-presentation-count) (lambda (_) 0))
                  ((symbol-function 'appkit-media-video-session-close)
                   (lambda (_) (cl-incf closes))))
          (should-error (appkit-media-track-activate track))
          (should (= closes 1))
          (should-not appkit-media--inline-hosts))))))

(ert-deftest appkit-media-track-native-composite-preserves-presented-session ()
  "A real composite target retains authored slices and an independent viewer."
  (skip-unless (and (display-graphic-p)
                    (image-type-available-p 'canvas)
                    (image-type-available-p 'svg)
                    (image-type-available-p 'png)))
  (let ((file (make-temp-file "appkit-track-" nil ".png"))
        (players (copy-sequence video--players))
        viewer session)
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
              (let* ((image
                      (appkit-media-track-image
                       (list (list :file file :display-width 80 :playable-p t)
                             (list :file file :display-width 120))
                       40 4
                       :session-function
                       (lambda (_index muted)
                         (setq session
                               (appkit-media-video-session-create
                                (appkit-media-resource-create :file file)
                                "Track test" :kind 'image :muted muted)))
                       :open-function
                       (lambda (_index inline _presentation)
                         (setq viewer (appkit-media-present-video-inline inline "Track test")))))
                     (track (appkit-media-track-test--insert image))
                     (source (buffer-string))
                     (size (image-size (cadr (get-char-property 1 'display)) t)))
                (appkit-media-track-activate track)
                (let* ((inline (appkit-media-track-inline track))
                       (native (appkit-media-video-inline-inline inline)))
                  (video-inline-prepare native)
                  (let ((target (video-inline-target native)))
                    (should (= (video-target-canvas-width target) 204))
                    (should (= (video-target-destination-x target) 0))
                    (should (= (video-target-width target) 80))
                    (should (eq (cadr (get-char-property 1 'display))
                                (video-target-canvas target)))
                    (should (equal (image-size (video-target-canvas target) t) size)))
                  (appkit-media-track-activate track 'dedicated)
                  (should (= (video-session-presentation-count
                              (appkit-media-video-session-video-session session)) 2))
                  (appkit-media-track-select track 1)
                  (should (appkit-media-video-inline-closed-p inline))
                  (should (appkit-media-video-session-live-p session))
                  (should (= (video-session-presentation-count
                              (appkit-media-video-session-video-session session)) 1))
                  (should (equal-including-properties source (buffer-string)))
                  (erase-buffer)
                  (should (appkit-media-video-session-live-p session)))))))
      (when (buffer-live-p viewer) (kill-buffer viewer))
      (dolist (player (copy-sequence video--players))
        (unless (memq player players) (video-player-close player)))
      (delete-file file))))

(ert-deftest appkit-media-track-hotspot-touch-scrolls-without-activation ()
  "Native gestures over every mounted hotspot remain buffer scrolling."
  (require 'touch-screen)
  (let ((function-key-map (copy-keymap function-key-map)))
    (save-window-excursion
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (appkit-media-track-test--with-images
          (let* ((activations 0)
                 (image (appkit-media-track-test--image
                         :open-function (lambda (&rest _) (cl-incf activations))))
                 (track (appkit-media-track-test--insert image)))
            (goto-char 1)
            (dolist (entry (plist-get (cdr image) :map))
              (let ((id (cadr entry)))
                (dolist (dy '(-40 40))
                  (let* ((touch-screen-current-tool nil)
                         (touch-screen-aux-tool nil)
                         (touch-screen-current-timer nil)
                         (touch-screen-simple-mouse-conversion nil)
                         (touch-screen-extend-selection nil)
                         (start (cons 1 (list (selected-window) 1 '(20 . 100) 0)))
                         (end (cons 1 (list (selected-window) 1
                                            (cons 20 (+ 100 dy)) 0)))
                         (unread-command-events
                          (list id (list 'touchscreen-begin start)
                                (list 'touchscreen-update (list end))
                                (list 'touchscreen-end end nil) 0)))
                    (unwind-protect
                        (progn
                          (should (equal (read-key-sequence-vector nil)
                                         (vector (list 'touchscreen-scroll
                                                       (selected-window) 0 (- dy)))))
                          (should (equal (read-key-sequence-vector nil) [0]))
                          (should (= activations 0))
                          (should (= (appkit-media-track-index track) 0)))
                      (when touch-screen-current-timer
                        (cancel-timer touch-screen-current-timer)))))))))))))

(ert-deftest appkit-media-track-native-tap-activates-exact-item-unless-canceled ()
  "Native taps retain hotspot identity; canceled contacts never activate."
  (require 'touch-screen)
  (let ((function-key-map (copy-keymap function-key-map)))
    (save-window-excursion
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (appkit-media-track-test--with-images
          (let* (opened
                 (image (appkit-media-track-test--image
                         :open-function (lambda (index _inline _presentation)
                                          (push index opened))))
                 (track (appkit-media-track-test--insert image)))
            (goto-char 1)
            (setq buffer-read-only t)
            (cl-loop
             for entry in (plist-get (cdr image) :map)
             for index from 0
             do
             (dolist (canceled '(t nil))
               (let* ((touch-screen-current-tool nil)
                      (touch-screen-aux-tool nil)
                      (touch-screen-current-timer nil)
                      (touch-screen-simple-mouse-conversion nil)
                      (touch-screen-extend-selection nil)
                      (before opened)
                      (contact
                       (cons 1 (list (selected-window) (cadr entry) '(20 . 100)
                                     0 nil 1 '(0 . 0) image '(20 . 10) '(146 . 20)))))
                 (unwind-protect
                     (progn
                       (when-let*
                           ((event (catch 'input-event
                                     (touch-screen-handle-point-up
                                      contact (cadr entry) canceled)))
                            (command (key-binding
                                      (vector (cadr entry) (car event))
                                      t nil (cdr contact))))
                         (funcall command event))
                       (if canceled
                           (should (eq opened before))
                         (should (equal opened (cons index before)))
                         (should (= (appkit-media-track-index track) index))))
                   (when touch-screen-current-timer
                     (cancel-timer touch-screen-current-timer))))))))))))

(ert-deftest appkit-media-track-hotspot-play-failure-releases-only-new-lease ()
  "A clicked video targets its displayed cell and failure detaches its lease."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (appkit-media-track-test--with-images
        (let* ((session (appkit-media--video-session-create :video-session 'native))
               (track (appkit-media-track-test--insert
                       (appkit-media-track-test--image
                        :session-function (lambda (_index _muted) session))))
               (native (video--make-inline))
               (lease (appkit-media--video-inline-create :session session :inline native))
               (source (buffer-string))
               arguments close-function
               (closes 0) (session-closes 0))
          (cl-letf (((symbol-function 'appkit-media--inline-canvas-p) (lambda (_) t))
                    ((symbol-function 'video-canvas-create)
                     (lambda (&rest _) (list 'image :type 'canvas)))
                    ((symbol-function 'video-canvas-draw-source) #'ignore)
                    ((symbol-function 'canvas-refresh) #'ignore)
                    ((symbol-function 'appkit-media-video-inline-create)
                     (lambda (_session _width _height &rest options)
                       (setq arguments options
                             close-function (plist-get options :close-function))
                       lease))
                    ((symbol-function 'appkit-media-video-inline-bind-controls) #'ignore)
                    ((symbol-function 'appkit-media-video-inline-play)
                     (lambda (_) (error "Target preparation failed")))
                    ((symbol-function 'appkit-media-video-inline-close)
                     (lambda (inline)
                       (should (eq inline lease))
                       (cl-incf closes)
                       (setf (video-inline-closed native) t)
                       (funcall close-function inline)))
                    ((symbol-function 'appkit-media-video-session-close)
                     (lambda (_) (cl-incf session-closes))))
            (should-error
             (funcall (lookup-key (get-char-property 1 'keymap)
                                  [appkit-media-track-1 mouse-1])
                      (list 'mouse-1 (list (selected-window) 1 '(0 . 0) 0))))
            (should (= (plist-get arguments :destination-x) 43))
            (should (= (plist-get arguments :canvas-width) 146))
            (should (= (appkit-media-track-index track) 1))
            (should-not (appkit-media-track-inline track))
            (should (= closes 1))
            (should (= session-closes 0))
            (should (equal-including-properties source (buffer-string)))))))))

(provide 'appkit-media-track-test)
;;; appkit-media-track-test.el ends here
