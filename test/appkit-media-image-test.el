;;; appkit-media-image-test.el --- Tests for shared image rendering -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-media-image)

(ert-deftest appkit-media-inline-image-rendering-detects-supported-types ()
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
            ((symbol-function 'image-type-available-p)
             (lambda (type) (eq type 'gif))))
    (should (appkit-media-inline-image-rendering-available-p)))
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) nil))
            ((symbol-function 'image-type-available-p) (lambda (_type) t)))
    (should-not (appkit-media-inline-image-rendering-available-p))))

(ert-deftest appkit-media-image-object-valid-p-catches-image-errors ()
  (cl-letf (((symbol-function 'image-size)
             (lambda (image &rest _)
               (if (eq image 'valid)
                   '(10 . 10)
                 (error "invalid image")))))
    (should (appkit-media-image-object-valid-p 'valid))
    (should-not (appkit-media-image-object-valid-p 'invalid))
    (should-not (appkit-media-image-object-valid-p nil))))

(ert-deftest appkit-media-image-header-dimensions-handle-format-boundaries ()
  "Header sizing must preserve endian and packed-bit dimension boundaries."
  (let ((file (make-temp-file "appkit-image-header-")))
    (unwind-protect
        (dolist
            (sample
             '((((0 . (137 80 78 71 13 10 26 10))
                 (16 . (0 1 0 1 1 0 0 1)))
                (65537 . 16777217))
               (((0 . (71 73 70 56 57 97))
                 (6 . (1 16 3 8)))
                (4097 . 2051))
               (((0 . (82 73 70 70))
                 (8 . (87 69 66 80 86 80 56 88))
                 (24 . (0 0 16 0 0 32)))
                (1048577 . 2097153))
               (((0 . (82 73 70 70))
                 (8 . (87 69 66 80 86 80 56 76))
                 (20 . (47 255 63 0 8)))
                (16384 . 8193))
               (((0 . (82 73 70 70))
                 (8 . (87 69 66 80 86 80 56 32))
                 (23 . (157 1 42 255 255 1 128)))
                (16383 . 1))
               (((0 . (137 80 78 71 13 10 26 10))) nil)))
          (let ((bytes (make-string 32 0)))
            (dolist (part (car sample))
              (cl-loop for byte in (cdr part)
                       for index from (car part)
                       do (aset bytes index byte)))
            (with-temp-file file
              (set-buffer-multibyte nil)
              (insert bytes)))
          (should (equal (appkit-media--image-header-size file)
                         (cadr sample))))
      (delete-file file))))

(ert-deftest appkit-media-image-header-truncation-falls-back ()
  (let ((file (make-temp-file "appkit-short-image-")))
    (unwind-protect
        (dolist (bytes (list "GIF89a" (unibyte-string 137 80 78 71 13 10 26 10)
                            "RIFF\0\0\0\0WEBPVP8L" "not an image"))
          (with-temp-file file
            (set-buffer-multibyte nil)
            (insert bytes))
          (should-not (appkit-media--image-header-size file)))
      (delete-file file))))

(ert-deftest appkit-media-image-mime-prefers-file-header ()
  (cl-letf (((symbol-function 'file-readable-p) (lambda (_file) t))
            ((symbol-function 'image-type-from-file-header)
             (lambda (_file) 'jpeg))
            ((symbol-function 'image-supported-file-p)
             (lambda (_file) 'png)))
    (should
     (equal "image/jpeg"
            (appkit-media-image-mime-type "/tmp/misnamed.png")))))

(ert-deftest appkit-media-circular-image-clips-and-center-crops-source ()
  (let (circle-call embed-call image-properties)
    (cl-letf (((symbol-function 'file-readable-p) (lambda (_file) t))
              ((symbol-function 'image-type-available-p)
               (lambda (type) (eq type 'svg)))
              ((symbol-function 'appkit-media-image-mime-type)
               (lambda (_file) "image/jpeg"))
              ((symbol-function 'svg-create)
               (lambda (width height) (list :svg width height)))
              ((symbol-function 'svg-clip-path)
               (lambda (_svg &rest _properties) :clip))
              ((symbol-function 'svg-circle)
               (lambda (&rest arguments)
                 (setq circle-call arguments)))
              ((symbol-function 'svg-embed)
               (lambda (&rest arguments)
                 (setq embed-call arguments)))
              ((symbol-function 'svg-image)
               (lambda (_svg &rest properties)
                 (setq image-properties properties)
                 '(image :type svg :data "avatar")))
              ((symbol-function 'appkit-media-image-object-valid-p)
               (lambda (_image) t)))
      (should
       (equal '(image :type svg :data "avatar")
              (appkit-media-circular-image-from-file
               "/tmp/avatar.jpg" 21)))
      (should (equal circle-call '(:clip 10.5 10.5 10.5)))
      (should (equal (seq-take embed-call 4)
                     '((:svg 21 21) "/tmp/avatar.jpg" "image/jpeg" nil)))
      (should
       (equal (plist-get (nthcdr 4 embed-call) :preserveAspectRatio)
              "xMidYMid slice"))
      (should
       (equal (plist-get (nthcdr 4 embed-call) :clip-path)
              "url(#appkit-avatar-clip)"))
      (should (equal image-properties
                     '(:ascent center :width 21 :height 21))))))


(ert-deftest appkit-media-cropped-preview-fills-a-fixed-multiline-box ()
  "Cropped previews should cover one exact box with stable slice metadata."
  (let (rectangle-call embed-call image-properties)
    (cl-letf (((symbol-function 'file-readable-p) (lambda (_file) t))
              ((symbol-function 'image-type-available-p)
               (lambda (type) (eq type 'svg)))
              ((symbol-function 'appkit-media-image-mime-type)
               (lambda (_file) "image/jpeg"))
              ((symbol-function 'appkit-media--base-char-pixel-height)
               (lambda () 20))
              ((symbol-function 'svg-create)
               (lambda (width height) (list :svg width height)))
              ((symbol-function 'svg-clip-path)
               (lambda (_svg &rest _properties) :clip))
              ((symbol-function 'svg-rectangle)
               (lambda (&rest arguments)
                 (setq rectangle-call arguments)))
              ((symbol-function 'svg-embed)
               (lambda (&rest arguments)
                 (setq embed-call arguments)))
              ((symbol-function 'svg-image)
               (lambda (_svg &rest properties)
                 (setq image-properties properties)
                 '(image :type svg :data "thumbnail")))
              ((symbol-function 'appkit-media-image-object-valid-p)
               (lambda (_image) t)))
      (let ((image
             (appkit-media-cropped-preview-image-from-file
              "/tmp/source.jpg" 40 60)))
        (should (= 3 (plist-get (cdr image) :appkit-media-nslices)))
        (should (equal rectangle-call '(:clip 0 0 40 60)))
        (should
         (equal (seq-take embed-call 4)
                '((:svg 40 60) "/tmp/source.jpg" "image/jpeg" nil)))
        (should
         (equal
          (plist-get (nthcdr 4 embed-call) :preserveAspectRatio)
          "xMidYMid slice"))
        (should
         (equal
          (plist-get (nthcdr 4 embed-call) :clip-path)
          "url(#appkit-cropped-preview-clip)"))
        (should
         (equal image-properties
                '(:ascent center :height 60 :scale 1.0)))))))

(ert-deftest appkit-media-cropped-preview-keeps-transparent-insets ()
  "Insets should reserve montage gutters without changing the outer box."
  (let (rectangle-call embed-call image-properties)
    (cl-letf (((symbol-function 'file-readable-p) (lambda (_file) t))
              ((symbol-function 'image-type-available-p)
               (lambda (type) (eq type 'svg)))
              ((symbol-function 'appkit-media-image-mime-type)
               (lambda (_file) "image/jpeg"))
              ((symbol-function 'appkit-media--base-char-pixel-height)
               (lambda () 20))
              ((symbol-function 'svg-create)
               (lambda (width height) (list :svg width height)))
              ((symbol-function 'svg-clip-path)
               (lambda (_svg &rest _properties) :clip))
              ((symbol-function 'svg-rectangle)
               (lambda (&rest arguments)
                 (setq rectangle-call arguments)))
              ((symbol-function 'svg-embed)
               (lambda (&rest arguments)
                 (setq embed-call arguments)))
              ((symbol-function 'svg-image)
               (lambda (_svg &rest properties)
                 (setq image-properties properties)
                 '(image :type svg :data "thumbnail")))
              ((symbol-function 'appkit-media-image-object-valid-p)
               (lambda (_image) t)))
      (let ((image
             (appkit-media-cropped-preview-image-from-file
              "/tmp/source.jpg" 40 60 '(1 2 3 4))))
        (should (= 3 (plist-get (cdr image) :appkit-media-nslices)))
        (should (equal rectangle-call '(:clip 4 1 34 56)))
        (should
         (equal
          (seq-take (nthcdr 4 embed-call) 8)
          '(:x 4 :y 1 :width 34 :height 56)))
        (should
         (equal image-properties
                '(:ascent center :height 60 :scale 1.0)))))))

(ert-deftest appkit-media-horizontal-strip-defers-rasterization ()
  "Offscreen strips retain usable hotspot geometry without allocating a raster."
  (skip-unless (and (display-graphic-p) (image-type-available-p 'svg)
                    (fboundp 'image-cache-size)))
  (let* ((first (make-symbol "first"))
         (second (make-symbol "second"))
         (items (list (list :display-width 30 :id first)
                      (list :display-width 50 :id second)))
         (gc-cons-threshold most-positive-fixnum)
         (before (image-cache-size))
         (image (appkit-media-horizontal-strip-image items 40 4 34)))
    (unwind-protect
        (progn
          (should image)
          (should (= before (image-cache-size)))
          (should (equal (mapcar (lambda (hit) (list (car hit) (cadr hit)))
                                 (image-property image :map))
                         (list (list '(rect . ((0 . 0) . (50 . 40)))
                                     second))))
          (should (equal (image-property image :original-map)
                         (image-property image :map)))
          (should (equal (image-size image t) '(84 . 40))))
      (when image (image-flush image t)))))

(ert-deftest appkit-media-horizontal-strip-reuses-scene-until-geometry-changes ()
  "Resizing text must reuse media; panning must move the visible hotspots."
  (skip-unless (and (display-graphic-p) (image-type-available-p 'svg)))
  (with-temp-buffer
    (let* ((items '((:display-width 30 :id first)
                    (:display-width 50 :id second)))
           (image (appkit-media-horizontal-strip-image items 40 4))
           (panned (appkit-media-horizontal-strip-image items 40 4 34)))
      (should image)
      (should (eq image (appkit-media-horizontal-strip-image
                         (copy-tree items) 40 4)))
      (should (equal (mapcar #'cadr (image-property image :map))
                     '(first second)))
      (should (equal (mapcar #'cadr (image-property panned :map)) '(second)))
      (should (equal (caar (image-property panned :map))
                     '(rect . ((0 . 0) . (50 . 40))))))))

(ert-deftest appkit-media-horizontal-strip-reloads-replaced-file ()
  "Replacing bytes at the same cache path must invalidate a live scene."
  (skip-unless (and (display-graphic-p) (image-type-available-p 'svg)
                    (fboundp 'libxml-parse-xml-region)))
  (let ((file (make-temp-file "appkit-strip-" nil ".svg"))
        (replacement (make-temp-file "appkit-strip-" nil ".svg"))
        (red "<svg xmlns='http://www.w3.org/2000/svg' width='10' height='10'><rect width='10' height='10' fill='#ff0000'/></svg>")
        (blue "<svg xmlns='http://www.w3.org/2000/svg' width='10' height='10'><rect width='10' height='10' fill='#0000ff'/></svg>"))
    (unwind-protect
        (with-temp-buffer
          (with-temp-file file (insert red))
          (let* ((items (list (list :file file :width 1 :height 1)))
                 (image (appkit-media-horizontal-strip-image items 40 0)))
            (should image)
            (should (eq image (appkit-media-horizontal-strip-image items 40 0)))
            (with-temp-file replacement (insert blue))
            (set-file-times replacement
                            (file-attribute-modification-time (file-attributes file)))
            (rename-file replacement file t)
            (let* ((updated (appkit-media-horizontal-strip-image items 40 0))
                   (svg (with-temp-buffer
                          (insert (image-property updated :data))
                          (libxml-parse-xml-region (point-min) (point-max))))
                   (href (dom-attr (car (dom-by-tag svg 'image)) 'href)))
              (should (equal (base64-decode-string
                              (substring href (1+ (string-match "," href))))
                             blue)))))
      (dolist (path (list file replacement))
        (when (file-exists-p path) (delete-file path))))))

(ert-deftest appkit-media-horizontal-strip-plan-is-renderer-independent ()
  "Strip geometry should be reusable without constructing an SVG."
  (let* ((items '((:id first :display-width 30)
                  (:id second :display-width 50)))
         (plan (appkit-media-horizontal-strip-plan items 40 4 34))
         (cells (plist-get plan :items)))
    (should (= (plist-get plan :width) 84))
    (should (= (plist-get plan :height) 40))
    (should (= (plist-get plan :gap) 4))
    (should (= (plist-get plan :offset) 34))
    (should (eq (plist-get (car cells) :item) (car items)))
    (should
     (equal
      (mapcar (lambda (cell)
                (list (plist-get cell :id)
                      (plist-get cell :x)
                      (plist-get cell :width)))
              cells)
      '((first 0 30) (second 34 50))))))

(ert-deftest appkit-media-play-icon-draws-translucent-circle-and-triangle ()
  "The shared play marker should honor an item's position in a larger SVG."
  (let ((appkit-media-video-play-icon-radius-divisor 8.0)
        (appkit-media-video-play-icon-circle-opacity 0.65)
        (appkit-media-video-play-icon-triangle-opacity 0.65)
        circle-call
        polygon-call)
    (cl-letf (((symbol-function 'svg-circle)
               (lambda (&rest arguments)
                 (setq circle-call arguments)))
              ((symbol-function 'svg-polygon)
               (lambda (&rest arguments)
                 (setq polygon-call arguments))))
      (appkit-media-append-video-play-icon :svg 40 24 100 10))
    (should
     (equal circle-call
            '(:svg 120.0 22.0 3.0
              :fill "#000000" :fill-opacity 0.65)))
    (should
     (equal polygon-call
            '(:svg
              ((118.875 . 20.5)
               (118.875 . 23.5)
               (121.875 . 22.0))
              :fill "#ffffff" :fill-opacity 0.65)))))

(ert-deftest appkit-media-image-slice-count-uses-only-appkit-property ()
  (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
             (lambda (_image) nil)))
    (should (= 3
               (appkit-media-image-slice-count
                '(image :appkit-media-nslices 3))))
    (should (= 1
               (appkit-media-image-slice-count
                '(image :disco-nslices 7 :telega-nslices 8)))))
  (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
             (lambda (_image) t))
            ((symbol-function 'image-size)
             (lambda (&rest _) '(20 . 4.4))))
    (should (= 4 (appkit-media-image-slice-count '(image :type png))))))

(ert-deftest appkit-media-image-slices-use-target-character-height ()
  "Slice geometry never follows an unrelated selected window's line."
  (with-temp-buffer
    (let ((image '(image :type png :appkit-media-nslices 3))
          slices)
      (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
                 (lambda (_image) nil))
                ((symbol-function 'appkit-media--char-pixel-height)
                 (lambda () 10))
                ((symbol-function 'line-pixel-height)
                 (lambda ()
                   (ert-fail
                    "slice insertion consulted the selected window")))
                ((symbol-function 'insert-image)
                 (lambda (_image alt &optional _area slice)
                   (push slice slices)
                   (insert alt))))
        (appkit-media-insert-image-slices image nil nil "preview"))
      (should (equal (nreverse slices)
                     '((0 0 1.0 10) (0 10 1.0 10) (0 20 1.0 10))))
      (should (equal (plist-get (cdr image) :appkit-media-nslices) 3)))))

(ert-deftest appkit-media-character-height-uses-target-buffer-window ()
  (with-temp-buffer
    (insert "render position")
    (goto-char 4)
    (let ((origin (point)))
      (cl-letf (((symbol-function 'appkit-geometry-display-window)
                 (lambda (&rest _arguments) 'target-window))
                ((symbol-function 'window-font-height)
                 (lambda (window face)
                   (should (eq window 'target-window))
                   (should (eq face 'default))
                   ;; Some Emacs window font queries move point internally.
                   (goto-char (point-max))
                   24))
                ((symbol-function 'line-pixel-height)
                 (lambda ()
                   (ert-fail "selected line height must not be consulted"))))
        (should (= 24 (appkit-media--char-pixel-height)))
        (should (= origin (point)))))))

(ert-deftest appkit-media-insert-slice-newline-has-no-line-gap ()
  (with-temp-buffer
    (insert "first")
    (appkit-media-insert-slice-newline)
    (let ((newline-position (1- (point))))
      (should (eq (char-after newline-position) ?\n))
      (should (eq (get-text-property newline-position 'line-height) t))
      (should (equal
               (get-text-property newline-position 'rear-nonsticky)
               '(line-height))))))

(ert-deftest appkit-media-insert-image-slices-prefixes-and-activates-slices ()
  (with-temp-buffer
    (let ((image '(image :type png :appkit-media-nslices 3))
          (called 0)
          slices)
      (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
                 (lambda (_image) nil))
                ((symbol-function 'appkit-media--char-pixel-height)
                 (lambda () 10))
                ((symbol-function 'insert-image)
                 (lambda (_image alt &optional _area slice)
                   (push slice slices)
                   (insert alt))))
        (appkit-media-insert-image-slices
         image
         (lambda () (setq called (1+ called)))
         "│ "
         "preview"
         "Open preview"))
      (should (equal (buffer-string)
                     "preview\n│ preview\n│ preview"))
      (should (equal (nreverse slices)
                     '((0 0 1.0 10) (0 10 1.0 10) (0 20 1.0 10))))
      (goto-char (point-min))
      (appkit-ui-activate-at (point-min))
      (should (= called 1)))))

(ert-deftest appkit-media-prepared-occurrences-preserve-actions-and-independence ()
  "Reusable posters retain independent occurrences without changing their data."
  (let* ((descriptor
          (appkit-media-image-with-resource
           '(image :type png :height (2 . ch) :appkit-media-nslices 2)
           '((url . "https://example.invalid/image"))
           :animated t :interaction nil))
         (snapshot (copy-tree descriptor))
         (keymap (make-sparse-keymap))
         (fallback (propertize "alt" 'keymap keymap 'help-echo "Original action"))
         (first (appkit-media-image-display-string descriptor fallback))
         (second (with-temp-buffer
                   (appkit-media-image-display-string descriptor fallback)))
         (first-occurrence (get-text-property 0 'appkit-media-occurrence first))
         (second-occurrence (get-text-property 0 'appkit-media-occurrence second)))
    (should (equal "alt" (substring-no-properties first)))
    (should (eq keymap (get-text-property 0 'keymap first)))
    (should (equal "Original action" (get-text-property 0 'help-echo first)))
    (should-not (eq first-occurrence second-occurrence))
    (should (eq descriptor (cadr (get-text-property 0 'display first))))
    (should (eq descriptor (cadr (get-text-property 0 'display second))))
    (cl-letf (((symbol-function 'appkit-media--char-pixel-height)
               (lambda () 10)))
      (let* ((rows (appkit-media-image-slice-rows descriptor))
             (occurrence (get-text-property 0 'appkit-media-occurrence (car rows)))
             (next-rows (appkit-media-image-slice-rows descriptor)))
        (should (= 2 (length rows)))
        (should (eq occurrence
                    (get-text-property 0 'appkit-media-occurrence (cadr rows))))
        (should-not (eq occurrence
                        (get-text-property
                         0 'appkit-media-occurrence (car next-rows))))
        (should (eq (cadr (get-text-property 0 'display (car rows)))
                    (cadr (get-text-property 0 'display (cadr rows)))))
        (should (equal snapshot descriptor))))))

(ert-deftest appkit-media-gif-capability-survives-static-poster-wrapping ()
  "Actual frame counts, not extensions or poster decoders, determine playback."
  (let ((file (make-temp-file "appkit-gif-source-"))
        (header (concat "GIF89a"
                        (unibyte-string 1 0 1 0 128 0 0 0 0 0 255 255 255)))
        (frame (unibyte-string 44 0 0 0 0 1 0 1 0 0 2 2 68 1 0)))
    (unwind-protect
        (dolist (frames '(1 2))
          (with-temp-file file
            (set-buffer-multibyte nil)
            (insert header)
            (dotimes (_ frames) (insert frame))
            (insert (unibyte-string 59)))
          (let* ((image (appkit-media-image-with-resource
                         '(image :type svg :data "<svg/>")
                         `((file . ,file)) :interaction nil :fit 'cover))
                 (string (appkit-media-one-line-image-display-string image "gif"))
                 (display-image (cadr (get-text-property 0 'display string))))
            (should (eq (> frames 1) (appkit-media-animated-image-p image)))
            (should (eq (> frames 1) (appkit-media-animated-image-p display-image)))
            (should-not
             (appkit-media-animated-image-p
              (appkit-media-image-with-resource image `((file . ,file))
                                                :animated nil)))
            (should
             (appkit-media-animated-image-p
              (appkit-media-image-with-resource image `((file . ,file))
                                                :animated t)))
            (should (eq (> frames 1) (appkit-media-animated-image-p image)))))
      (delete-file file))))

(ert-deftest appkit-media-image-slice-rows-sizes-copy-to-current-line ()
  "Slice rows must not rewrite a cached `:height Nch' descriptor."
  (let ((image '(image :type png :height (3 . ch) :appkit-media-nslices 3)))
    (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
               (lambda (_image) nil))
              ((symbol-function 'appkit-media--char-pixel-height)
               (lambda () 10)))
      (let ((rows (appkit-media-image-slice-rows image)))
        (should (= (length rows) 3))
        (should (equal (plist-get (cdr image) :height) '(3 . ch)))
        (cl-loop for row in rows
                 for index from 0
                 for display = (get-text-property 0 'display row)
                 do (should (equal (car display)
                                   (list 'slice 0 (* index 10) 1.0 10)))
                 do (should (= (plist-get (cdr (cadr display)) :height)
                               30)))))))

(ert-deftest appkit-media-canvas-slice-rows-preserve-descriptor-identity ()
  "Canvas slices must display the descriptor whose mutable buffer is updated."
  (let ((canvas '(image :type canvas :id test-canvas
                  :data-width 120 :data-height 30
                  :appkit-media-nslices 3)))
    (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
               (lambda (_image) nil))
              ((symbol-function 'appkit-media--char-pixel-height)
               (lambda () 10)))
      (let ((rows (appkit-media-image-slice-rows canvas)))
        (should (= (length rows) 3))
        (dolist (row rows)
          (should (eq (cadr (get-text-property 0 'display row))
                      canvas)))
        (should (= (plist-get (cdr canvas) :height) 30))))))

(ert-deftest appkit-media-insert-image-slices-sizes-copy-to-current-line ()
  "Insertion must not rewrite a cached `:height Nch' descriptor."
  (let* ((image '(image :type png :height (3 . ch) :appkit-media-nslices 3))
         inserted)
    (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
               (lambda (_image) nil))
              ((symbol-function 'appkit-media--char-pixel-height)
               (lambda () 10))
              ((symbol-function 'insert-image)
               (lambda (img _alt &optional _area _slice)
                 (push (plist-get (cdr img) :height) inserted)
                 (insert "x"))))
      (appkit-media-insert-image-slices image nil nil "preview"))
    (should (equal (plist-get (cdr image) :height) '(3 . ch)))
    (should (equal inserted '(30 30 30)))))

(ert-deftest appkit-media-preview-height-preserves-aspect-and-bounds ()
  (cl-letf (((symbol-function 'appkit-media--char-pixel-width)
             (lambda () 10))
            ((symbol-function 'appkit-media--base-char-pixel-height)
             (lambda () 20)))
    ;; 1000x500 becomes 400x200, or ten 20-pixel text rows.
    (should (= 10
               (appkit-media-preview-height-chars
                '(1000 . 500) 400 300)))
    ;; Small images are never enlarged.
    (should (= 5
               (appkit-media-preview-height-chars
                '(100 . 100) 400 300)))))

(ert-deftest appkit-media-preview-height-ignores-remapped-line-height ()
  "A larger current line must not reduce the default-scale row count."
  (cl-letf (((symbol-function 'appkit-media--char-pixel-width)
             (lambda () 10))
            ((symbol-function 'appkit-media--base-char-pixel-height)
             (lambda () 20))
            ((symbol-function 'appkit-media--char-pixel-height)
             (lambda () 35)))
    (should (= 10
               (appkit-media-preview-height-chars
                '(1000 . 500) 400 300)))))

(ert-deftest appkit-media-preview-image-owns-slice-property ()
  (let ((appkit-media-preview-max-width 400)
        (appkit-media-preview-max-height 200)
        created-properties)
    (cl-letf (((symbol-function 'appkit-media--image-file-size-pixels)
               (lambda (_file) '(800 . 400)))
              ((symbol-function 'appkit-media--char-pixel-width)
               (lambda () 10))
              ((symbol-function 'appkit-media--base-char-pixel-height)
               (lambda () 20))
              ((symbol-function 'appkit-media--char-pixel-height)
               (lambda () 20))
              ((symbol-function 'create-image)
               (lambda (_file &optional _type _data-p &rest properties)
                 (setq created-properties properties)
                 (cons 'image properties)))
              ((symbol-function 'appkit-media-image-object-valid-p)
               (lambda (_image) t)))
      (let ((image (appkit-media-preview-image-from-file "/tmp/example.png")))
        (should (eq (car image) 'image))
        (should (= 10
                   (plist-get created-properties
                              :appkit-media-nslices)))
        (should (equal '(10 . ch)
                       (plist-get created-properties :height)))
        (should-not (plist-member created-properties :disco-nslices))
        (should-not (plist-member created-properties :telega-nslices))))))

(ert-deftest appkit-media-adjacent-cached-images-remain-separate-glyphs ()
  "Repeated images must occupy three glyphs, not one replacement span."
  (skip-unless (and (display-graphic-p) (image-type-available-p 'svg)))
  (let ((buffer (generate-new-buffer " *appkit-adjacent-images-test*"))
        (image
         (create-image
          "<svg xmlns='http://www.w3.org/2000/svg' width='16' height='16'><rect width='16' height='16' fill='orange'/></svg>"
          'svg t :scale 1.0)))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (dotimes (_ 3)
            (insert (appkit-media-image-display-string image "[face]")))
          (redisplay t)
          (should (= (car (window-text-pixel-size
                           nil (point-min) (point-max)))
                     (* 3 (car (image-size image t)))))
          (should (equal (buffer-substring-no-properties
                          (point-min) (point-max))
                         "[face][face][face]")))
      (kill-buffer buffer))))

(ert-deftest appkit-media-canvas-occurrences-display-the-live-pixel-store ()
  "Native pixel updates must reach every adjacent Canvas glyph."
  (skip-unless (and (display-graphic-p) (image-type-available-p 'canvas)))
  (require 'video-runtime)
  (skip-unless (featurep 'video-module))
  (let ((buffer (generate-new-buffer " *appkit-canvas-occurrences-test*"))
        (canvas (video-canvas-create 16 16 "#ffaa00")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (dotimes (_ 3)
            (insert (appkit-media-image-display-string canvas "[face]")))
          (video-native-canvas-fill canvas 16 16 0 0 16 16 #x00aaff)
          (canvas-refresh canvas)
          (goto-char (point-min))
          (redisplay t)
          (should (= (car (window-text-pixel-size
                           nil (point-min) (point-max)))
                     48))
          (dolist (position '(1 7 13))
            ;; The glyph must refer to the image native drawing updates, not
            ;; an equal Lisp descriptor with a different, empty backing store.
            (should (eq (posn-image (posn-at-point position)) canvas))))
      (kill-buffer buffer))))

(ert-deftest appkit-media-one-line-thumbnail-scales-both-axes-on-cold-and-warm-load ()
  (skip-unless (and (display-graphic-p) (image-type-available-p 'canvas)))
  (let ((file (make-temp-file "appkit-one-line-zoom-" nil ".png"))
        (buffer (generate-new-buffer " *appkit-thumbnail-zoom-test*"))
        (appkit-media--thumbnail-cache (make-hash-table :test #'equal))
        (appkit-media--thumbnail-waiting nil)
        (appkit-media--thumbnail-active nil)
        (appkit-media--thumbnail-timer nil)
        image previous-width)
    (unwind-protect
        (save-window-excursion
          (let ((coding-system-for-write 'no-conversion))
            (write-region
             (base64-decode-string
              "iVBORw0KGgoAAAANSUhEUgAAAAIAAAABCAIAAAB7QOjdAAAADUlEQVR4nGP4zwAE/wEHAAH/4iOeWQAAAABJRU5ErkJggg==")
             nil file nil 'silent))
          (switch-to-buffer buffer)
          (dolist (amount '(-2 0 2))
            (text-scale-set amount)
            (let ((cold-image
                   (appkit-media-one-line-preview-image-from-file file))
                  rendered-size)
              (unless image (setq image cold-image))
              (dolist (source (if (eq image cold-image)
                                 (list image)
                               (list image cold-image)))
                (erase-buffer)
                (insert (appkit-media-one-line-image-display-string source "▧"))
                (goto-char (point-min))
                (redisplay t)
                (let* ((window (selected-window))
                       (pixels (posn-object-width-height
                                (posn-at-point (point-min) window))))
                  (should (= (cdr pixels)
                             (window-font-height window 'default)))
                  (when rendered-size
                    (should (equal pixels rendered-size)))
                  (setq rendered-size pixels)
                  (when (and previous-width (eq source image))
                    (should (> (car pixels) previous-width)))))
              (setq previous-width
                    (car (posn-object-width-height
                          (posn-at-point (point-min) (selected-window))))))))
      (when (timerp appkit-media--thumbnail-timer)
        (cancel-timer appkit-media--thumbnail-timer))
      (setq appkit-media--thumbnail-waiting nil)
      (dolist (job (copy-sequence appkit-media--thumbnail-active))
        (appkit-media--thumbnail-finish job))
      (kill-buffer buffer)
      (delete-file file))))

(ert-deftest appkit-media-normalizes-known-image-leading-newline ()
  (let* ((png (concat (unibyte-string 137 80 78 71 13 10 26 10) "data"))
         (jpeg (concat (unibyte-string 255 216 255) "data"))
         (unknown "\nnot-an-image"))
    (should (equal png
                   (appkit-media-normalize-image-bytes
                    (concat (unibyte-string ?\n) png))))
    (should (equal jpeg
                   (appkit-media-normalize-image-bytes
                    (concat (unibyte-string ?\r ?\n) jpeg))))
    (should (equal unknown
                   (appkit-media-normalize-image-bytes unknown)))))

(ert-deftest appkit-media-detects-image-extensions-from-bytes ()
  (should (equal "png"
                 (appkit-media-bytes-to-extension
                  (unibyte-string 137 80 78 71 13 10 26 10) "img")))
  (should (equal "jpg"
                 (appkit-media-bytes-to-extension
                  (unibyte-string 255 216 255) "img")))
  (should (equal "gif"
                 (appkit-media-bytes-to-extension "GIF89a" "img")))
  (should (equal "webp"
                 (appkit-media-bytes-to-extension "RIFF1234WEBP" "img")))
  (should (equal "fallback"
                 (appkit-media-bytes-to-extension "unknown" "fallback"))))

(ert-deftest appkit-media-thumbnail-known-geometry-never-decodes-source ()
  (require 'video-inline)
  (let ((file (make-temp-file "appkit-thumbnail-"))
        (appkit-media--thumbnail-cache (make-hash-table :test #'equal))
        (appkit-media--thumbnail-waiting nil)
        (metadata-reads 0))
    (let ((coding-system-for-write 'no-conversion))
      (write-region
       (base64-decode-string
        "iVBORw0KGgoAAAANSUhEUgAAAAIAAAABCAIAAAB7QOjdAAAADUlEQVR4nGP4zwAE/wEHAAH/4iOeWQAAAABJRU5ErkJggg==")
       nil file nil 'silent))
    (unwind-protect
        (cl-letf (((symbol-function 'image-type-available-p) (lambda (_) t))
                  ((symbol-function 'video-canvas-create)
                   (lambda (w h &rest _) (list 'image :type 'canvas :width w :height h)))
                  ((symbol-function 'video-background-color) (lambda (&rest _) "#ffffff"))
                  ((symbol-function 'appkit-media--thumbnail-schedule) #'ignore)
                  ((symbol-function 'appkit-media--base-char-pixel-height) (lambda () 20))
                  ((symbol-function 'appkit-media--file-animated-p)
                   (lambda (_) (cl-incf metadata-reads) t))
                  ((symbol-function 'image-size) (lambda (&rest _) (ert-fail "Decoded geometry")))
                  ((symbol-function 'video-source-poster) (lambda (&rest _) (ert-fail "Blocking poster")))
                  ((symbol-function 'svg-embed) (lambda (&rest _) (ert-fail "Embedded source"))))
          (let* ((image (appkit-media-one-line-preview-image-from-file file 40 'contain))
                 (thumbnail (plist-get (cdr image) :appkit-media-thumbnail))
                 (video (appkit-media-image-with-resource
                         image '((file . "/original/video.mp4"))
                         :kind 'video :autoplay nil :animated nil))
                 (text (appkit-media-one-line-image-display-string image "[image]"))
                 (video-text (appkit-media-one-line-image-display-string video "[video]"))
                 (canvas (cadr (get-text-property 0 'display text))))
            (should (= 40 (appkit-media--thumbnail-width thumbnail)))
            (should (= 20 (appkit-media--thumbnail-height thumbnail)))
            (should (eq 'contain (appkit-media--thumbnail-fit thumbnail)))
            (should (equal file (appkit-media--thumbnail-source thumbnail)))
            (should (eq canvas (cadr (get-text-property 0 'display video-text))))
            (should (eq thumbnail (plist-get (cdr video) :appkit-media-thumbnail)))
            (should (equal '(2.0 . ch) (plist-get (cdr canvas) :width)))
            (should (equal '(1 . ch) (plist-get (cdr canvas) :height)))
            (should (equal '((file . "/original/video.mp4"))
                           (appkit-media--playback-resource
                            (plist-get (cdr video) :appkit-media-playback))))
            (dotimes (_ 50)
              (appkit-media-one-line-preview-image-from-file file 40 'contain))
            (should (= 1 metadata-reads))
            (should (= 1 (length appkit-media--thumbnail-waiting)))
            (appkit-media--thumbnail-get file 40 20 'contain "#000000")
            (should (= 2 (length appkit-media--thumbnail-waiting)))))
      (delete-file file))))

(ert-deftest appkit-media-thumbnail-jobs-bound-preroll-and-retire-once ()
  (require 'video-inline)
  (let ((appkit-media--thumbnail-active nil)
        (appkit-media--thumbnail-waiting nil)
        (appkit-media--thumbnail-timer nil)
        jobs prepared closed timers)
    (dotimes (index 4)
      (push (appkit-media--thumbnail-job-create
             :thumbnail (appkit-media--thumbnail-create
                         :source (number-to-string index) :width 40 :height 20
                         :fit 'cover :background "#ffffff"
                         :poster (list 'image :type 'canvas :width 40 :height 20)))
            jobs))
    (setq appkit-media--thumbnail-waiting (copy-sequence jobs))
    (cl-letf (((symbol-function 'video-session-create)
               (lambda (source &rest _) (video--make-session :player source)))
              ((symbol-function 'video-session-inline-create)
               (lambda (session _w _h &rest args)
                 (cons session args)))
              ((symbol-function 'video-inline-prepare) (lambda (inline) (push inline prepared)))
              ((symbol-function 'video-inline-play) (lambda (_) (ert-fail "Autoplayed poster")))
              ((symbol-function 'video-session-close) (lambda (session) (push session closed)))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat callback &rest args)
                 (push (cons callback args) timers) nil))
              ((symbol-function 'appkit-media--thumbnail-schedule) #'ignore)
              ((symbol-function 'video-native-canvas-fill) #'ignore)
              ((symbol-function 'canvas-refresh) #'ignore))
      (unwind-protect
          (progn
            (appkit-media--thumbnail-pump)
            (should (= 2 (length prepared)))
            (should (= 2 (length appkit-media--thumbnail-active)))
            (should (= 2 (length appkit-media--thumbnail-waiting)))
            (let* ((job (car jobs))
                   (inline (appkit-media--thumbnail-job-inline job))
                   (activate (plist-get (cdr inline) :activate-function))
                   (buffer (appkit-media--thumbnail-job-buffer job)))
              (funcall activate inline nil)
              (apply (caar timers) (cdar timers))
              (should-not (buffer-live-p buffer))
              (should (= 1 (length closed)))
              (funcall activate inline nil)
              (appkit-media--thumbnail-finish job)
              (should (= 1 (length closed))))
            (appkit-media--thumbnail-pump)
            (should (= 3 (length prepared)))
            (let ((job (cadr jobs)))
              (appkit-media--thumbnail-error
               (appkit-media--thumbnail-job-player job) "bad image")
              (should (appkit-media--thumbnail-job-finished job))
              (should-not (buffer-live-p (appkit-media--thumbnail-job-buffer job))))
            (appkit-media--thumbnail-pump)
            (should (= 4 (length prepared)))
            (should-not appkit-media--thumbnail-waiting))
        (dolist (job jobs) (appkit-media--thumbnail-finish job))))))


(provide 'appkit-media-image-test)

;;; appkit-media-image-test.el ends here
