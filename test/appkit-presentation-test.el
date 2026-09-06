;;; appkit-presentation-test.el --- Tests for Appkit presentation helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-presentation)
(require 'appkit-test-helper)

(ert-deftest appkit-presentation-render-list-spec-renders-items-and-footer ()
  (with-temp-buffer
    (appkit-presentation-render-list-spec
     (appkit-presentation-list-spec-create
      :title "Threads"
      :summary "2 items"
      :items '("one" "two")
      :item-inserter (lambda (item)
                       (insert (format "- %s\n" item)))
      :footer-lines '("footer")))
    (should (string-match-p "Threads" (buffer-string)))
    (should (string-match-p "- one" (buffer-string)))
    (should (string-match-p "footer" (buffer-string)))))

(ert-deftest appkit-presentation-render-list-spec-preserving-position-restores-anchor ()
  (with-temp-buffer
    (let ((items '(("a" . "first")
                   ("b" . "second"))))
      (cl-labels ((render-spec (rows)
                    (appkit-presentation-list-spec-create
                     :items rows
                     :item-inserter
                     (lambda (item)
                       (let ((start (point)))
                         (insert (format "%s\n" (cdr item)))
                         (add-text-properties
                          start
                          (point)
                          (list 'row-id (car item))))))))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (appkit-presentation-render-list-spec (render-spec items)))
        (goto-char (point-min))
        (search-forward "second")
        (beginning-of-line)
        (appkit-presentation-render-list-spec-preserving-position
         (render-spec '(("a" . "first updated")
                        ("b" . "second updated")))
         :anchor-property 'row-id)
        (should (equal "b" (get-text-property (point) 'row-id)))
        (should (looking-at-p "second updated"))))))

(ert-deftest appkit-presentation-canonicalize-number-supports-ratio-and-bounds ()
  (should (= 42 (appkit-presentation-canonicalize-number 42 100)))
  (should (= 35 (appkit-presentation-canonicalize-number 0.35 100)))
  (should (= 20 (appkit-presentation-canonicalize-number '(0.1 20 60) 100)))
  (should (= 60 (appkit-presentation-canonicalize-number '(0.8 20 60) 100)))
  (should (= 90 (appkit-presentation-canonicalize-number '(0.9 20) 100))))

(ert-deftest appkit-presentation-one-line-column-widths-follow-context-ratio ()
  (let ((widths (appkit-presentation-one-line-column-widths 60 '(0.45 20))))
    (should (= 27 (plist-get widths :context-inner-width)))
    (should (= 30 (plist-get widths :preview-width)))
    (should (= 1 (plist-get widths :separator-width))))
  (let ((widths (appkit-presentation-one-line-column-widths 30 '(0.45 20))))
    (should (= 20 (plist-get widths :context-inner-width)))
    (should (= 7 (plist-get widths :preview-width)))
    (should (= 1 (plist-get widths :separator-width)))))

(ert-deftest appkit-presentation-one-line-row-collapses-preview-newlines ()
  (with-temp-buffer
    (appkit-presentation-insert-one-line-row
     (appkit-presentation-one-line-row-create :context "Group\nName" :preview (appkit-ui-one-line-preview-create :text "first line\nsecond line\r\nthird") :time "12:34")
     :width 80
     :icon-slot-width 4
     :context-width-spec '(0.32 16 30))
    (should (= (count-lines (point-min) (point-max)) 1))
    (should (string-match-p "Group Name" (buffer-string)))
    (should (string-match-p "first line second line third"
                            (buffer-string)))))

(ert-deftest appkit-presentation-one-line-row-renders-rich-preview-in-preview-column ()
  (let* ((image '(image :type png :data "bytes"))
         (prefix (propertize "[image]" 'display image)))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'display-images-p) (lambda (&rest _) t)))
      (with-temp-buffer
        (appkit-presentation-insert-one-line-row
         (appkit-presentation-one-line-row-create
          :context "Group"
          :preview
          (appkit-ui-one-line-preview-create
           :text "hello"
           :label "Alice"
           :separator ":"
           :visual prefix
           :visual-columns 2
           :label-face 'success))
         :width 60
         :context-width-spec 20)
        (should (= 1 (count-lines (point-min) (point-max))))
        (let ((image-position
               (text-property-any (point-min) (point-max) 'display image)))
          (should image-position)
          (should-not
           (memq 'success
                 (ensure-list
                  (get-text-property image-position 'face)))))
        (goto-char (point-min))
        (search-forward "Alice:")
        (should
         (memq 'success
               (ensure-list
                (get-text-property (- (point) 6) 'face))))))))

(ert-deftest appkit-presentation-one-line-row-places-normalized-context-trail-inside-brackets ()
  (with-temp-buffer
    (appkit-presentation-insert-one-line-row
     (appkit-presentation-one-line-row-create :context "Group"
                                              :context-trail "  7\nnew  " :preview (appkit-ui-one-line-preview-create :text "preview") )
     :width 60
     :context-width-spec 20)
    (goto-char (point-min))
    (let ((open (search-forward "["))
          (context (search-forward "Group"))
          (trail (search-forward "7 new"))
          (close (search-forward "]")))
      (should (< open context trail close)))
    (should (= 1 (count-lines (point-min) (point-max))))))

(ert-deftest appkit-presentation-one-line-row-allows-zero-width-icon-slot ()
  (with-temp-buffer
    (appkit-presentation-insert-one-line-row
     (appkit-presentation-one-line-row-create
      :context "Channel"
      :preview (appkit-ui-one-line-preview-create :text "preview"))
     :width 60
     :icon-slot-width 0)
    (should (string-prefix-p "[Channel" (buffer-string)))))

(ert-deftest appkit-presentation-one-line-row-supports-client-context-delimiters ()
  (with-temp-buffer
    (appkit-presentation-insert-one-line-row
     (appkit-presentation-one-line-row-create
      :context "Guild > Channel"
      :context-open "<"
      :context-close ">"
      :context-trail "7"
      :preview (appkit-ui-one-line-preview-create :text "preview"))
     :width 60
     :context-width-spec 20)
    (goto-char (point-min))
    (let ((open (search-forward "<"))
          (context (search-forward "Guild > Channel"))
          (trail (search-forward "7"))
          (close (search-forward ">")))
      (should (< open context trail close)))
    (should-not (string-match-p "\\[" (buffer-string)))))

(ert-deftest appkit-presentation-one-line-row-measures-wide-context-delimiters ()
  (with-temp-buffer
    (appkit-presentation-insert-one-line-row
     (appkit-presentation-one-line-row-create
      :context "Voice"
      :context-open "「"
      :context-close "」"
      :context-trail "7"
      :preview (appkit-ui-one-line-preview-create :text "preview")
      :time "12:34")
     :width 60
     :context-width-spec 20)
    (should (string-match-p "「Voice.*7」" (buffer-string)))
    (goto-char (point-min))
    (search-forward "12:34")
    (should (= 60 (current-column)))))

(ert-deftest appkit-presentation-one-line-row-applies-face-only-to-context-trail ()
  (with-temp-buffer
    (let ((trail (propertize "42" 'appkit-test-trail t)))
      (appkit-presentation-insert-one-line-row
       (appkit-presentation-one-line-row-create :context "Group"
                                                :context-trail trail
                                                :context-trail-face 'font-lock-warning-face :preview (appkit-ui-one-line-preview-create :text "preview") )
       :width 60
       :context-width-spec 20))
    (goto-char (point-min))
    (search-forward "42")
    (let ((trail-start (- (point) 2))
          (close (point)))
      (should (eq 'font-lock-warning-face
                  (get-text-property trail-start 'face)))
      (should (get-text-property trail-start 'appkit-test-trail))
      (should-not (get-text-property (1- trail-start) 'face))
      (should-not (get-text-property close 'face)))))

(ert-deftest appkit-presentation-one-line-row-preserves-context-trail-text-properties ()
  (with-temp-buffer
    (appkit-presentation-insert-one-line-row
     (appkit-presentation-one-line-row-create :context "Group"
                                              :context-trail
                                              (concat (propertize "12\n" 'face 'success)
                                                      (propertize "@3" 'face 'warning)) :preview (appkit-ui-one-line-preview-create :text "preview") )
     :width 60
     :context-width-spec 20)
    (goto-char (point-min))
    (search-forward "12 @3")
    (let ((trail-start (- (point) 5)))
      (should (eq 'success (get-text-property trail-start 'face)))
      (should (eq 'success (get-text-property (1+ trail-start) 'face)))
      (should (eq 'warning (get-text-property (+ trail-start 3) 'face)))
      (should (eq 'warning (get-text-property (+ trail-start 4) 'face))))))

(ert-deftest appkit-presentation-one-line-row-keeps-context-column-with-trails ()
  (with-temp-buffer
    (dolist (trail '(nil "7" "12345"))
      (appkit-presentation-insert-one-line-row
       (appkit-presentation-one-line-row-create :context "Group"
                                                :context-trail trail :preview (appkit-ui-one-line-preview-create :text "preview") )
       :width 60
       :context-width-spec 20))
    (let (closing-columns)
      (goto-char (point-min))
      (dotimes (_ 3)
        (search-forward "]" (line-end-position))
        (backward-char)
        (push (current-column) closing-columns)
        (forward-line 1))
      (should (= 1 (length (delete-dups closing-columns)))))))

(ert-deftest appkit-presentation-one-line-row-long-context-retains-right-aligned-trail ()
  (with-temp-buffer
    (appkit-presentation-insert-one-line-row
     (appkit-presentation-one-line-row-create :context (make-string 80 ?x)
                                              :context-trail "NEW" :preview (appkit-ui-one-line-preview-create :text "preview") )
     :width 60
     :context-width-spec 16)
    (goto-char (point-min))
    (search-forward "NEW")
    (let* ((trail-start (- (point) 3))
           (trail-column (save-excursion
                           (goto-char trail-start)
                           (current-column)))
           (separator-column (save-excursion
                               (goto-char (1- trail-start))
                               (current-column)))
           (close-column (save-excursion
                           (search-forward "]")
                           (backward-char)
                           (current-column))))
      (should (= trail-column (- close-column 3)))
      (should (>= (- trail-column separator-column) 2))
      (should-not (get-text-property trail-start 'display))
      (should (equal "…"
                     (save-excursion
                       (goto-char (point-min))
                       (search-forward "[")
                       (get-text-property
                        (next-single-property-change
                         (point) 'display nil trail-start)
                        'display)))))))

(ert-deftest appkit-presentation-one-line-row-does-not-infer-hover-from-help ()
  (with-temp-buffer
    (appkit-presentation-insert-one-line-row
     (appkit-presentation-one-line-row-create :context "Group" :preview (appkit-ui-one-line-preview-create :text "preview") :time "12:34"
                                              :help-echo "Open Group")
     :width 80)
    (should (equal "Open Group" (get-text-property (point-min) 'help-echo)))
    (should-not (text-property-not-all
                 (point-min) (point-max) 'mouse-face nil))))

(ert-deftest appkit-presentation-one-line-row-supports-explicit-hover-face ()
  (with-temp-buffer
    (appkit-presentation-insert-one-line-row
     (appkit-presentation-one-line-row-create :context "Group" :preview (appkit-ui-one-line-preview-create :text "preview") :time "12:34"
                                              :mouse-face 'highlight)
     :width 80)
    (should (eq 'highlight (get-text-property (point-min) 'mouse-face)))))

(ert-deftest appkit-presentation-one-line-row-keeps-context-column-across-time-formats ()
  (with-temp-buffer
    (dolist (time '("" "Thu•" "29.06.26•"))
      (appkit-presentation-insert-one-line-row
       (appkit-presentation-one-line-row-create :context "channel" :preview (appkit-ui-one-line-preview-create :text "preview") :time time)
       :width 80
       :time-slot-width 9))
    (let (targets)
      (goto-char (point-min))
      (dotimes (_ 3)
        (let ((line-end (line-end-position)))
          (should (search-forward "]" line-end t))
          (push (get-text-property (- (point) 2) 'display) targets))
        (forward-line 1))
      (should (= 1 (length (delete-dups targets)))))))

(ert-deftest appkit-presentation-chars-xwidth-does-not-select-display-window ()
  (select-window (get-largest-window))
  (let ((original-window (selected-window))
        other-window
        (buffer (generate-new-buffer " *appkit-presentation-width*")))
    (unwind-protect
        (progn
          (setq other-window (split-window original-window))
          (set-window-buffer other-window buffer)
          (with-current-buffer buffer
            (insert "window point\nbuffer insertion point")
            (set-window-point other-window (point-min))
            (goto-char (point-max))
            (let ((expected-point (point)))
              (cl-letf (((symbol-function 'display-graphic-p)
                         (lambda (&optional _display) t))
                        ((symbol-function 'string-pixel-width)
                         (lambda (_string &optional measured-buffer)
                           (should (eq measured-buffer buffer))
                           9)))
                (should (= 9 (appkit-geometry-columns-pixel-width 1 other-window))))
              (should (= expected-point (point)))
              (should (eq original-window (selected-window))))))
      (when (window-live-p other-window)
        (delete-window other-window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest appkit-geometry-default-line-pixel-height-keeps-insertion-point ()
  (select-window (get-largest-window))
  (let ((original-window (selected-window))
        other-window
        (buffer (generate-new-buffer " *appkit-presentation-line-height*")))
    (unwind-protect
        (progn
          (setq other-window (split-window original-window))
          (set-window-buffer other-window buffer)
          (with-current-buffer buffer
            (insert "window point\nbuffer insertion point")
            (set-window-point other-window (point-min))
            (goto-char (point-max))
            (let ((expected-point (point)))
              (cl-letf (((symbol-function 'window-font-height)
                         (lambda (&optional window &rest _args)
                           (with-selected-window
                               (or window (selected-window))
                             16))))
                (should (= 16 (appkit-geometry-default-line-pixel-height))))
              (should (= expected-point (point)))
              (should (eq original-window (selected-window))))))
      (when (window-live-p other-window)
        (delete-window other-window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest appkit-geometry-insert-alignment-space-inserts-align-spacer-when-needed ()
  (with-temp-buffer
    (insert "abc")
    (let ((insert-pos (point)))
      (appkit-geometry-insert-alignment-space 3)
      (should (= (point) (1+ insert-pos)))
      (should (>= (appkit-geometry-current-column) 3))
      (let ((display-prop (get-text-property insert-pos 'display)))
        (should (consp display-prop))
        (should (eq (car display-prop) 'space))
        (should (plist-member (cdr display-prop) :align-to))))))

(ert-deftest appkit-geometry-insert-alignment-space-always-inserts-absolute-spacer ()
  (with-temp-buffer
    (insert "abcdef")
    (let ((insert-pos (point)))
      (appkit-geometry-insert-alignment-space 2)
      (should (= (point) (1+ insert-pos)))
      (should (equal '(space :align-to 2)
                     (get-text-property insert-pos 'display))))))

(ert-deftest appkit-geometry-display-space-keeps-source-width-constant ()
  (let ((columns (appkit-geometry-display-space :columns 8))
        (pixels (appkit-geometry-display-space :pixels 24))
        (aligned (appkit-geometry-display-space :align-to 40)))
    (should (= 1 (length columns)))
    (should (equal '(space :width 8)
                   (get-text-property 0 'display columns)))
    (should (equal '(space :width (24))
                   (get-text-property 0 'display pixels)))
    (should (equal '(space :align-to 40)
                   (get-text-property 0 'display aligned)))
    (should-error
     (appkit-geometry-display-space :columns 1 :pixels 2))))

(ert-deftest appkit-geometry-window-width-uses-remapped-width-and-margins ()
  (let* ((win (selected-window))
         (buffer (window-buffer win))
         (margins (window-margins win))
         (expected (- (+ (window-width win 'remap)
                         (or (car margins) 0)
                         (or (cdr margins) 0))
                      1)))
    (with-current-buffer buffer
      (let ((display-line-numbers-mode nil))
        (should (= (appkit-geometry-window-width win 1) expected))))))

(ert-deftest appkit-geometry-display-window-prefers-focus-then-widest ()
  (save-window-excursion
    (delete-other-windows)
    (let* ((buffer (generate-new-buffer " *appkit-presentation-display*"))
           (other-buffer (generate-new-buffer " *appkit-presentation-other*"))
           (first-window (selected-window))
           (second-window (split-window-right))
           (other-window (split-window-below)))
      (unwind-protect
          (progn
            (set-window-buffer first-window buffer)
            (set-window-buffer second-window buffer)
            (set-window-buffer other-window other-buffer)
            (cl-letf (((symbol-function 'window-width)
                       (lambda (window &optional _pixelwise)
                         (if (eq window second-window) 80 40))))
              (select-window first-window)
              (with-current-buffer buffer
                (should (eq first-window (appkit-geometry-display-window))))
              (select-window other-window)
              (with-current-buffer buffer
                (should (eq second-window (appkit-geometry-display-window))))))
        (kill-buffer buffer)
        (kill-buffer other-buffer)))))

(ert-deftest appkit-geometry-display-window-supports-frame-capability-filter ()
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (should
       (eq (selected-window)
           (appkit-geometry-display-window nil (lambda (_frame) t))))
      (should-not
       (appkit-geometry-display-window nil (lambda (_frame) nil)))
      (should-not
       (appkit-geometry-display-frame nil (lambda (_frame) nil))))))

(ert-deftest appkit-presentation-elide-string-adds-display-ellipsis ()
  (let* ((text "abcdefghijklmnopqrstuvwxyz")
         (elided (appkit-presentation-elide-string text 8 'shadow)))
    (should (> (length elided) 8))
    (let ((display-pos (next-single-property-change 0 'display elided)))
      (should (integerp display-pos))
      (should (equal "…"
                     (get-text-property display-pos 'display elided))))))

(ert-deftest appkit-presentation-elide-string-noop-when-string-fits ()
  (let ((text "short"))
    (should (equal text (appkit-presentation-elide-string text 12 'shadow)))))

(ert-deftest appkit-presentation-elide-string-for-columns-uses-pixel-width ()
  (cl-labels ((pixel-width
                (text)
                (let ((total 0))
                  (dolist (character (string-to-list text) total)
                    (setq total
                          (+ total
                             (cond
                              ((= character ?🏆) 30)
                              ((= character ?…) 10)
                              (t 10))))))))
    (cl-letf (((symbol-function 'appkit-presentation--string-pixel-width)
               (lambda (text &optional _face) (pixel-width text)))
              ((symbol-function 'appkit-geometry-columns-pixel-width)
               (lambda (columns &optional _window) (* columns 10))))
      (let* ((text "🏆abcdefgh")
             (elided (appkit-presentation-elide-string-for-columns text 5 'shadow))
             (display-position
              (next-single-property-change 0 'display elided)))
        (should (= 2 display-position))
        (should (equal "…"
                       (get-text-property display-position 'display elided)))
        (should
         (= 50
            (pixel-width
             (concat (substring elided 0 display-position) "…"))))))))

(ert-deftest appkit-presentation-safe-elide-boundary-preserves-emoji-clusters ()
  (should (= 0 (appkit-presentation--safe-elide-boundary "👍🏽ok" 1)))
  (should (= 1 (appkit-presentation--safe-elide-boundary "👩‍💻ok" 2)))
  (should (= 0 (appkit-presentation--safe-elide-boundary "🇨🇳ok" 1))))

(ert-deftest appkit-presentation-insert-label-row-applies-struct-fields ()
  (with-temp-buffer
    (appkit-presentation-insert-label-row
     (appkit-presentation-label-row-create
      :label "Section"
      :prefix "[+] "
      :suffix " (4)"
      :face 'font-lock-keyword-face
      :line-properties '(row-kind section)))
    (goto-char (point-min))
    (should (equal "[+] Section (4)\n" (buffer-string)))
    (should (equal 'section (get-text-property (point) 'row-kind)))
    (should (equal 'font-lock-keyword-face
                   (get-text-property (point) 'face)))))

(ert-deftest appkit-presentation-insert-label-line-supports-prefix-icon-and-suffix ()
  (with-temp-buffer
    (appkit-presentation-insert-label-line
     "Guild"
     :prefix "  [-] "
     :icon-inserter (lambda ()
                      (insert "*"))
     :icon-separator " "
     :suffix " (2)"
     :line-properties '(row-kind guild)
     :help-echo "toggle")
    (goto-char (point-min))
    (should (looking-at-p "  \\[-\\] \\* Guild (2)"))
    (should (equal 'guild (get-text-property (point) 'row-kind)))
    (should (equal "toggle" (get-text-property (point) 'help-echo)))))

(ert-deftest appkit-presentation-insert-heading-line-applies-face-and-properties ()
  (with-temp-buffer
    (appkit-presentation-insert-heading-line
     "Heading"
     :face 'font-lock-keyword-face
     :line-properties '(row-kind section))
    (goto-char (point-min))
    (should (equal 'section (get-text-property (point) 'row-kind)))
    (should (equal 'font-lock-keyword-face
                   (get-text-property (point) 'face)))))
