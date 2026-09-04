;;; appkit-scroll-test.el --- Tests for window-edge observation -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(require 'appkit-test-helper)
(require 'appkit-scroll)

(ert-deftest appkit-scroll-proximity-uses-configurable-character-distance ()
  (should (appkit-scroll-near-start-p 10 1 20))
  (should-not (appkit-scroll-near-start-p 21 1 20))
  (should-not (appkit-scroll-near-start-p 1 1 nil))
  (should (appkit-scroll-near-end-p 95 100 20))
  (should-not (appkit-scroll-near-end-p 80 100 20))
  (should-not (appkit-scroll-near-end-p 100 100 nil))
  (should-not (appkit-scroll-near-start-p 1 1 -1))
  (should-not (appkit-scroll-near-end-p 100 100 -1)))

(ert-deftest appkit-scroll-visible-range-follows-window-and-content-bounds ()
  (with-temp-buffer
    (let ((window 'appkit-scroll-window))
      (cl-letf (((symbol-function 'window-live-p)
                 (lambda (candidate) (eq candidate window)))
                ((symbol-function 'window-buffer)
                 (lambda (_window) (current-buffer)))
                ((symbol-function 'window-start)
                 (lambda (_window) 5))
                ((symbol-function 'window-end)
                 (lambda (_window update)
                   (should update)
                   95))
                ((symbol-function 'pos-visible-in-window-p)
                 (lambda (position candidate _partially)
                   (and (eq candidate window) (= position 94)))))
        (should (equal '(10 . 90)
                       (appkit-scroll-window-visible-range window 10 90)))
        (should (equal '(5 . 95)
                       (appkit-scroll-window-visible-range window)))))))

(ert-deftest appkit-scroll-observer-owns-hooks-and-rejects-reentrancy ()
  (appkit-test-with-surface
    (let ((surface appkit-test-surface)
          (buffer appkit-test-buffer)
          (window 'appkit-scroll-window)
          observer
          calls)
      (setq observer
            (appkit-scroll-observer-install
             surface
             :start-boundary-function (lambda () 10)
             :end-boundary-function (lambda () 90)
             :start-function
             (lambda (candidate position boundary)
               (push (list 'start candidate position boundary) calls)
               (appkit-scroll-observer-check observer candidate))
             :end-function
             (lambda (candidate position boundary)
               (push (list 'end candidate position boundary) calls))))
      (should (memq (appkit-scroll-observer-post-command-function observer)
                    post-command-hook))
      (should (memq (appkit-scroll-observer-window-scroll-function observer)
                    window-scroll-functions))
      (cl-letf (((symbol-function 'window-live-p)
                 (lambda (candidate) (eq candidate window)))
                ((symbol-function 'window-buffer)
                 (lambda (_window) buffer))
                ((symbol-function 'window-start)
                 (lambda (_window) 5))
                ((symbol-function 'window-end)
                 (lambda (_window update)
                   (should update)
                   95))
                ((symbol-function 'pos-visible-in-window-p)
                 (lambda (position candidate _partially)
                   (and (eq candidate window) (= position 94)))))
        (appkit-scroll-observer-check observer window))
      (should
       (equal '((end appkit-scroll-window 90 90)
                (start appkit-scroll-window 10 10))
              calls))
      (appkit-surface-stop surface)
      (should-not (appkit-scroll-observer-active-p observer))
      (should-not
       (memq (appkit-scroll-observer-post-command-function observer)
             post-command-hook))
      (should-not
       (memq (appkit-scroll-observer-window-scroll-function observer)
             window-scroll-functions)))))

(ert-deftest appkit-scroll-observer-hooks-measure-after-redisplay ()
  (appkit-test-with-surface
    (let ((surface appkit-test-surface)
          (buffer appkit-test-buffer)
          (window 'appkit-scroll-window)
          callback
          callback-arguments
          (scheduled 0)
          (measurement-allowed-p nil)
          (measurements 0)
          calls
          observer)
      (setq observer
            (appkit-scroll-observer-install
             surface :end-function
             (lambda (_window position boundary)
               (push (cons position boundary) calls))))
      (cl-letf (((symbol-function 'window-live-p)
                 (lambda (candidate) (eq candidate window)))
                ((symbol-function 'window-buffer)
                 (lambda (_window) buffer))
                ((symbol-function 'window-start) (lambda (_window) 1))
                ((symbol-function 'window-end)
                 (lambda (_window update)
                   (should measurement-allowed-p)
                   (should update)
                   (cl-incf measurements)
                   (point-max)))
                ((symbol-function 'pos-visible-in-window-p)
                 (lambda (_position _window _partially) t))
                ((symbol-function 'get-buffer-window-list)
                 (lambda (&rest _arguments) (list window)))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (_seconds _repeat function &rest arguments)
                   (cl-incf scheduled)
                   (setq callback function
                         callback-arguments arguments)
                   'hook-timer)))
        (funcall
         (appkit-scroll-observer-window-scroll-function observer)
         window 1)
        (funcall
         (appkit-scroll-observer-window-scroll-function observer)
         window 2)
        (funcall
         (appkit-scroll-observer-post-command-function observer))
        (should (= scheduled 1))
        (should (zerop measurements))
        (should-not calls)
        (setq measurement-allowed-p t)
        (apply callback callback-arguments)
        (should (= measurements 1))
        (should (equal calls
                       (list (cons (point-max) (point-max)))))))))

(ert-deftest appkit-scroll-observer-defers-stale-window-end-once ()
  (appkit-test-with-surface
    (let ((surface appkit-test-surface)
          (buffer appkit-test-buffer)
          (window 'appkit-scroll-window)
          callback
          callback-arguments
          (scheduled 0)
          visible-p
          calls
          observer)
      (setq observer
            (appkit-scroll-observer-install
             surface :end-function
             (lambda (_window position boundary)
               (push (cons position boundary) calls))))
      (cl-letf (((symbol-function 'window-live-p)
                 (lambda (candidate) (eq candidate window)))
                ((symbol-function 'window-buffer)
                 (lambda (_window) buffer))
                ((symbol-function 'window-start) (lambda (_window) 1))
                ((symbol-function 'window-end)
                 (lambda (_window _update) (point-max)))
                ((symbol-function 'pos-visible-in-window-p)
                 (lambda (_position _window _partially) visible-p))
                ((symbol-function 'get-buffer-window-list)
                 (lambda (&rest _arguments) (list window)))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (_seconds _repeat function &rest arguments)
                   (cl-incf scheduled)
                   (setq callback function
                         callback-arguments arguments)
                   'deferred-timer)))
        (appkit-scroll-observer-check observer window)
        (appkit-scroll-observer-check observer window)
        (should (= scheduled 1))
        (should-not calls)
        (setq visible-p t)
        (apply callback callback-arguments)
        (should (equal calls
                       (list (cons (point-max) (point-max)))))
        (should-not
         (appkit-scroll-observer-deferred-check-handle observer))))))

(ert-deftest appkit-scroll-observer-defers-render-pass-callbacks-and-revokes-on-stop ()
  (appkit-test-with-surface
    (let* ((surface appkit-test-surface)
           (buffer appkit-test-buffer)
           (window 'appkit-scroll-window)
           (scheduled 0) (calls 0)
           callback callback-arguments posted observer)
      (setq observer
            (appkit-scroll-observer-install
             surface :end-function
             (lambda (&rest _)
               (cl-incf calls)
               (setq posted (appkit-surface-post surface 'next-page)))))
      (setf (appkit-generated-renderer-render (appkit-surface-renderer surface))
            (lambda (&rest _)
              (appkit-scroll-observer-check observer window)
              nil))
      (cl-letf (((symbol-function 'window-live-p)
                 (lambda (candidate) (eq candidate window)))
                ((symbol-function 'window-buffer) (lambda (_window) buffer))
                ((symbol-function 'get-buffer-window-list) (lambda (&rest _) (list window)))
                ((symbol-function 'appkit-scroll-window-visible-range)
                 (lambda (&rest _) (cons (point-min) (point-max))))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (_delay _repeat function &rest arguments)
                   (cl-incf scheduled)
                   (setq callback function callback-arguments arguments)
                   'deferred-timer)))
        (appkit-surface-send surface 'refresh)
        (appkit-surface-send surface 'refresh)
        (should (= calls 0))
        (should (= scheduled 1))
        (apply callback callback-arguments)
        (should (= calls 1))
        (should (eq posted 'enqueued))
        (appkit-surface-stop surface)
        (apply callback callback-arguments)
        (should (= calls 1))))))

(provide 'appkit-scroll-test)

;;; appkit-scroll-test.el ends here
