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
  (let* ((app (appkit-app-start 'appkit-test :id 'account))
         (view
          (appkit-open-view
           :app app :id 'timeline :mode 'special-mode
           :buffer-name " *appkit-scroll-observer*"))
         (buffer (appkit-view-buffer view))
         (window 'appkit-scroll-window)
         observer
         calls)
    (unwind-protect
        (with-current-buffer buffer
          (setq observer
                (appkit-scroll-observer-install
                 view
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
          (appkit-kill-view view)
          (should-not (appkit-scroll-observer-active-p observer))
          (should-not
           (memq (appkit-scroll-observer-post-command-function observer)
                 post-command-hook))
          (should-not
           (memq (appkit-scroll-observer-window-scroll-function observer)
                 window-scroll-functions)))
      (appkit-app-close app)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest appkit-scroll-observer-hooks-measure-after-redisplay ()
  (let* ((app (appkit-app-start 'appkit-test :id 'hook-scroll))
         (view
          (appkit-open-view
           :app app :id 'timeline :mode 'special-mode
           :buffer-name " *appkit-scroll-hook*"))
         (buffer (appkit-view-buffer view))
         (window 'appkit-scroll-window)
         callback
         callback-arguments
         (scheduled 0)
         (measurement-allowed-p nil)
         (measurements 0)
         calls
         observer)
    (unwind-protect
        (with-current-buffer buffer
          (setq observer
                (appkit-scroll-observer-install
                 view :end-function
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
                           (list (cons (point-max) (point-max)))))))
      (appkit-app-close app)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest appkit-scroll-observer-defers-stale-window-end-once ()
  (let* ((app (appkit-app-start 'appkit-test :id 'deferred-scroll))
         (view
          (appkit-open-view
           :app app :id 'timeline :mode 'special-mode
           :buffer-name " *appkit-scroll-deferred*"))
         (buffer (appkit-view-buffer view))
         (window 'appkit-scroll-window)
         callback
         callback-arguments
         (scheduled 0)
         visible-p
         calls
         observer)
    (unwind-protect
        (with-current-buffer buffer
          (setq observer
                (appkit-scroll-observer-install
                 view :end-function
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
             (appkit-scroll-observer-deferred-check-handle observer))))
      (appkit-app-close app)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'appkit-scroll-test)

;;; appkit-scroll-test.el ends here
