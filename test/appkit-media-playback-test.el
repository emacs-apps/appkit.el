;;; appkit-media-playback-test.el --- Playback plan tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'appkit-media-image)
(require 'appkit-media-effect)

(ert-deftest appkit-media-playback-plan-carries-video-policy ()
  (let* ((resource '((url . "https://cdn.example/canvas.mp4")
                     (name . "canvas.mp4")
                     (mime-type . "video/mp4")))
         (headers '(("Referer" . "https://open.spotify.com/")))
         (image (appkit-media-image-with-resource
                 '(image :type png :data "")
                 resource
                 :kind 'video :label "Canvas"
                 :autoplay nil :muted t :inline-autoplay t :loop t
                 :cache-key 'spotify-canvas
                 :cache-directory "/tmp/verse-media"
                 :cache-policy 'automatic
                 :request-headers headers))
         (plan (plist-get (cdr image) :appkit-media-playback)))
    (should (appkit-media-playback-p plan))
    (should (eq 'video (appkit-media-playback-kind plan)))
    (should (equal resource (appkit-media-playback-resource plan)))
    (should-not (appkit-media-playback-autoplay plan))
    (should (appkit-media-playback-muted plan))
    (should (appkit-media-playback-inline-autoplay plan))
    (should (appkit-media-playback-loop plan))
    (should (eq 'automatic (appkit-media-playback-cache-policy plan)))
    (should (equal headers (appkit-media-playback-request-headers plan)))
    (should (eq 'spotify-canvas (appkit-media-playback-cache-key plan)))))

(ert-deftest appkit-media-playback-presentation-uses-plan-policy ()
  (let* ((plan (appkit-media-playback-create
                :resource '((url . "https://cdn.example/episode.mp4"))
                :kind 'video :label "Episode"
                :muted t :loop t :live nil
                :cache-key 'episode-video
                :cache-directory "/tmp/verse-media"
                :cache-policy 'automatic
                :request-headers '(("Referer" . "spotify"))))
         captured)
    (cl-letf (((symbol-function 'appkit-media-video-presentation-create)
               (lambda (&rest args)
                 (setq captured args)
                 :presentation)))
      (should (eq :presentation
                  (appkit-media-playback-presentation-create
                   plan :start nil))))
    (should (equal '((url . "https://cdn.example/episode.mp4"))
                   (car captured)))
    (let ((args (cdr captured)))
      (should (eq 'video (plist-get args :kind)))
      (should (eq 'episode-video (plist-get args :cache-key)))
      (should (equal "/tmp/verse-media" (plist-get args :cache-directory)))
      (should (eq 'automatic (plist-get args :cache-policy)))
      (should (eq t (plist-get args :muted)))
      (should (eq t (plist-get args :loop)))
      (should-not (plist-get args :live))
      (should (equal '(("Referer" . "spotify"))
                     (plist-get args :request-headers)))
      (should (eq video-animation-loop-policy
                  (plist-get args :animation-loop-policy)))
      (should-not (plist-get args :start)))))

(ert-deftest appkit-media-playback-inline-autoplay-is-opt-in ()
  (require 'appkit-media-inline)
  (let ((appkit-media-inline-animation-enabled t))
    (should-not
     (appkit-media--playback-inline-autoplay-p
      (appkit-media-playback-create :kind 'video :autoplay t)))
    (should
     (appkit-media--playback-inline-autoplay-p
      (appkit-media-playback-create
       :kind 'video :autoplay t :inline-autoplay t)))
    (should
     (appkit-media--playback-inline-autoplay-p
      (appkit-media-playback-create
       :kind 'video :autoplay nil :inline-autoplay t)))
    (should-not
     (appkit-media--playback-inline-autoplay-p
      (appkit-media-playback-create
       :kind 'image :autoplay t :animated-p nil)))
    (should
     (appkit-media--playback-inline-autoplay-p
      (appkit-media-playback-create
       :kind 'image :autoplay t :animated-p t)))))

(ert-deftest appkit-media-video-session-forwards-loop-policy ()
  (let (captured)
    (cl-letf (((symbol-function 'video-session-create)
               (lambda (source &rest args)
                 (setq captured (cons source args))
                 :native-session)))
      (let ((session
             (appkit-media-video-session-create
              '((url . "https://cdn.example/loop.mp4"))
              "Loop" :kind 'video :loop t :muted t)))
        (should (eq :native-session
                    (appkit-media-video-session-video-session session)))))
    (should (equal "https://cdn.example/loop.mp4" (car captured)))
    (should (eq t (plist-get (cdr captured) :loop)))
    (should (eq t (plist-get (cdr captured) :muted)))))

(ert-deftest appkit-media-playback-session-uses-plan-policy ()
  (let* ((plan (appkit-media-playback-create
                :resource '((url . "https://cdn.example/session.mp4"))
                :kind 'video :label "Session"
                :muted t :loop t :live nil
                :cache-key 'session
                :cache-directory "/tmp/verse-media"
                :cache-policy 'automatic
                :request-headers '(("Referer" . "spotify"))))
         captured)
    (cl-letf (((symbol-function 'appkit-media-video-session-create)
               (lambda (&rest args)
                 (setq captured args)
                 :session)))
      (should (eq :session
                  (appkit-media-playback-session-create
                   plan :owner 'owner))))
    (should (equal '((url . "https://cdn.example/session.mp4"))
                   (car captured)))
    (let ((args (cddr captured)))
      (should (eq 'video (plist-get args :kind)))
      (should (eq 'session (plist-get args :cache-key)))
      (should (equal "/tmp/verse-media" (plist-get args :cache-directory)))
      (should (eq 'automatic (plist-get args :cache-policy)))
      (should (eq t (plist-get args :muted)))
      (should (eq t (plist-get args :loop)))
      (should (eq 'owner (plist-get args :owner)))
      (should (equal '(("Referer" . "spotify"))
                     (plist-get args :request-headers)))
      (should (eq video-animation-loop-policy
                  (plist-get args :animation-loop-policy))))))

(provide 'appkit-media-playback-test)
;;; appkit-media-playback-test.el ends here
