;;; appkit-chat-history-test.el --- Tests for continuous history windows -*- lexical-binding: t; -*-

(require 'ert)

(require 'appkit-chat-history)

(ert-deftest appkit-chat-history-reset-is-an-unknown-production-window ()
  (with-temp-buffer
    (appkit-chat-history-init-state)
    (should-not (appkit-chat-history-window-known-p))
    (should-not (appkit-chat-history-window-partial-p))
    (appkit-chat-history-window-set nil nil)
    (should (appkit-chat-history-window-known-p))
    (should-not (appkit-chat-history-window-empty-p))
    (should-not (appkit-chat-history-window-partial-p))
    (appkit-chat-history-reset-state)
    (should-not (appkit-chat-history-window-known-p))
    (should-not (appkit-chat-history-window-empty-p))
    (should-not (appkit-chat-history-window-first-key))
    (should-not (appkit-chat-history-window-last-key))))

(ert-deftest appkit-chat-history-strict-slice-distinguishes-empty-and-invalid ()
  (with-temp-buffer
    (appkit-chat-history-reset-state)
    (let ((unknown (appkit-chat-history-window-slice nil #'car)))
      (should-not (plist-get unknown :valid-p))
      (should (eq 'unknown-window (plist-get unknown :reason))))
    (appkit-chat-history-window-set nil nil)
    (let ((empty (appkit-chat-history-window-slice nil #'car)))
      (should (plist-get empty :valid-p))
      (should-not (plist-get empty :entries))
      (should-not (plist-get empty :reason)))
    (let ((entries '(("a" . 1) ("b" . 2) ("c" . 3) ("d" . 4))))
      (appkit-chat-history-window-set "b" "c")
      (let ((slice (appkit-chat-history-window-slice entries #'car)))
        (should (plist-get slice :valid-p))
        (should (equal '(("b" . 2) ("c" . 3))
                       (plist-get slice :entries))))
      (appkit-chat-history-window-set "missing" "c")
      (should (eq 'missing-first-key
                  (plist-get
                   (appkit-chat-history-window-slice entries #'car)
                   :reason)))
      (appkit-chat-history-window-set "b" "missing")
      (should (eq 'missing-last-key
                  (plist-get
                   (appkit-chat-history-window-slice entries #'car)
                   :reason)))
      (appkit-chat-history-window-set "c" "b")
      (should (eq 'reversed-edges
                  (plist-get
                   (appkit-chat-history-window-slice entries #'car)
                   :reason))))))

(ert-deftest appkit-chat-history-authoritative-empty-hides-cache-and-seeds-live ()
  (with-temp-buffer
    (let ((cached '(("old-island" . 1) ("other-island" . 2))))
      (appkit-chat-history-window-establish-empty)
      (should (appkit-chat-history-window-known-p))
      (should (appkit-chat-history-window-empty-p))
      (should (appkit-chat-history-older-loaded-p))
      (let ((slice (appkit-chat-history-window-slice cached #'car)))
        (should (plist-get slice :valid-p))
        (should-not (plist-get slice :entries)))
      ;; Seed before cache merge remains safe: the exact first key is missing.
      (should (appkit-chat-history-window-seed-live "live"))
      (should-not (appkit-chat-history-window-empty-p))
      (let ((before-merge (appkit-chat-history-window-slice cached #'car)))
        (should-not (plist-get before-merge :valid-p))
        (should (eq 'missing-first-key (plist-get before-merge :reason))))
      ;; Once canonical state contains the live entry, old cache islands remain
      ;; outside the exact window.
      (let ((after-merge
             (appkit-chat-history-window-slice
              (append cached '(("live" . 3))) #'car)))
        (should (plist-get after-merge :valid-p))
        (should (equal '(("live" . 3))
                       (plist-get after-merge :entries))))
      (should-not (appkit-chat-history-window-seed-live "later")))))

(ert-deftest appkit-chat-history-window-set-clears-authoritative-empty ()
  (with-temp-buffer
    (appkit-chat-history-window-establish-empty)
    (appkit-chat-history-window-set nil nil)
    (should (appkit-chat-history-window-known-p))
    (should-not (appkit-chat-history-window-empty-p))
    (let ((slice
           (appkit-chat-history-window-slice '(("cached" . 1)) #'car)))
      (should (plist-get slice :valid-p))
      (should (equal '(("cached" . 1)) (plist-get slice :entries))))))

(ert-deftest appkit-chat-history-newer-stall-follows-the-exact-edge ()
  (with-temp-buffer
    (appkit-chat-history-window-set "a" "b")
    (should (equal "b" (appkit-chat-history-newer-stalled-set)))
    (should (appkit-chat-history-newer-stalled-p))
    (appkit-chat-history-window-set "a" "c")
    (should-not (appkit-chat-history-newer-stalled-p))
    (appkit-chat-history-newer-stalled-set)
    (should (appkit-chat-history-newer-stalled-p))
    (appkit-chat-history-newer-stalled-clear)
    (should-not (appkit-chat-history-newer-stalled-p))))

(ert-deftest appkit-chat-history-window-clear-discards-window-edge-facts ()
  (with-temp-buffer
    (appkit-chat-history-window-set "a" "b")
    (appkit-chat-history-older-loaded-set t)
    (appkit-chat-history-newer-stalled-set)
    (progn
      (appkit-chat-history-loading-set 'around)
      (appkit-chat-history-window-clear)
      (should-not (appkit-chat-history-window-known-p))
      (should-not (appkit-chat-history-older-loaded-p))
      (should-not (appkit-chat-history-newer-stalled-p))
      (should (eq 'around (appkit-chat-history-loading))))
    ))

(ert-deftest appkit-chat-history-autoload-gates-window-state-and-position ()
  (with-temp-buffer
    (appkit-chat-history-reset-state)
    (should-not (appkit-chat-history-autoload-older-p 10 1 20))
    (should-not (appkit-chat-history-autoload-newer-p 95 100 20))
    (appkit-chat-history-window-set "a" "b")
    (should (appkit-chat-history-autoload-older-p 10 1 20))
    (should (appkit-chat-history-autoload-newer-p 95 100 20))
    (should-not (appkit-chat-history-autoload-newer-p 95 100 20 nil))
    (should-not (appkit-chat-history-autoload-newer-p 50 100 20))
    (appkit-chat-history-older-loaded-set t)
    (should-not (appkit-chat-history-autoload-older-p 10 1 20))
    (appkit-chat-history-newer-stalled-set)
    (should-not (appkit-chat-history-autoload-newer-p 95 100 20))
    (appkit-chat-history-newer-stalled-clear)
    (progn
      (appkit-chat-history-loading-set 'newer)
      (should-not (appkit-chat-history-autoload-newer-p 95 100 20))
      (appkit-chat-history-loading-set nil))

    (appkit-chat-history-window-set "a" nil)
    (should-not (appkit-chat-history-autoload-newer-p 95 100 20))))

(ert-deftest appkit-chat-history-delimiter-reflects-window-and-loading ()
  (with-temp-buffer
    (appkit-chat-history-reset-state)
    (should (equal "········"
                   (appkit-chat-history-delimiter-string 8 :face nil)))
    (appkit-chat-history-window-set nil nil)
    (should (equal "────────"
                   (appkit-chat-history-delimiter-string 8 :face nil)))
    (appkit-chat-history-window-set "a" "b")
    (should (equal "········"
                   (appkit-chat-history-delimiter-string 8 :face nil)))
    (progn
      (appkit-chat-history-loading-set 'newer)
      (let
          ((delimiter
            (appkit-chat-history-delimiter-string 14 :loading-text
                                                  "loading" :face nil)))
        (should (= 14 (string-width delimiter)))
        (should (string-match-p " loading " delimiter)))
      (appkit-chat-history-loading-set nil))
    ))

(provide 'appkit-chat-history-test)

;;; appkit-chat-history-test.el ends here
