;;; appkit-selection-test.el --- Operation target precedence -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit-selection)

(ert-deftest appkit-selection-captured-targets-win-over-new-user-selection ()
  (should
   (equal (appkit-selection-target-keys
           :captured '("original-a" "original-b")
           :region '("region") :marked '("marked") :point "point")
          '("original-a" "original-b")))
  ;; An invalidated/empty captured target must not act on a different message.
  (should-not (appkit-selection-target-keys
               :captured nil :region '("region") :marked '("marked") :point "point")))

(ert-deftest appkit-selection-active-region-never-falls-back-to-marks-or-point ()
  (should (equal (appkit-selection-target-keys
                  :region '("region-b" "region-a")
                  :marked '("marked") :point "point")
                 '("region-b" "region-a")))
  ;; An active region containing no message (for example a draft) is explicit.
  (should-not (appkit-selection-target-keys
               :region nil :marked '("marked") :point "point"))
  (should (equal (appkit-selection-target-keys :marked '("marked") :point "point")
                 '("marked")))
  (should (equal (appkit-selection-target-keys :point "point") '("point")))
  (should-not (appkit-selection-target-keys)))

(ert-deftest appkit-selection-retiring-operation-targets-does-not-mutate-marks ()
  (let* ((marked (list "a" "b"))
         (operation (appkit-selection-target-keys :marked marked)))
    (setcdr operation nil)
    (should (equal marked '("a" "b")))
    (should (equal (appkit-selection-target-keys :marked marked) '("a" "b")))))

(provide 'appkit-selection-test)
;;; appkit-selection-test.el ends here
