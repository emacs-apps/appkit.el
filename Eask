;; -*- mode: eask; lexical-binding: t -*-

(package "appkit"
         "0.3.0"
         "Runtime primitives for stateful buffer applications")

(website-url "https://github.com/emacs-im/appkit.el")
(keywords "lisp" "extensions")

(package-file "appkit.el")

(source "gnu")
(source 'melpa)

(depends-on "emacs" "32.0")
(depends-on "plz" "0.8")
(depends-on "video" :file
            (expand-file-name "../video.el"
                              (file-truename default-directory)))

(development
 (depends-on "evil"))

(script "test"
        "eask test ert --quiet test/appkit-test-helper.el test/*-test.el")
