;;; appkit-url.el --- URL handlers for buffer applications -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;;; Commentary:

;; Applications register local URL matchers and autoloadable opening commands.
;; `appkit-url-mode' installs them as package defaults in Browse URL, leaving
;; user rules and the default browser in control of all other destinations.

;;; Code:

(require 'browse-url)

(defgroup appkit-url nil
  "Open supported web links in buffer applications."
  :group 'applications)

(defvar appkit-url--handlers nil
  "Registered (NAME . (MATCHER . HANDLER)) entries, newest first.
Each entry's identity tracks the Browse URL rule owned by Appkit.")

(defun appkit-url-unregister (name)
  "Remove the URL handler registered under NAME.
Only remove Appkit's own Browse URL entry; preserve other package rules."
  (when-let* ((registration (assq name appkit-url--handlers)))
    (setq browse-url-default-handlers
          (remq (cdr registration) browse-url-default-handlers)
          appkit-url--handlers
          (assq-delete-all name appkit-url--handlers)))
  name)

(defun appkit-url-register (name matcher handler)
  "Register NAME's MATCHER and internal URL opening command HANDLER.
MATCHER is a regexp or a function called with the URL.  A function should
recognize supported destinations locally, without opening views or fetching
remote data.  HANDLER is a function symbol accepting a URL and an optional
new-window argument; both functions may be autoloads.

Replace any previous registration for NAME.  Newer registrations take
precedence among Appkit handlers, after `browse-url-handlers'.  Install the
rule immediately when `appkit-url-mode' is enabled."
  (unless (and name (symbolp name))
    (signal 'wrong-type-argument (list 'symbolp name)))
  (unless (or (stringp matcher) (functionp matcher))
    (signal 'wrong-type-argument (list '(or stringp functionp) matcher)))
  (unless (and (symbolp handler) (functionp handler))
    (signal 'wrong-type-argument (list 'functionp handler)))
  (appkit-url-unregister name)
  (let ((entry (cons matcher handler)))
    (push (cons name entry) appkit-url--handlers)
    (function-put handler 'browse-url-browser-kind 'internal)
    (when appkit-url-mode
      (add-to-list 'browse-url-default-handlers entry)))
  name)

;;;###autoload
(define-minor-mode appkit-url-mode
  "Open supported URLs in registered buffer applications.
User rules in `browse-url-handlers' take precedence.  Unclaimed URLs keep
using `browse-url-browser-function'.  Disabling the mode removes only the
entries installed by Appkit.  Applications can register before or after
this mode is enabled."
  :global t
  :group 'appkit-url
  (if appkit-url-mode
      (dolist (registration (reverse appkit-url--handlers))
        (add-to-list 'browse-url-default-handlers (cdr registration)))
    (dolist (registration appkit-url--handlers)
      (setq browse-url-default-handlers
            (remq (cdr registration) browse-url-default-handlers)))))

;;;###autoload
(defun appkit-url-open-external (url &optional new-window)
  "Open URL with an external browser, bypassing internal app handlers.
NEW-WINDOW is passed to the selected browser."
  (interactive (browse-url-interactive-arg "URL: "))
  (browse-url-with-browser-kind 'external url new-window))

(provide 'appkit-url)

;;; appkit-url.el ends here
