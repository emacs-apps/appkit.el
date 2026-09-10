;;; appkit-fontify.el --- Isolated native Font Lock presentation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;;; Commentary:

;; One-shot, source-preserving face projection from a client-selected major
;; mode.  Clients own mode selection, fallback, base faces, and any bounded
;; presentation cache.  No source or scratch buffers are retained here.

;;; Code:

(require 'cl-lib)
(require 'font-lock)
(require 'treesit)

(defun appkit-fontify--faces-only (text)
  "Return a detached copy of TEXT carrying only face presentation.
Normalize `font-lock-face' to `face', below any existing `face'."
  (let* ((result (substring-no-properties text))
         (length (length text))
         (position 0))
    (while (< position length)
      (let ((next
             (min (next-single-property-change position 'face text length)
                  (next-single-property-change
                   position 'font-lock-face text length))))
        (dolist (property '(face font-lock-face))
          (when-let* ((face (get-text-property position property text)))
            (add-face-text-property
             position next (copy-tree face) 'append result)))
        (setq position next)))
    result))

(defun appkit-fontify-string (text mode)
  "Return TEXT with native Font Lock faces from major mode MODE.
The characters are unchanged; input text properties are ignored.  Only
`face' properties are returned, never overlays, display replacements,
keymaps, actions, syntax properties, or mode-local state.

MODE must be a non-nil symbol naming a trusted, client-selected major
mode, not a mode inferred from untrusted text.  The scratch buffer is
marked as `untrusted-content'.  Standard mode hooks in that buffer and
automatic Tree-sitter grammar installation are suppressed.  This is not
a sandbox for arbitrary mode implementations.

Return nil if MODE is unavailable, initialization/fontification fails,
or the mode changes the source characters.  Invalid argument types signal
an error and user quits propagate.  The disposable scratch buffer is
killed on success, error, or quit.  No result cache is maintained."
  (unless (stringp text)
    (signal 'wrong-type-argument (list 'stringp text)))
  (unless (and mode (symbolp mode))
    (signal 'wrong-type-argument (list 'symbolp mode)))
  (when (fboundp mode)
    (save-match-data
      (condition-case nil
          (let ((source (substring-no-properties text))
                (inhibit-message t)
                (message-log-max nil)
                (treesit-auto-install-grammar nil)
                (enable-local-variables nil)
                (enable-local-eval nil)
                (change-major-mode-hook nil))
            ;; Like Gnus and Eglot: use native one-shot fontification, not
            ;; Font Lock mode.  `with-temp-buffer' suppresses buffer lifecycle
            ;; hooks and disposes of parsers as well as text on every exit.
            (with-temp-buffer
              (setq-local untrusted-content t)
              (insert source)
              (let ((scratch (current-buffer))
                    (run-hooks (symbol-function 'run-hooks)))
                ;; `delay-mode-hooks' handles major modes; minor modes
                ;; enabled by a mode body use `run-hooks' directly.
                ;; Do not suppress hooks in unrelated buffers.
                (cl-letf (((symbol-function 'run-hooks)
                           (lambda (&rest hooks)
                             (unless (eq (current-buffer) scratch)
                               (apply run-hooks hooks)))))
                  (delay-mode-hooks
                    (funcall mode)
                    (font-lock-ensure (point-min) (point-max))
                    (widen)
                    (when (equal source
                                 (buffer-substring-no-properties
                                  (point-min) (point-max)))
                      (appkit-fontify--faces-only (buffer-string))))))))
        (error nil)))))

(provide 'appkit-fontify)

;;; appkit-fontify.el ends here
