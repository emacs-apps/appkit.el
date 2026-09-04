;;; appkit-test-helper.el --- Shared Appkit test fixtures -*- lexical-binding: t; -*-

(require 'appkit-app)
(require 'appkit-command)
(require 'appkit-surface)

(defconst appkit-test--app-type
  (appkit-app-type-create
   :name 'appkit-test
   :init (lambda (_context _input)
           (appkit-next :model nil :render appkit-render-none))
   :update (lambda (_context model _message)
             (appkit-next :model model :render appkit-render-none))
   :shutdown #'ignore)
  "Canonical App type for runtime tests.")

(defconst appkit-test--surface-type
  (appkit-surface-type-create
   :name 'appkit-test
   :mode #'fundamental-mode
   :init (lambda (_context _input)
           (appkit-next :model nil :render t))
   :update (lambda (_context model _message)
             (appkit-next :model model :render t))
   :renderer-factory
   (lambda (_surface)
     (appkit-generated-renderer-create
      :mount #'ignore
      :merge (lambda (_left right) right)
      :render #'ignore
      :recover nil
      :unmount #'ignore)))
  "Generated Surface type for presentation tests.")

(defmacro appkit-test-with-surface (&rest body)
  "Evaluate BODY in a temporary buffer with a live Generated Surface."
  (declare (indent 0) (debug t))
  `(let* ((appkit-test-app
           (appkit-app-start
            appkit-test--app-type :identity (make-symbol "app")))
          (appkit-test-surface
           (appkit-open-generated-surface
            appkit-test--surface-type
            :app appkit-test-app :identity (make-symbol "surface")))
          (appkit-test-buffer
           (appkit-surface-buffer appkit-test-surface)))
     (unwind-protect
         (with-current-buffer appkit-test-buffer
           ,@body)
       (when (buffer-live-p appkit-test-buffer)
         (kill-buffer appkit-test-buffer))
       (unless (eq (appkit-app-status appkit-test-app) 'stopped)
         (appkit-app-close appkit-test-app)))))

(provide 'appkit-test-helper)

;;; appkit-test-helper.el ends here
