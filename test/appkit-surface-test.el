;;; appkit-surface-test.el --- Tests for generated Surface runtime -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'appkit-surface)
(require 'appkit-geometry)

(define-derived-mode appkit-surface-test-mode special-mode "Appkit-Surface")

(defun appkit-surface-test--renderer
    (record &optional render-function recover-function mount-function)
  "Create a test renderer reporting lifecycle events through RECORD."
  (appkit-generated-renderer-create
   :mount
   (or mount-function
       (lambda (_surface _app-read-view model)
         (funcall record (list 'mount model))))
   :merge
   (lambda (left right)
     (append (if (listp left) left (list left))
             (if (listp right) right (list right))))
   :render
   (or render-function
       (lambda (_surface _app-read-view model request)
         (funcall record (list 'render model request))
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (format "%S" model)))))
   :recover
   (or recover-function
       (lambda (_surface _app-read-view model condition)
         (funcall record
                  (list 'recover model (error-message-string condition)))
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (format "recovered:%S" model)))))
   :unmount
   (lambda (_surface) (funcall record '(unmount)))))

(cl-defun appkit-surface-test--type
    (&key init update renderer-factory)
  "Create a generated test Surface type."
  (appkit-surface-type-create
   :name 'test-generated
   :mode #'appkit-surface-test-mode
   :init init
   :update update
   :renderer-factory renderer-factory))

(defun appkit-surface-test--cleanup (surface)
  "Stop SURFACE and kill its test buffer."
  (when (appkit-surface-p surface)
    (let ((buffer (appkit-surface-buffer surface)))
      (condition-case nil
          (appkit-surface-stop surface)
        ((error quit) nil))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defmacro appkit-surface-test--with-surface (binding &rest body)
  "Open the Surface described by BINDING and evaluate BODY with cleanup."
  (declare (indent 1) (debug ((symbolp form &rest form) body)))
  (let ((surface (car binding))
        (type (cadr binding))
        (arguments (cddr binding)))
    `(let (,surface)
       (unwind-protect
           (progn
             (setq ,surface
                   (appkit-open-generated-surface ,type ,@arguments))
             ,@body)
         (appkit-surface-test--cleanup ,surface)))))

(ert-deftest appkit-geometry-insert-alignment-space-uses-remapped-buffer-metrics ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'appkit-geometry-display-window)
               (lambda (&rest _) (selected-window)))
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'frame-char-width) (lambda (&rest _) 8))
              ((symbol-function 'appkit-geometry-columns-pixel-width)
               (lambda (columns &optional _window) (* columns 12))))
      (let ((position (point)))
        (appkit-geometry-insert-alignment-space 10)
        (should
         (equal '(space :align-to (120))
                (get-text-property position 'display)))))))

(ert-deftest appkit-geometry-alignment-keeps-right-edge-across-remap ()
  (cl-labels
      ((right-edge
         (columns character-pixels)
         (with-temp-buffer
           (cl-letf (((symbol-function 'appkit-geometry-display-window)
                      (lambda (&rest _) (selected-window)))
                     ((symbol-function 'display-graphic-p)
                      (lambda (&rest _) t))
                     ((symbol-function 'frame-char-width)
                      (lambda (&rest _) 8))
                     ((symbol-function 'appkit-geometry-columns-pixel-width)
                      (lambda (count &optional _window)
                        (* count character-pixels))))
             (let ((position (point)))
               (appkit-geometry-insert-alignment-space (- columns 5))
               (+ (car (nth 2 (get-text-property position 'display)))
                  (* 5 character-pixels)))))))
    ;; Both remaps describe a 960-pixel text area.  The timestamp's right edge
    ;; must stay at that same pixel even though its column count changes.
    (should (= 960 (right-edge 120 8)))
    (should (= 960 (right-edge 80 12)))))

(ert-deftest appkit-surface-responsive-geometry-observes-and-cleans-hooks ()
  (save-window-excursion
    (let ((width 30) callbacks hook)
      (appkit-surface-test--with-surface
          (surface
           (appkit-surface-test--type
            :init (lambda (_context input)
                    (appkit-next :model input :render appkit-render-none))
            :update (lambda (_context model _message)
                      (appkit-next :model model :render appkit-render-none))
            :renderer-factory
            (lambda (_surface)
              (appkit-surface-test--renderer #'ignore))))
        (set-window-buffer (selected-window) (appkit-surface-buffer surface))
        (with-current-buffer (appkit-surface-buffer surface)
          (cl-letf (((symbol-function 'appkit-geometry-display-window)
                     (lambda (&rest _) (selected-window)))
                    ((symbol-function 'appkit-geometry-window-width)
                     (lambda (&rest _) width)))
            (appkit-surface-enable-responsive-geometry
             surface
             (lambda (owner measured)
               (push (list owner measured) callbacks)))
            (setq hook appkit-surface--responsive-hook)
            (should (= 30 (appkit-surface-responsive-width surface)))
            (appkit-surface-refresh-responsive-geometry surface)
            (setq width 40)
            (funcall hook 'window-change)
            ;; A no-argument text-scale hook must redraw even when column width
            ;; stays unchanged, because image pixel geometry has changed.
            (funcall hook)
            (should (equal (mapcar #'cadr (nreverse callbacks))
                           '(30 40 40)))
            (appkit-surface-stop surface)
            (should-not appkit-surface--responsive-owner)
            (should-not (memq hook window-state-change-functions))
            (should-not (memq hook display-line-numbers-mode-hook))
            (should-not (memq hook text-scale-mode-hook))))))))

(cl-defun appkit-surface-test--effect
    (key record &key synchronous label cancel-error)
  "Create a test Effect under KEY reporting lifecycle through RECORD.

SYNCHRONOUS resolves during startup.  LABEL distinguishes instances sharing
KEY.  CANCEL-ERROR makes physical cleanup fail after recording the attempt."
  (let ((name (or label key)))
    (appkit-effect-create
     :key key
     :input key
     :start
     (lambda (_context _input _observe resolve _reject)
       (funcall record (list 'start name))
       (if synchronous
           (progn
             (funcall resolve 'ready)
             nil)
         (appkit-cancellation-create
          :kind 'logical
          :cancel
          (lambda ()
            (funcall record (list 'cancel name))
            (when cancel-error
              (error "%s cancellation failed" name))))))
     :success (lambda (owned value) (list 'effect-done owned value))
     :failure (lambda (owned reason) (list 'effect-failed owned reason)))))

(ert-deftest appkit-surface-initializes-mode-before-attachment ()
  (let (events init-mode init-owner init-ready init-send-condition surface
               (update-count 0))
    (unwind-protect
        (progn
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-test--type
                  :init
                  (lambda (_context input)
                    (setq init-mode major-mode
                          init-owner (appkit-current-surface)
                          init-ready
                          (appkit-surface-ready-p init-owner)
                          init-send-condition
                          (condition-case condition
                              (appkit-surface-send init-owner 'too-early)
                            ((error quit) condition)))
                    (appkit-next :model input :render 'initial))
                  :update
                  (lambda (_context model _message)
                    (setq update-count (1+ update-count))
                    (appkit-next :model model :render appkit-render-none))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-surface-test--renderer
                     (lambda (event) (push event events)))))
                 :input 'ready))
          (should (eq init-mode 'appkit-surface-test-mode))
          (should (eq init-owner surface))
          (should-not init-ready)
          (should (consp init-send-condition))
          (should (= update-count 0))
          (should (appkit-surface-live-p surface))
          (with-current-buffer (appkit-surface-buffer surface)
            (should (eq (appkit-current-surface) surface))
            (should (equal (buffer-string) "ready")))
          (should (equal (nreverse events)
                         '((mount ready) (render ready initial)))))
      (appkit-surface-test--cleanup surface))))

(ert-deftest appkit-surface-merges-one-render-per-loop-pass ()
  (let (events surface)
    (unwind-protect
        (progn
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-test--type
                  :init (lambda (_surface _input)
                          (appkit-next :model nil :render appkit-render-none))
                  :update
                  (lambda (_surface model message)
                    (appkit-next
                     :model (append model (list message))
                     :render (list message)))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-surface-test--renderer
                     (lambda (event) (push event events)))))))
          (appkit-surface-post surface 'first)
          (appkit-surface-post surface 'second)
          (should (= (appkit-loop-run-pass (appkit-surface-loop surface)) 2))
          (should (equal (appkit-surface-model surface) '(first second)))
          (should (equal (nreverse events)
                         '((mount nil)
                           (render (first second) (first second))))))
      (appkit-surface-test--cleanup surface))))

(ert-deftest appkit-surface-explicit-no-render-skips-presentation ()
  (let (events surface)
    (unwind-protect
        (progn
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-test--type
                  :init (lambda (_surface _input)
                          (appkit-next :model 'initial :render appkit-render-none))
                  :update
                  (lambda (_surface _model _message)
                    (appkit-next :model 'changed :render appkit-render-none))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-surface-test--renderer
                     (lambda (event) (push event events)))))))
          (let ((ticket (appkit-surface-send surface 'change)))
            (should (eq (appkit-loop-ticket-state ticket) 'accepted)))
          (should (eq (appkit-surface-model surface) 'changed))
          (should (equal events '((mount initial)))))
      (appkit-surface-test--cleanup surface))))

(ert-deftest appkit-surface-render-failure-recovers-from-committed-model ()
  (let (events surface)
    (unwind-protect
        (progn
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-test--type
                  :init (lambda (_surface _input)
                          (appkit-next :model 'initial :render appkit-render-none))
                  :update
                  (lambda (_surface _model message)
                    (appkit-next :model message :render 'render))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-surface-test--renderer
                     (lambda (event) (push event events))
                     (lambda (_surface _app-read-view _model _request)
                       (error "incremental render failed")))))))
          (let ((ticket (appkit-surface-send surface 'committed)))
            (should (eq (appkit-loop-ticket-state ticket) 'accepted)))
          (should (eq (appkit-surface-model surface) 'committed))
          (should (appkit-surface-renderer-valid-p surface))
          (with-current-buffer (appkit-surface-buffer surface)
            (should (equal (buffer-string) "recovered:committed")))
          (should (eq (caar events) 'recover)))
      (appkit-surface-test--cleanup surface))))

(ert-deftest appkit-surface-second-render-failure-faults-send ()
  (let (events surface)
    (unwind-protect
        (progn
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-test--type
                  :init (lambda (_surface _input)
                          (appkit-next :model 'initial :render appkit-render-none))
                  :update
                  (lambda (_surface _model message)
                    (appkit-next :model message :render 'render))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-surface-test--renderer
                     (lambda (event) (push event events))
                     (lambda (_surface _app-read-view _model _request)
                       (error "render failed"))
                     (lambda (_surface _app-read-view _model _condition)
                       (error "recover failed")))))))
          (let ((condition
                 (should-error
                  (appkit-surface-send surface 'committed)
                  :type 'error)))
            (should (string-match-p "render failed"
                                    (error-message-string condition)))
            (should (string-match-p "recover failed"
                                    (error-message-string condition))))
          (should (eq (appkit-surface-model surface) 'committed))
          (should (eq (appkit-surface-status surface) 'faulted))
          (should-not (appkit-surface-renderer-valid-p surface)))
      (appkit-surface-test--cleanup surface))))

(ert-deftest appkit-surface-domain-rejection-does-not-render ()
  (let (events surface)
    (unwind-protect
        (progn
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-test--type
                  :init (lambda (_surface _input)
                          (appkit-next :model 'initial :render appkit-render-none))
                  :update
                  (lambda (_surface _model _message)
                    (appkit-next-reject 'not-allowed))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-surface-test--renderer
                     (lambda (event) (push event events)))))))
          (let ((ticket (appkit-surface-send surface 'reject)))
            (should (eq (appkit-loop-ticket-state ticket) 'rejected))
            (should (eq (appkit-loop-ticket-outcome ticket) 'not-allowed)))
          (should (eq (appkit-surface-model surface) 'initial))
          (should (= (appkit-loop-revision (appkit-surface-loop surface)) 0))
          (should (equal events '((mount initial)))))
      (appkit-surface-test--cleanup surface))))

(ert-deftest appkit-surface-open-failure-removes-created-buffer ()
  (let ((name (generate-new-buffer-name " *appkit-surface-open-failure*"))
        unmounted)
    (should-error
     (appkit-open-generated-surface
      (appkit-surface-test--type
       :init (lambda (_surface _input)
               (appkit-next :model nil :render appkit-render-none))
       :update (lambda (_surface model _message)
                 (appkit-next :model model :render appkit-render-none))
       :renderer-factory
       (lambda (_surface)
         (appkit-generated-renderer-create
          :mount
          (lambda (_surface _app-read-view _model) (error "mount failed"))
          :merge (lambda (_left right) right)
          :render (lambda (&rest _arguments))
          :recover (lambda (&rest _arguments))
          :unmount (lambda (_surface) (setq unmounted t)))))
      :buffer-name name)
     :type 'error)
    (should unmounted)
    (should-not (get-buffer name))))

(ert-deftest appkit-surface-kill-buffer-stops-and-unmounts ()
  (let (events surface)
    (setq surface
          (appkit-open-generated-surface
           (appkit-surface-test--type
            :init (lambda (_surface _input)
                    (appkit-next :model nil :render appkit-render-none))
            :update (lambda (_surface model _message)
                      (appkit-next :model model :render appkit-render-none))
            :renderer-factory
            (lambda (_surface)
              (appkit-surface-test--renderer
               (lambda (event) (push event events)))))))
    (kill-buffer (appkit-surface-buffer surface))
    (should-not (appkit-surface-live-p surface))
    (should (eq (appkit-surface-status surface) 'stopped))
    (should (equal events '((unmount) (mount nil))))))

(ert-deftest appkit-surface-post-renders-through-real-timer ()
  (let (events surface)
    (unwind-protect
        (progn
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-test--type
                  :init (lambda (_surface _input)
                          (appkit-next :model nil :render appkit-render-none))
                  :update
                  (lambda (_surface model message)
                    (appkit-next :model (cons message model) :render 'full))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-surface-test--renderer
                     (lambda (event) (push event events)))))))
          (appkit-surface-post surface 'timer)
          (with-timeout (1 (ert-fail "Surface timer render did not run"))
            (while (= (appkit-loop-revision (appkit-surface-loop surface)) 0)
              (accept-process-output nil 0.01)))
          (with-current-buffer (appkit-surface-buffer surface)
            (should (equal (buffer-string) "(timer)"))))
      (appkit-surface-test--cleanup surface))))

(ert-deftest appkit-surface-starts-initial-effects-after-render ()
  (let* (events
         (record (lambda (event) (push event events)))
         (effect (appkit-surface-test--effect 'initial record)))
    (appkit-surface-test--with-surface
        (surface
         (appkit-surface-test--type
          :init
          (lambda (_surface _input)
            (appkit-next
             :model 'ready
             :render 'initial
             :commands (list (appkit-command-start-effect effect))))
          :update
          (lambda (_surface model _message)
            (appkit-next :model model :render appkit-render-none))
          :renderer-factory
          (lambda (_surface)
            (appkit-surface-test--renderer record))))
      (should (equal (nreverse events)
                     '((mount ready) (render ready initial)
                       (start initial))))
      (should (= (appkit-effect-runtime-count
                  (appkit-surface-effect-runtime surface))
                 1)))))

(ert-deftest appkit-surface-folds-effect-commands-before-startup ()
  (let* (events
         (record (lambda (event) (push event events)))
         (old-a (appkit-surface-test--effect 'a record :label 'old-a))
         (effect-b (appkit-surface-test--effect 'b record :label 'b))
         (new-a (appkit-surface-test--effect 'a record :label 'new-a)))
    (appkit-surface-test--with-surface
        (surface
         (appkit-surface-test--type
          :init
          (lambda (_surface _input)
            (appkit-next :model nil :render appkit-render-none))
          :update (lambda (_surface _model next) next)
          :renderer-factory
          (lambda (_surface)
            (appkit-surface-test--renderer record))))
      (setq events nil)
      (dolist (next
               (list
                (appkit-next
                 :model 'first :render '(first)
                 :commands (list (appkit-command-start-effect old-a)))
                (appkit-next
                 :model 'second :render '(second)
                 :commands (list (appkit-command-start-effect effect-b)))
                (appkit-next
                 :model 'third :render '(third)
                 :commands (list (appkit-command-start-effect new-a)))))
        (appkit-surface-post surface next))
      (should (= (appkit-loop-run-pass (appkit-surface-loop surface)) 3))
      (should (equal (appkit-surface-model surface) 'third))
      (should (equal (nreverse events)
                     '((render third (first second third))
                       (start b) (start new-a))))
      (should (= (appkit-effect-runtime-count
                  (appkit-surface-effect-runtime surface))
                 2)))))

(ert-deftest appkit-surface-defers-synchronous-effect-result ()
  (let* (events
         (record (lambda (event) (push event events)))
         (effect
          (appkit-surface-test--effect 'sync record :synchronous t)))
    (appkit-surface-test--with-surface
        (surface
         (appkit-surface-test--type
          :init
          (lambda (_surface _input)
            (appkit-next :model 'idle :render appkit-render-none))
          :update
          (lambda (_surface _model message)
            (pcase message
              ('begin
               (appkit-next
                :model 'starting
                :render 'starting
                :commands (list (appkit-command-start-effect effect))))
              (`(effect-done sync ready)
               (appkit-next :model 'done :render 'done))))
          :renderer-factory
          (lambda (_surface)
            (appkit-surface-test--renderer record))))
      (setq events nil)
      (let ((ticket (appkit-surface-send surface 'begin)))
        (should (eq (appkit-loop-ticket-state ticket) 'accepted)))
      (should (eq (appkit-surface-model surface) 'starting))
      (should (equal (nreverse events)
                     '((render starting starting) (start sync))))
      (should (= (appkit-loop-pending-count
                  (appkit-surface-loop surface))
                 1))
      (setq events nil)
      (should (= (appkit-loop-run-pass (appkit-surface-loop surface)) 1))
      (should (eq (appkit-surface-model surface) 'done))
      (should (equal (nreverse events) '((render done done)))))))

(ert-deftest appkit-surface-cancels-effect-before-render ()
  (let* (events
         (record (lambda (event) (push event events)))
         (effect (appkit-surface-test--effect 'owned record)))
    (appkit-surface-test--with-surface
        (surface
         (appkit-surface-test--type
          :init
          (lambda (_surface _input)
            (appkit-next :model 'idle :render appkit-render-none))
          :update
          (lambda (_surface _model message)
            (pcase message
              ('start
               (appkit-next
                :model 'active :render 'active
                :commands (list (appkit-command-start-effect effect))))
              ('cancel
               (appkit-next
                :model 'cancelled :render 'cancelled
                :commands (list (appkit-command-cancel-effect 'owned))))))
          :renderer-factory
          (lambda (_surface)
            (appkit-surface-test--renderer record))))
      (appkit-surface-send surface 'start)
      (setq events nil)
      (appkit-surface-send surface 'cancel)
      (should (equal (nreverse events)
                     '((cancel owned) (render cancelled cancelled))))
      (should (= (appkit-effect-runtime-count
                  (appkit-surface-effect-runtime surface))
                 0)))))

(ert-deftest appkit-surface-fault-revokes-active-effects ()
  (let* (events
         (record (lambda (event) (push event events)))
         (effect (appkit-surface-test--effect 'owned record)))
    (appkit-surface-test--with-surface
        (surface
         (appkit-surface-test--type
          :init
          (lambda (_surface _input)
            (appkit-next :model 'idle :render appkit-render-none))
          :update
          (lambda (_surface model message)
            (if (eq message 'start)
                (appkit-next
                 :model 'active :render appkit-render-none
                 :commands (list (appkit-command-start-effect effect)))
              (error "broken update from %S" model)))
          :renderer-factory
          (lambda (_surface)
            (appkit-surface-test--renderer record))))
      (appkit-surface-send surface 'start)
      (setq events nil)
      (should-error (appkit-surface-send surface 'boom) :type 'error)
      (should (eq (appkit-surface-status surface) 'faulted))
      (should (equal events '((cancel owned))))
      (should (= (appkit-effect-runtime-count
                  (appkit-surface-effect-runtime surface))
                 0)))))

(ert-deftest appkit-surface-cancellation-failure-does-not-skip-cleanup ()
  (let* (events
         (record (lambda (event) (push event events)))
         (effect-a
          (appkit-surface-test--effect
           'a record :cancel-error t))
         (effect-b (appkit-surface-test--effect 'b record)))
    (appkit-surface-test--with-surface
        (surface
         (appkit-surface-test--type
          :init
          (lambda (_surface _input)
            (appkit-next
             :model 'active
             :render appkit-render-none
             :commands
             (list (appkit-command-start-effect effect-a)
                   (appkit-command-start-effect effect-b))))
          :update
          (lambda (_surface _model _message)
            (appkit-next
             :model 'cancelled
             :render 'must-not-render
             :commands
             (list (appkit-command-cancel-effect 'a)
                   (appkit-command-cancel-effect 'b))))
          :renderer-factory
          (lambda (_surface)
            (appkit-surface-test--renderer record))))
      (setq events nil)
      (should-error (appkit-surface-send surface 'cancel) :type 'error)
      (should (eq (appkit-surface-status surface) 'faulted))
      (should (equal (nreverse events) '((cancel a) (cancel b))))
      (should (= (appkit-effect-runtime-count
                  (appkit-surface-effect-runtime surface))
                 0)))))

(ert-deftest appkit-surface-discards-folded-work-on-command-overflow ()
  (let ((events nil))
    (appkit-surface-test--with-surface
        (surface
         (appkit-surface-test--type
          :init
          (lambda (_surface _input)
            (appkit-next :model nil :render appkit-render-none))
          :update
          (lambda (_surface _model key)
            (appkit-next
             :model key
             :render key
             :commands (list (appkit-command-cancel-effect key))))
          :renderer-factory
          (lambda (_surface)
            (appkit-surface-test--renderer
             (lambda (event) (push event events)))))
         :folded-command-limit 1)
      (appkit-surface-post surface 'first)
      (appkit-surface-post surface 'second)
      (should (= (appkit-loop-run-pass (appkit-surface-loop surface)) 2))
      (should (eq (appkit-surface-status surface) 'faulted))
      (should (eq (appkit-surface-model surface) 'first))
      (should (equal events '((mount nil)))))))

(ert-deftest appkit-surface-renderer-and-starter-cannot-send-before-ready ()
  (let* (surface
         attempts
         (update-count 0)
         (attempt
          (lambda (phase current)
            (push
             (list
              phase
              (eq (appkit-current-surface) current)
              (appkit-surface-ready-p current)
              (condition-case nil
                  (progn
                    (appkit-surface-send current phase)
                    'sent)
                (error 'blocked)))
             attempts)))
         (effect
          (appkit-effect-create
           :key 'initial
           :input nil
           :start
           (lambda (&rest _arguments)
             (funcall attempt 'starter (appkit-current-surface))
             (appkit-cancellation-create :kind 'logical :cancel nil))
           :success (lambda (&rest _arguments) nil)
           :failure (lambda (&rest _arguments) nil))))
    (appkit-surface-test--with-surface
        (current
         (appkit-surface-test--type
          :init
          (lambda (_context input)
            (appkit-next
             :model input
             :render 'initial
             :commands
             (list (appkit-command-start-effect effect))))
          :update
          (lambda (_context model _message)
            (setq update-count (1+ update-count))
            (appkit-next :model model :render appkit-render-none))
          :renderer-factory
          (lambda (leaked)
            (funcall attempt 'factory leaked)
            (appkit-generated-renderer-create
             :mount
             (lambda (mounted &rest _arguments)
               (funcall attempt 'mount mounted))
             :merge (lambda (_left right) right)
             :render
             (lambda (rendered &rest _arguments)
               (funcall attempt 'render rendered)
               nil)
             :recover (lambda (&rest _arguments))
             :unmount (lambda (&rest _arguments)))))
         :input 'ready)
      (setq surface current
            attempts (nreverse attempts))
      (should (= update-count 0))
      (should (appkit-surface-ready-p surface))
      (should (appkit-surface-live-p surface))
      (should (equal (mapcar #'car attempts)
                     '(factory mount render starter)))
      (dolist (entry attempts)
        (should (nth 1 entry))
        (should-not (nth 2 entry))
        (should (eq (nth 3 entry) 'blocked))))))

(ert-deftest appkit-surface-fault-remains-attached-until-owner-stop ()
  (let (app surface buffer)
    (unwind-protect
        (progn
          (setq app
                (appkit-app-start
                 (appkit-app-type-create
                  :name 'fault-parent
                  :init
                  (lambda (_context input)
                    (appkit-next
                     :model input :render appkit-render-none))
                  :update
                  (lambda (_context model _message)
                    (appkit-next
                     :model model :render appkit-render-none)))
                 :input nil))
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-test--type
                  :init
                  (lambda (_context input)
                    (appkit-next
                     :model input :render appkit-render-none))
                  :update
                  (lambda (&rest _arguments)
                    (error "Surface transition fault"))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-surface-test--renderer #'ignore)))
                 :app app :identity 'faulted)
                buffer (appkit-surface-buffer surface))
          (should-error (appkit-surface-send surface 'fault)
                        :type 'error)
          (should (eq (appkit-surface-status surface) 'faulted))
          (should (appkit-surface-alive-p surface))
          (should-not (appkit-surface-ready-p surface))
          (should-not (appkit-surface-live-p surface))
          (should (= (appkit-app-surface-count app) 1))
          (with-current-buffer buffer
            (should (eq (appkit-current-surface) surface)))
          (should-error (appkit-surface-send surface 'after-fault)
                        :type 'error)
          (should (appkit-app-close app))
          (should (eq (appkit-surface-status surface) 'stopped))
          (should (= (appkit-app-surface-count app) 0))
          (should (buffer-live-p buffer))
          (with-current-buffer buffer
            (should-not (appkit-current-surface))))
      (when (and app
                 (not (eq (appkit-app-status app) 'stopped)))
        (condition-case nil
            (appkit-app-close app)
          ((error quit) nil)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest appkit-surface-indirect-clone-does-not-stop-base-host ()
  (let (surface alias-one alias-two events)
    (unwind-protect
        (progn
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-test--type
                  :init
                  (lambda (_context input)
                    (appkit-next :model input :render appkit-render-none))
                  :update
                  (lambda (_context model _message)
                    (appkit-next :model model :render appkit-render-none))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-surface-test--renderer
                     (lambda (event) (push event events))))))
                alias-one
                (make-indirect-buffer
                 (appkit-surface-buffer surface)
                 (generate-new-buffer-name " *appkit-surface-alias*")
                 t))
          (with-current-buffer alias-one
            (should (eq appkit--current-surface surface)))
          (kill-buffer alias-one)
          (setq alias-one nil)
          (should (appkit-surface-live-p surface))
          (should-not (member '(unmount) events))
          (setq alias-two
                (make-indirect-buffer
                 (appkit-surface-buffer surface)
                 (generate-new-buffer-name " *appkit-surface-alias*")
                 t))
          (with-current-buffer alias-two
            (fundamental-mode)
            (should-not appkit--current-surface))
          (should (appkit-surface-live-p surface))
          (should-not (member '(unmount) events)))
      (when (buffer-live-p alias-one)
        (kill-buffer alias-one))
      (when (buffer-live-p alias-two)
        (kill-buffer alias-two))
      (appkit-surface-test--cleanup surface))))

(ert-deftest appkit-surface-update-runs-in-exact-host-buffer ()
  (let (seen-buffer caller)
    (appkit-surface-test--with-surface
        (surface
         (appkit-surface-test--type
          :init
          (lambda (_context input)
            (appkit-next :model input :render appkit-render-none))
          :update
          (lambda (_context model _message)
            (setq seen-buffer (current-buffer))
            (appkit-next :model model :render appkit-render-none))
          :renderer-factory
          (lambda (_surface)
            (appkit-surface-test--renderer #'ignore))))
      (unwind-protect
          (progn
            (setq caller (generate-new-buffer " *appkit-surface-caller*"))
            (with-current-buffer caller
              (appkit-surface-send surface 'update))
            (should (eq seen-buffer (appkit-surface-buffer surface))))
        (when (buffer-live-p caller)
          (kill-buffer caller))))))

(ert-deftest appkit-surface-renderer-cannot-post-from-ready-pass ()
  (let (surface messages (recover-count 0))
    (unwind-protect
        (progn
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-test--type
                  :init
                  (lambda (_context input)
                    (appkit-next :model input :render appkit-render-none))
                  :update
                  (lambda (_context model message)
                    (push message messages)
                    (appkit-next
                     :model model
                     :render (if (eq message 'render)
                                 'frame
                               appkit-render-none)))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-generated-renderer-create
                     :mount (lambda (&rest _arguments))
                     :merge (lambda (_left right) right)
                     :render
                     (lambda (&rest _arguments)
                       (appkit-surface-post surface 'hidden))
                     :recover
                     (lambda (&rest _arguments)
                       (setq recover-count (1+ recover-count)))
                     :unmount (lambda (&rest _arguments)))))))
          (should-error (appkit-surface-send surface 'render)
                        :type 'appkit-runtime-contract-error)
          (should (eq (appkit-surface-status surface) 'faulted))
          (should (equal messages '(render)))
          (should (zerop recover-count))
          (should (zerop
                   (appkit-loop-pending-count
                    (appkit-surface-loop surface)))))
      (appkit-surface-test--cleanup surface))))

(ert-deftest appkit-surface-effect-starter-cannot-post-from-ready-pass ()
  (let (surface messages)
    (unwind-protect
        (progn
          (setq surface
                (appkit-open-generated-surface
                 (appkit-surface-test--type
                  :init
                  (lambda (_context input)
                    (appkit-next :model input :render appkit-render-none))
                  :update
                  (lambda (_context model message)
                    (push message messages)
                    (appkit-next
                     :model model
                     :render appkit-render-none
                     :commands
                     (and (eq message 'start)
                          (list
                           (appkit-command-start-effect
                            (appkit-effect-create
                             :key 'hidden
                             :input nil
                             :start
                             (lambda (&rest _arguments)
                               (appkit-surface-post surface 'hidden))
                             :success (lambda (&rest _) 'done)
                             :failure (lambda (&rest _) 'failed)))))))
                  :renderer-factory
                  (lambda (_surface)
                    (appkit-surface-test--renderer #'ignore)))))
          (should-error (appkit-surface-send surface 'start)
                        :type 'appkit-runtime-contract-error)
          (should (eq (appkit-surface-status surface) 'faulted))
          (should (equal messages '(start)))
          (should (zerop
                   (appkit-loop-pending-count
                    (appkit-surface-loop surface)))))
      (appkit-surface-test--cleanup surface))))

(provide 'appkit-surface-test)

;;; appkit-surface-test.el ends here
