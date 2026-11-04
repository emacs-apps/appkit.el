;;; appkit-chat-render.el --- Shared chat projection renderer -*- lexical-binding: t; -*-

(require 'appkit-chat-timeline)

(defun appkit-chat-render--rekeys (rows mappings)
  "Compose identity MAPPINGS still applicable to the live timeline.
Coalesced changes may promote an identity through several intermediate keys.
Retired evidence never promotes a node twice; ambiguous live promotions fail."
  (let ((targets (make-hash-table :test #'equal))
        (edges (make-hash-table :test #'equal))
        (resolved (make-hash-table :test #'equal))
        (claimed (make-hash-table :test #'equal))
        (absent (make-symbol "absent")) result)
    (dolist (row rows) (puthash (appkit-chat-timeline-row-key row) t targets))
    (dolist (mapping mappings)
      (let ((old (gethash (car mapping) edges absent)))
        (when (and (not (eq old absent)) (not (equal old (cdr mapping))))
          (error "Conflicting chat identity replacements for %S" (car mapping)))
        (puthash (car mapping) (cdr mapping) edges)))
    (cl-labels
        ((resolve (key)
           (let ((visiting (make-hash-table :test #'equal)) path)
             (while (and (eq (gethash key resolved absent) absent)
                         (not (eq (gethash key edges absent) absent)))
               (when (gethash key visiting)
                 (error "Cyclic chat identity replacement at %S" key))
               (puthash key t visiting)
               (push key path)
               (setq key (gethash key edges)))
             (setq key (gethash key resolved key))
             (dolist (source path) (puthash source key resolved))
             key)))
      (dolist (mapping mappings (nreverse result))
        (let* ((source (car mapping)) (target (resolve source)))
          (when (and (appkit-chat-timeline-node source)
                     (gethash target targets) (not (gethash source targets))
                     (not (appkit-chat-timeline-node target)))
            (let ((previous (gethash target claimed absent)))
              (when (and (not (eq previous absent)) (not (equal previous source)))
                (error "Multiple live chat identities replace %S" target))
              (when (eq previous absent)
                (puthash target source claimed)
                (push (cons source target) result)))))))))

(cl-defun appkit-chat-renderer-create
    (&key ensure project rekeys frame result before-render after-render resource-request
          mount unmount (geometry-mode 'redraw) defer-while-mark-active)
  "Create a Generated Renderer using the shared chat timeline.
ENSURE and PROJECT receive Surface, pass-scoped App read view, and committed
Surface model.  ENSURE configures the timeline; PROJECT returns its rows.
REKEYS optionally receives those arguments and projected rows, returning
local-to-canonical identity mappings.  Only live replacements are applied.

FRAME, BEFORE-RENDER and AFTER-RENDER receive Surface, view, model and change.
FRAME runs for initial, full, frame or geometry changes; resource completion
does not rebuild the footer.  RESULT receives Surface, view, model, cached
rows and change, and returns the Resource declaration for the current pass.
MOUNT and UNMOUNT use the standard Generated Renderer signatures.
RESOURCE-REQUEST maps completed resource keys to a projection change; its
default requests presentation invalidation without reprojecting data.

Resource-only passes reuse rows and invalidate their dependency index.
Geometry redraws cached rows, unless GEOMETRY-MODE is `reproject'.  Explicit
keys reconcile current data and force those keys.  All mutations preserve
the composer, selection and semantic position through the chat timeline."
  (dolist (callback (list ensure project))
    (unless (functionp callback)
      (signal 'wrong-type-argument (list 'functionp callback))))
  (unless (memq geometry-mode '(redraw reproject))
    (error "Unsupported chat geometry mode: %S" geometry-mode))
  (let (rows initialized)
    (cl-labels
        ((render (surface view model change &optional reset)
           (unless (appkit-projection-change-p change)
             (error "Chat Renderer received invalid request: %S" change))
           (let* ((initial (not initialized))
                  (effective (if initial
                                 (appkit-projection-change-merge
                                  change (appkit-projection-change-create
                                          :full-p t :frame-p t))
                               change)))
             (funcall
              (if (and reset (appkit-chat-timeline--current-state))
                  #'appkit-chat-timeline-run-preserving-position #'funcall)
              (lambda ()
                (appkit-with-content-update surface
                  (when reset
                    (when (appkit-chatbuf-input-start-position)
                      (let ((input (appkit-chatbuf-input-string))
                            (cached (appkit-chatbuf-input-state)))
                        ;; Generated editability is not part of a rich draft.
                        (dolist (value (list input cached))
                          (remove-text-properties 0 (length value) (quote (read-only nil)) value))
                        (unless (equal-including-properties input cached)
                          (appkit-chatbuf-input-state-set input))))
                    (appkit-chatbuf-clear-prompt-and-input)
                    (appkit-chat-timeline-reset))
                  (when before-render (funcall before-render surface view model effective))
                  (funcall ensure surface view model)
                  (let* ((projection (appkit-chat-timeline--projection))
                         (plan (appkit-projection--render-plan projection effective geometry-mode))
                         (reconcile (nth 0 plan))
                         (force-keys (nth 1 plan))
                         (resources (nth 2 plan))
                         (position (nth 3 plan))
                         (snapshot (unless (eq position 'preserve)
                                     (appkit-projection--capture-position projection position))))
                    (if reconcile
                        (let* ((next (funcall project surface view model))
                               (mappings (appkit-chat-render--rekeys
                                          next
                                          (append (appkit-projection-change-rekeys effective)
                                                  (when rekeys (funcall rekeys surface view model next))))))
                          (appkit-chat-timeline-sync next :force-keys force-keys
                                                     :changed-resources resources :rekeys mappings)
                          (setq rows next))
                      (appkit-chat-timeline-invalidate
                       (append force-keys (appkit-chat-timeline-dependent-keys resources))
                       :defer-while-mark-active defer-while-mark-active))
                    (when (and frame
                               (or initial (appkit-projection-change-full-p effective)
                                   (appkit-projection-change-frame-p effective)
                                   (appkit-projection-change-geometry-p effective)))
                      (funcall frame surface view model effective))
                    (unless (eq position 'preserve)
                      (appkit-projection--restore-position projection position snapshot))))))
             (when after-render
               (appkit-with-content-update surface
                 (funcall after-render surface view model effective)))
             (setq initialized t)
             (when result (funcall result surface view model rows effective)))))
      (appkit-generated-renderer-create
       :mount (lambda (surface view model)
                (setq rows nil initialized nil)
                (when mount (funcall mount surface view model)))
       :render #'render
       :merge #'appkit-projection-change-merge
       :resource-request (or resource-request
                             (lambda (keys) (appkit-projection-change-create :resources keys)))
       :recover (lambda (surface view model _condition)
                  (setq rows nil initialized nil)
                  (render surface view model
                          (appkit-projection-change-create :full-p t :frame-p t) t))
       :unmount (lambda (surface)
                  (setq rows nil initialized nil)
                  (when unmount (funcall unmount surface)))))))

(provide 'appkit-chat-render)
;;; appkit-chat-render.el ends here
