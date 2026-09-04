;;; appkit-command.el --- Closed post-commit command values  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Closed transition results and deferred runtime commands.  Command batches
;; validate per-transition input, bound retained keyed deltas, and preserve the
;; order of each key's final command without executing client code.

;;; Code:

(require 'cl-lib)
(require 'appkit-effect)
(require 'appkit-cleanup)

(defconst appkit-render-none 'appkit-render-none
  "Explicit disposition requesting no presentation work.")

(defconst appkit-command-default-per-next-limit 32
  "Default maximum command count returned by one transition.")

(defconst appkit-command-default-folded-limit 64
  "Default maximum distinct keyed commands retained by one pass.")

(cl-defstruct (appkit-next
               (:constructor appkit-next--create)
               (:copier nil))
  "One accepted transition's model, render disposition, and commands."
  model
  render
  commands)

(cl-defun appkit-next (&key model (render nil render-supplied-p) commands)
  "Create an accepted transition result.

MODEL is the next domain model.  RENDER must be supplied explicitly, even when
its value is nil.  COMMANDS is a bounded proper list from the closed command
set; its owner runtime validates the applicable capacity."
  (unless render-supplied-p
    (error "Transition result requires an explicit render disposition"))
  (appkit-next--create :model model :render render :commands commands))

(cl-defstruct (appkit-command
               (:constructor nil)
               (:copier nil))
  "Base type for AppKit's closed deferred command set.")

(cl-defstruct (appkit-command-start-effect
               (:include appkit-command)
               (:constructor appkit-command-start-effect (effect))
               (:copier nil))
  "Replace and start one keyed EFFECT after its owner commits."
  effect)

(cl-defstruct (appkit-command-cancel-effect
               (:include appkit-command)
               (:constructor appkit-command-cancel-effect (key))
               (:copier nil))
  "Revoke and cancel the current Effect under KEY after commit."
  key)

(cl-defstruct (appkit-command--batch
               (:constructor appkit-command--batch-create-internal)
               (:copier nil))
  reverse-effects
  effect-deltas
  folded-limit)

(defun appkit-command--check-limit (name value)
  "Return positive integer VALUE or signal an error naming NAME."
  (unless (and (integerp value) (> value 0))
    (error "%s must be positive: %S" name value))
  value)

(defun appkit-command--batch-create (folded-limit)
  "Create an empty command batch retaining at most FOLDED-LIMIT keys."
  (appkit-command--check-limit "Folded command limit" folded-limit)
  (appkit-command--batch-create-internal
   :reverse-effects nil
   :effect-deltas (make-hash-table :test #'equal)
   :folded-limit folded-limit))

(defun appkit-command--effect-key (command)
  "Return validated Effect folding key for COMMAND."
  (cond
   ((appkit-command-start-effect-p command)
    (appkit-effect-spec-key
     (appkit-command-start-effect-effect command)))
   ((appkit-command-cancel-effect-p command)
    (appkit-command-cancel-effect-key command))
   (t
    (error "Unsupported AppKit command: %S" command))))

(defun appkit-command--batch-add (batch commands per-next-limit)
  "Fold one transition's COMMANDS into BATCH.

PER-NEXT-LIMIT bounds traversal as well as valid command count, so circular and
oversized lists fail without an unbounded scan."
  (let ((remaining commands)
        (count 0)
        (deltas (appkit-command--batch-effect-deltas batch)))
    (while (consp remaining)
      (setq count (1+ count))
      (when (> count per-next-limit)
        (error "Per-next command limit exceeded: %S" per-next-limit))
      (let* ((command (pop remaining))
             (key (appkit-command--effect-key command)))
        (unless (gethash key deltas)
          (when (>= (hash-table-count deltas)
                    (appkit-command--batch-folded-limit batch))
            (error "Folded command limit exceeded: %S"
                   (appkit-command--batch-folded-limit batch))))
        ;; Every list cell is a unique occurrence token.  Superseded cells stay
        ;; in the bounded input log and are skipped during the linear drain.
        (let ((entry (cons key command)))
          (puthash key entry deltas)
          (push entry (appkit-command--batch-reverse-effects batch)))))
    (when remaining
      (error "Commands must be a proper list: %S" commands))))

(defun appkit-command--batch-clear (batch)
  "Discard all deferred commands retained by BATCH."
  (setf (appkit-command--batch-reverse-effects batch) nil)
  (clrhash (appkit-command--batch-effect-deltas batch)))

(defun appkit-command--batch-drain-effects (batch)
  "Return BATCH's final Effect commands in explicit order, then clear BATCH."
  (let ((deltas (appkit-command--batch-effect-deltas batch))
        effects)
    (dolist (entry (nreverse (appkit-command--batch-reverse-effects batch)))
      (when (eq entry (gethash (car entry) deltas))
        (push (cdr entry) effects)))
    (appkit-command--batch-clear batch)
    (nreverse effects)))

(defun appkit-command--revoke-effects (runtime commands warning-type)
  "Revoke final Effect COMMANDS from RUNTIME, reporting as WARNING-TYPE."
  (let (conditions)
    (appkit--run-cleanup-items
     commands
     (lambda (command)
       (appkit-effect-runtime-cancel
        runtime (appkit-command--effect-key command)))
     (lambda (condition) (push condition conditions)))
    (setq conditions (nreverse conditions))
    (appkit--warn-cleanup-conditions (cdr conditions) warning-type)
    (when-let* ((condition (car conditions)))
      (signal (car condition) (cdr condition)))))

(defun appkit-command--start-effects (runtime commands)
  "Start final Effect COMMANDS in RUNTIME in explicit order."
  (dolist (command commands)
    (when (appkit-command-start-effect-p command)
      (appkit-effect-runtime-start
       runtime (appkit-command-start-effect-effect command)))))

(provide 'appkit-command)

;;; appkit-command.el ends here
