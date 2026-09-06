;;; appkit-chat-completion.el --- Shared chat composer completion  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Protocol-neutral completion primitives for chat composers.  Applications
;; own their member/emoji/sticker data and insertion semantics; appkit owns the
;; common token, candidate, CAPF, picker, and ordered TAB-dispatch mechanics.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-chatbuf)

(defcustom appkit-chat-completion-ignore-case t
  "When non-nil, chat composer completion ignores letter case."
  :type 'boolean
  :group 'appkit)

(defcustom appkit-chat-completion-styles '(substring basic)
  "Completion styles used by the `appkit-chat' completion category."
  :type '(repeat symbol)
  :group 'appkit)

(cl-defstruct
    (appkit-chat-completion-candidate
     (:constructor appkit-chat-completion-candidate-create))
  "One protocol-neutral chat composer completion candidate.

LABEL is the visible completion string and must be unique within one table;
clients should append a stable identity when display names collide.  INSERT is
the default replacement.  PREFIX and ANNOTATION are strings or functions of
CANDIDATE, shown before or after LABEL by capable completion UIs.  Functions
keep expensive image annotations lazy until the completion UI requests a row.
GROUP is a string or function of CANDIDATE naming its completion section.
SEARCH-TERMS contains alternate strings matched by the shared table.  VALUE
carries the opaque application object used by an insertion callback."
  label insert prefix annotation search-terms value group)

(defvar-local appkit-chat-completion-functions nil
  "Ordered functions tried by `appkit-chat-completion-complete'.

Each function takes no arguments and returns non-nil when it handled point.
This mirrors telega's chat input completion dispatcher while remaining
independent of any protocol.")

(defun appkit-chat-completion--left-boundary-p (position input-start)
  "Return non-nil when POSITION starts a token after INPUT-START."
  (or (= position input-start)
      (let ((char (char-before position)))
        (or (null char)
            (not (memq (char-syntax char) '(?w ?_)))))))

(defun appkit-chat-completion-token-bounds (triggers)
  "Return completion token metadata before point for TRIGGERS.

TRIGGERS is a list of trigger characters such as `(?@ ?# ?:)' or a single
character.  The returned plist contains `:start', `:end', `:trigger', `:raw'
and `:query'.  Tokens may contain non-ASCII text, stop at whitespace or a
structured input object, and must not begin in the middle of a word (so an
email address is not treated as an @mention).  Return nil outside the active
appkit composer input.  A closing delimiter such as the final colon in
`:wave:' needs a client-specific bounds function."
  (when (appkit-chatbuf-point-in-input-p)
    (let* ((triggers (if (listp triggers) triggers (list triggers)))
           (input-start (appkit-chatbuf-input-start-position))
           (end (point))
           (position end)
           found)
      (while (and (not found) (> position input-start))
        (let* ((char-position (1- position))
               (char (char-after char-position)))
          (cond
           ((get-text-property char-position
                               appkit-chatbuf-input-object-property)
            (setq position input-start))
           ((memq char triggers)
            (setq found char-position)
            ;; Keep repeated triggers together (`@@admin' style).
            (while (and (> found input-start)
                        (eq (char-before found) char))
              (setq found (1- found))))
           ((eq (char-syntax char) ?\ )
            (setq position input-start))
           (t
            (setq position char-position)))))
      (when (and found
                 (appkit-chat-completion--left-boundary-p found input-start))
        (let ((trigger (char-after found)))
          (list :start found
                :end end
                :trigger trigger
                :raw (buffer-substring-no-properties found end)
                :query (buffer-substring-no-properties (1+ found) end)))))))

(defun appkit-chat-completion-delimited-token-bounds (delimiter)
  "Return metadata for a delimited token before point.

DELIMITER is the character wrapping the token.  Both an open token such as
`:rock' and a closed token such as `:rocket:' are accepted.  The token must
stay inside the active composer, contain no whitespace or structured input
object, and start at a normal token boundary."
  (unless (characterp delimiter)
    (error "Chat completion delimiter must be a character"))
  (or
   (when (and (appkit-chatbuf-point-in-input-p)
              (eq (char-before) delimiter))
     (let* ((input-start (appkit-chatbuf-input-start-position))
            (end (point))
            (position (- end 2))
            start)
       (while (and (not start) (>= position input-start))
         (cond
          ((get-text-property position
                              appkit-chatbuf-input-object-property)
           (setq position (1- input-start)))
          ((eq (char-after position) delimiter)
           (setq start position))
          ((eq (char-syntax (char-after position)) ?\ )
           (setq position (1- input-start)))
          (t
           (setq position (1- position)))))
       (when (and start
                  (< (1+ start) (1- end))
                  (appkit-chat-completion--left-boundary-p start input-start))
         (list :start start
               :end end
               :trigger delimiter
               :raw (buffer-substring-no-properties start end)
               :query (buffer-substring-no-properties
                       (1+ start) (1- end))))))
   (let ((input-start (appkit-chatbuf-input-start-position)))
     (unless (and input-start
                  (eq (char-before) delimiter)
                  (> (point) (1+ input-start))
                  (eq (char-before (1- (point))) delimiter))
       (appkit-chat-completion-token-bounds delimiter)))))

(defun appkit-chat-completion--candidate-label (candidate)
  "Return and validate CANDIDATE's plain LABEL."
  (let ((label (and (appkit-chat-completion-candidate-p candidate)
                    (appkit-chat-completion-candidate-label candidate))))
    (unless (and (stringp label) (not (string-empty-p label)))
      (error "Appkit chat completion candidate needs a non-empty label"))
    (substring-no-properties label)))

(defun appkit-chat-completion--candidate-map (candidates)
  "Return hash table mapping labels to CANDIDATES, rejecting duplicates."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (candidate candidates)
      (let ((label (appkit-chat-completion--candidate-label candidate)))
        (when (gethash label table)
          (error "Duplicate appkit chat completion label: %s" label))
        (puthash label candidate table)))
    table))

(defun appkit-chat-completion--candidate-search-values (candidate)
  "Return searchable strings carried by CANDIDATE."
  (let ((terms (appkit-chat-completion-candidate-search-terms candidate)))
    (cons (appkit-chat-completion--candidate-label candidate)
          (cond
           ((null terms) nil)
           ((stringp terms) (list terms))
           ((listp terms) (seq-filter #'stringp terms))
           (t nil)))))

(defun appkit-chat-completion--candidate-matches-p (candidate input)
  "Return non-nil when CANDIDATE matches INPUT or one of its aliases."
  (let* ((input (or input ""))
         (plain (substring-no-properties input))
         (normalize (if appkit-chat-completion-ignore-case #'downcase #'identity))
         (without-trigger
          (replace-regexp-in-string "\\`[@#:/]+" "" plain))
         (needles
          (delete-dups
           (list (funcall normalize plain)
                 (funcall normalize without-trigger)))))
    (seq-some
     (lambda (value)
       (let ((haystack (funcall normalize value))
             (case-fold-search nil))
         (seq-some (lambda (needle)
                     (or (string-empty-p needle)
                         (string-match-p (regexp-quote needle) haystack)))
                   needles)))
     (appkit-chat-completion--candidate-search-values candidate))))

(defun appkit-chat-completion--table-action
    (string pred action candidates category candidate-map)
  "Serve completion table ACTION for STRING over CANDIDATES.
PRED optionally filters candidate labels.  CATEGORY and CANDIDATE-MAP carry
the table metadata and exact label lookup."
  (pcase action
    ('metadata
     `(metadata
       (category . ,category)
       (display-sort-function . identity)
       (cycle-sort-function . identity)
       (group-function
        . ,(appkit-chat-completion--group-function candidate-map))))
    ('t
     (let ((labels
            (mapcar
             #'appkit-chat-completion--candidate-label
             (seq-filter
              (lambda (candidate)
                (and (appkit-chat-completion--candidate-matches-p
                      candidate string)
                     (or (null pred)
                         (funcall pred
                                  (appkit-chat-completion--candidate-label
                                   candidate)))))
              candidates))))
       labels))
    ('nil
     (let* ((completion-ignore-case appkit-chat-completion-ignore-case)
            (labels (mapcar #'appkit-chat-completion--candidate-label candidates))
            (regular (try-completion string labels pred)))
       (or regular
           (let ((matches
                  (seq-filter
                   (lambda (candidate)
                     (and (appkit-chat-completion--candidate-matches-p
                           candidate string)
                          (or (null pred)
                              (funcall pred
                                       (appkit-chat-completion--candidate-label
                                        candidate)))))
                   candidates)))
             (cond
              ((null matches) nil)
              ((null (cdr matches))
               (appkit-chat-completion--candidate-label (car matches)))
              (t string))))))
    ('lambda
      (and (stringp string)
           (let* ((plain (substring-no-properties string))
                  (folded-plain
                   (and appkit-chat-completion-ignore-case
                        (downcase plain)))
                  (candidate
                   (if appkit-chat-completion-ignore-case
                       (seq-find
                        (lambda (item)
                          (string-equal
                           folded-plain
                           (downcase
                            (appkit-chat-completion--candidate-label item))))
                        candidates)
                     (gethash plain candidate-map))))
             (and candidate
                  (or (null pred)
                      (funcall pred
                               (appkit-chat-completion--candidate-label candidate)))
                  t))))
    (`(boundaries . ,_) nil)
    (_
     (let ((completion-ignore-case appkit-chat-completion-ignore-case))
       (complete-with-action
        action
        (mapcar #'appkit-chat-completion--candidate-label candidates)
        string pred)))))

(defun appkit-chat-completion--candidate-decoration (candidate accessor)
  "Return CANDIDATE decoration from ACCESSOR, evaluating it lazily."
  (let ((value (funcall accessor candidate)))
    (cond
     ((functionp value) (or (funcall value candidate) ""))
     ((stringp value) value)
     (t ""))))

(defun appkit-chat-completion--candidate-group (candidate)
  "Return CANDIDATE's completion group, or nil."
  (let ((group (appkit-chat-completion-candidate-group candidate)))
    (cond
     ((functionp group) (funcall group candidate))
     ((stringp group) group))))

(defun appkit-chat-completion--group-function (candidate-map)
  "Return a completion grouping function backed by CANDIDATE-MAP."
  (lambda (label transform)
    (if transform
        label
      (when-let* ((candidate
                   (gethash (substring-no-properties label) candidate-map)))
        (appkit-chat-completion--candidate-group candidate)))))

(defun appkit-chat-completion-affixation (labels candidate-map)
  "Return completion affixation rows for LABELS using CANDIDATE-MAP."
  (mapcar
   (lambda (label)
     (let* ((key (substring-no-properties label))
            (candidate (gethash key candidate-map)))
       (list label
             (if candidate
                 (appkit-chat-completion--candidate-decoration
                  candidate #'appkit-chat-completion-candidate-prefix)
               "")
             (if candidate
                 (appkit-chat-completion--candidate-decoration
                  candidate #'appkit-chat-completion-candidate-annotation)
               ""))))
   labels))

(cl-defun appkit-chat-completion-apply-candidate
    (completed candidate &key start insert-function suffix sync-function)
  "Replace COMPLETED at point with CANDIDATE.

INSERT-FUNCTION, when non-nil, is called with CANDIDATE after the completion
label is removed.  Otherwise insert CANDIDATE's INSERT value (falling back to
its label).  Append SUFFIX when non-nil and it is not already present.  Call
SYNC-FUNCTION after appkit synchronizes canonical input.  START may be the
captured token marker; otherwise it is derived from COMPLETED.  Return non-nil
on success."
  (let* ((completed (and (stringp completed)
                         (substring-no-properties completed)))
         (end (point))
         (start (or (and (markerp start) (marker-position start))
                    (and (integerp start) start)
                    (and completed (- end (length completed)))))
         (input-start (appkit-chatbuf-input-start-position)))
    (when (and start
               input-start
               (appkit-chatbuf-point-in-input-p end)
               (>= start input-start)
               (string= completed
                        (buffer-substring-no-properties start end)))
      (condition-case error-data
          (atomic-change-group
            (delete-region start end)
            (if (functionp insert-function)
                (funcall insert-function candidate)
              (insert (or (appkit-chat-completion-candidate-insert candidate)
                          (appkit-chat-completion-candidate-label candidate))))
            (when (and (stringp suffix)
                       (not (string-empty-p suffix))
                       (not (and (>= (- (point) (length suffix)) (point-min))
                                 (string= suffix
                                          (buffer-substring-no-properties
                                           (- (point) (length suffix)) (point)))))
                       (not (looking-at-p (regexp-quote suffix))))
              (insert suffix)))
        (error
         (appkit-chatbuf-input-state-sync)
         (signal (car error-data) (cdr error-data))))
      (appkit-chatbuf-input-state-sync)
      (when (functionp sync-function)
        (funcall sync-function))
      t)))

(cl-defun appkit-chat-completion-capf
    (start end candidates &key category insert-function suffix sync-function)
  "Build CAPF data for CANDIDATES spanning START through END.

CATEGORY defaults to `appkit-chat'.  INSERT-FUNCTION, SUFFIX, and
SYNC-FUNCTION are forwarded to `appkit-chat-completion-apply-candidate'."
  (when candidates
    (let* ((candidate-map (appkit-chat-completion--candidate-map candidates))
           (start-marker (copy-marker start))
           (category (or category 'appkit-chat)))
      (list
       start end
       (lambda (string pred action)
         (appkit-chat-completion--table-action
          string pred action candidates category candidate-map))
       :affixation-function
       (lambda (completion-labels)
         (appkit-chat-completion-affixation completion-labels candidate-map))
       :annotation-function
       (lambda (label)
         (when-let* ((candidate
                      (gethash (substring-no-properties label) candidate-map)))
           ;; Company-capf consumes only `annotation-function', while modern
           ;; CAPF UIs prefer `affixation-function'.  Join both here so legacy
           ;; frontends retain avatars/prefixes without changing rich UIs.
           (concat
            (appkit-chat-completion--candidate-decoration
             candidate #'appkit-chat-completion-candidate-prefix)
            (appkit-chat-completion--candidate-decoration
             candidate #'appkit-chat-completion-candidate-annotation))))
       :exit-function
       (lambda (label status)
         ;; `sole' and `exact' explicitly mean completion may continue; only
         ;; an accepted `finished' candidate may become a protocol object.
         (when (eq status 'finished)
           (when-let* ((key (and (stringp label)
                                 (substring-no-properties label)))
                       (candidate (gethash key candidate-map)))
             (appkit-chat-completion-apply-candidate
              key candidate
              :start start-marker
              :insert-function insert-function
              :suffix suffix
              :sync-function sync-function))))
       :exclusive 'no))))

(defun appkit-chat-completion--visual-title (candidate)
  "Return CANDIDATE as one searchable visual-reader completion title.

The visible prefix is supplied through native completion affixation so
frontends may strip candidate properties without discarding it."
  (let* ((label (appkit-chat-completion--candidate-label candidate))
         (search-terms
          (delete-dups
           (seq-filter
            (lambda (term)
              (and (not (string-empty-p term))
                   (not (string-equal term label))))
            (mapcar
             #'substring-no-properties
             (cdr
              (appkit-chat-completion--candidate-search-values candidate)))))))
    (concat
     label
     (when search-terms
       (propertize
        (concat "\t" (string-join search-terms "\t"))
        'display "")))))

(defun appkit-chat-completion--visual-affixation (titles candidate-map)
  "Return visual affixation rows for TITLES using CANDIDATE-MAP."
  (mapcar
   (lambda (title)
     (let ((candidate
            (gethash (substring-no-properties title) candidate-map)))
       (list
        title
        (if candidate
            (appkit-chat-completion--candidate-decoration
             candidate #'appkit-chat-completion-candidate-prefix)
          "")
        "")))
   titles))

(defun appkit-chat-completion--visual-choices (candidates)
  "Return validated (TITLE . CANDIDATE) entries for CANDIDATES."
  (let ((labels (make-hash-table :test #'equal))
        (titles (make-hash-table :test #'equal))
        choices)
    (dolist (candidate candidates)
      (let* ((label (appkit-chat-completion--candidate-label candidate))
             (title (appkit-chat-completion--visual-title candidate)))
        (when (gethash label labels)
          (error "Duplicate appkit chat completion label: %s" label))
        (when (gethash title titles)
          (error "Duplicate appkit chat visual title: %s" label))
        (puthash label t labels)
        (puthash title t titles)
        (push (cons title candidate) choices)))
    (nreverse choices)))

(cl-defun appkit-chat-completion-read-visual
    (prompt candidates
            &key category history initial-input default-candidate)
  "Read one item from a bounded visual CANDIDATES catalog.

Each completion title contains its visible label and hidden search terms.
Native affixation supplies candidate prefixes independently of candidate text
properties and maps the selected title back to its opaque candidate object.
CATEGORY, HISTORY, and INITIAL-INPUT customize `completing-read'.
DEFAULT-CANDIDATE, when non-nil, must be one of CANDIDATES and becomes the
native minibuffer default."
  (unless candidates
    (user-error "No completion candidates"))
  (let* ((choices (appkit-chat-completion--visual-choices candidates))
         (candidate-map (make-hash-table :test #'equal))
         (default-entry
          (and default-candidate (rassq default-candidate choices))))
    (dolist (entry choices)
      (puthash (car entry) (cdr entry) candidate-map))
    (when (and default-candidate (not default-entry))
      (error "Visual completion default is not one of its candidates"))
    (let* ((category (or category 'appkit-chat))
           (table
            (completion-table-with-metadata
             choices
             `((category . ,category)
               (display-sort-function . identity)
               (cycle-sort-function . identity)
               (group-function
                . ,(appkit-chat-completion--group-function candidate-map))
               (affixation-function
                . ,(lambda (titles)
                     (appkit-chat-completion--visual-affixation
                      titles candidate-map))))))
           (completion-category-overrides
            (cons
             `(appkit-chat
               (styles ,@appkit-chat-completion-styles))
             (assq-delete-all
              'appkit-chat
              (copy-tree completion-category-overrides))))
           (completion-ignore-case appkit-chat-completion-ignore-case)
           (choice
            (completing-read
             prompt table nil t initial-input history
             (car default-entry)))
           (entry (assoc choice choices)))
      (if entry
          (cdr entry)
        (user-error
         "Unknown visual completion candidate: %s" choice)))))

(cl-defun appkit-chat-completion-read
    (prompt candidates &key category history initial-input)
  "Read and return one item from CANDIDATES using shared rich metadata.

PROMPT is the minibuffer prompt.  Return the selected candidate object, not
merely its label.  CATEGORY, HISTORY, and INITIAL-INPUT customize
`completing-read'."
  (unless candidates
    (user-error "No completion candidates"))
  (let* ((candidate-map (appkit-chat-completion--candidate-map candidates))
         (table
          (lambda (string pred action)
            (if (eq action 'metadata)
                `(metadata
                  (category . ,(or category 'appkit-chat))
                  (display-sort-function . identity)
                  (cycle-sort-function . identity)
                  (group-function
                   . ,(appkit-chat-completion--group-function candidate-map))
                  (annotation-function
                   . ,(lambda (label)
                        (when-let* ((candidate
                                     (gethash
                                      (substring-no-properties label)
                                      candidate-map)))
                          (concat
                           (appkit-chat-completion--candidate-decoration
                            candidate
                            #'appkit-chat-completion-candidate-prefix)
                           (appkit-chat-completion--candidate-decoration
                            candidate
                            #'appkit-chat-completion-candidate-annotation)))))
                  (affixation-function
                   . ,(lambda (completion-labels)
                        (appkit-chat-completion-affixation
                         completion-labels candidate-map))))
              (appkit-chat-completion--table-action
               string pred action candidates
               (or category 'appkit-chat) candidate-map))))
         (completion-category-overrides
          (cons `(appkit-chat (styles ,@appkit-chat-completion-styles))
                (assq-delete-all 'appkit-chat
                                 (copy-tree completion-category-overrides))))
         (completion-ignore-case appkit-chat-completion-ignore-case)
         (choice (completing-read prompt table nil t initial-input history)))
    (or (gethash (substring-no-properties choice) candidate-map)
        (user-error "Unknown completion candidate: %s" choice))))

(defun appkit-chat-completion-at-point ()
  "Run ordinary CAPF completion when point is in the shared composer."
  (when (appkit-chatbuf-point-in-input-p)
    (completion-at-point)))

(defun appkit-chat-completion-complete ()
  "Try `appkit-chat-completion-functions' in order at composer point."
  (interactive)
  (when (appkit-chatbuf-point-in-input-p)
    (seq-some (lambda (function)
                (and (functionp function) (funcall function)))
              appkit-chat-completion-functions)))

(cl-defun appkit-chat-completion-setup
    (&key capf-functions dispatch-functions append)
  "Install shared composer completion in the current buffer.

CAPF-FUNCTIONS are installed in `completion-at-point-functions'.
DISPATCH-FUNCTIONS run before the ordinary CAPF adapter on TAB.  When APPEND
is non-nil preserve existing CAPFs and dispatch functions after the supplied
ones; otherwise replace them."
  (let ((old-capfs (and append completion-at-point-functions))
        (old-dispatch (and append appkit-chat-completion-functions)))
    (setq-local completion-category-overrides
                (cons `(appkit-chat (styles ,@appkit-chat-completion-styles))
                      (assq-delete-all
                       'appkit-chat (copy-tree completion-category-overrides))))
    (setq-local completion-at-point-functions
                (append capf-functions old-capfs))
    (setq-local appkit-chat-completion-functions
                (append dispatch-functions
                        (list #'appkit-chat-completion-at-point)
                        old-dispatch))))

(provide 'appkit-chat-completion)

;;; appkit-chat-completion.el ends here
