;;; appkit-markup-codec.el --- Markup source codec boundary -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;;; Commentary:

;; Registry, result values, and property-preserving structured-object fencing
;; for editable markup source.  Codecs are synchronous local transforms; they
;; never own drafts, transport, or provider wire encoding.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-markup)
(require 'appkit-chatbuf)

(define-error 'appkit-markup-codec-error "Invalid Appkit markup codec operation")
(define-error 'appkit-markup-object-rejected
              "Structured compose object rejected" 'appkit-markup-codec-error)

(defcustom appkit-markup-codec-source-limit 200000
  "Maximum source characters accepted by one codec operation."
  :type 'integer
  :group 'appkit)

(defcustom appkit-markup-codec-object-limit 1024
  "Maximum structured object occurrences accepted by one parse."
  :type 'integer
  :group 'appkit)

(defcustom appkit-markup-codec-diagnostic-limit 100
  "Maximum diagnostics retained from one codec operation."
  :type 'integer
  :group 'appkit)

(cl-defstruct (appkit-markup-codec
               (:constructor appkit-markup-codec--create)
               (:copier nil))
  (name nil :read-only t)
  (label nil :read-only t)
  (parse-function nil :read-only t)
  (print-function nil :read-only t))

(cl-defstruct (appkit-markup-diagnostic
               (:constructor appkit-markup-diagnostic--create)
               (:copier nil))
  (kind nil :read-only t)
  (severity nil :read-only t)
  (start nil :read-only t)
  (end nil :read-only t))

(cl-defstruct (appkit-markup-loss
               (:constructor appkit-markup-loss--create)
               (:copier nil))
  (kind nil :read-only t)
  (path nil :read-only t))

(cl-defstruct (appkit-markup-object-occurrence
               (:constructor appkit-markup-object-occurrence--create)
               (:copier nil))
  (value nil :read-only t)
  (text nil :read-only t)
  (start nil :read-only t)
  (end nil :read-only t))

(cl-defstruct (appkit-markup-parse-result
               (:constructor appkit-markup-parse-result--create)
               (:copier nil))
  (document nil :read-only t)
  (diagnostics nil :read-only t)
  (side-channels nil :read-only t))

(cl-defstruct (appkit-markup-print-result
               (:constructor appkit-markup-print-result--create)
               (:copier nil))
  (source nil :read-only t)
  (losses nil :read-only t))

(cl-defstruct (appkit-markup-codec--object
               (:constructor appkit-markup-codec--object-create)
               (:copier nil))
  (token nil :read-only t)
  (occurrence nil :read-only t))

(cl-defstruct (appkit-markup-codec--protected
               (:constructor appkit-markup-codec--protected-create)
               (:copier nil))
  (source nil :read-only t)
  (mapping nil :read-only t)
  (semantic nil :read-only t)
  (side-channels nil :read-only t))

(defvar appkit-markup-codec--registry (make-hash-table :test #'eq)
  "Registered markup codecs keyed by symbolic name.")

(defun appkit-markup-diagnostic (kind severity start end)
  "Return a source diagnostic with no source content in its fields."
  (unless (and (symbolp kind)
               (memq severity '(info warning error))
               (integerp start) (integerp end)
               (<= 0 start end))
    (signal 'appkit-markup-codec-error '(invalid-diagnostic)))
  (appkit-markup-diagnostic--create
   :kind kind :severity severity :start start :end end))

(defun appkit-markup-loss (kind path)
  "Return semantic loss KIND at structural PATH."
  (unless (and (symbolp kind) (proper-list-p path)
               (cl-every (lambda (part)
                           (or (symbolp part)
                               (and (integerp part) (>= part 0))))
                         path))
    (signal 'appkit-markup-codec-error '(invalid-loss)))
  (appkit-markup-loss--create :kind kind :path (copy-sequence path)))

(cl-defun appkit-markup-parse-result (document &key diagnostics side-channels)
  "Return a validated parse result for DOCUMENT."
  (let ((document (appkit-markup-normalize document))
        (diagnostics (copy-sequence (or diagnostics nil)))
        (side-channels (copy-tree (or side-channels nil))))
    (unless (and (proper-list-p diagnostics)
                 (cl-every #'appkit-markup-diagnostic-p diagnostics)
                 (proper-list-p side-channels))
      (signal 'appkit-markup-codec-error '(invalid-parse-result)))
    (appkit-markup-parse-result--create
     :document document
     :diagnostics (seq-take diagnostics appkit-markup-codec-diagnostic-limit)
     :side-channels side-channels)))

(defun appkit-markup-print-result (source &optional losses)
  "Return a validated property-preserving SOURCE and semantic LOSSES."
  (unless (and (stringp source) (proper-list-p losses)
               (cl-every #'appkit-markup-loss-p losses))
    (signal 'appkit-markup-codec-error '(invalid-print-result)))
  (appkit-markup-print-result--create
   :source (copy-sequence source)
   :losses (copy-sequence losses)))

(cl-defun appkit-markup-register-codec
    (name &key parse print label)
  "Register source codec NAME and return its immutable descriptor.

PARSE and PRINT are required synchronous functions."
  (unless (and (symbolp name) name (functionp parse) (functionp print)
               (or (null label) (stringp label)))
    (signal 'appkit-markup-codec-error '(invalid-registration)))
  (let ((codec
         (appkit-markup-codec--create
          :name name
          :label (substring-no-properties (or label (symbol-name name)))
          :parse-function parse
          :print-function print)))
    (puthash name codec appkit-markup-codec--registry)
    codec))

(defun appkit-markup-codec (name)
  "Return registered codec NAME or signal a programmer error."
  (or (and (symbolp name) (gethash name appkit-markup-codec--registry))
      (signal 'appkit-markup-codec-error '(unknown-codec))))

(defun appkit-markup-codecs ()
  "Return registered codec names in deterministic symbol-name order."
  (sort (hash-table-keys appkit-markup-codec--registry)
        (lambda (left right)
          (string-lessp (symbol-name left) (symbol-name right)))))

(defun appkit-markup-codec--check-source (source)
  "Return an owned SOURCE or signal without exposing its contents."
  (unless (stringp source)
    (signal 'appkit-markup-codec-error '(invalid-source)))
  (when (> (length source) appkit-markup-codec-source-limit)
    (signal 'appkit-markup-codec-error '(source-too-long)))
  (copy-sequence source))

(defun appkit-markup-codec--object-end (source start)
  "Return the structured-object end after START in SOURCE."
  (appkit-chatbuf-next-input-object-change start source (length source)))

(defun appkit-markup-codec--validate-object-run (source start end)
  "Return one validated occurrence from SOURCE START..END."
  (let* ((payload (get-text-property
                   start appkit-chatbuf-input-object-property source))
         (span (get-text-property
                start appkit-chatbuf-input-object-span-property source))
         (stored (get-text-property
                  start appkit-chatbuf-input-object-text-property source))
         (body-end (1- end)))
    (unless (and payload span (stringp stored)
                 (< start end)
                 (get-text-property
                  start appkit-chatbuf-input-object-start-property source)
                 (get-text-property
                  body-end appkit-chatbuf-input-object-end-property source)
                 (= (aref source body-end) ?\s)
                 (equal stored (substring-no-properties source start body-end))
                 (not (text-property-not-all
                       start end appkit-chatbuf-input-object-span-property span
                       source))
                 (not (text-property-not-all
                       start end appkit-chatbuf-input-object-property payload
                       source)))
      (signal 'appkit-markup-codec-error '(invalid-object-span)))
    (appkit-markup-object-occurrence--create
     :value payload :text (substring-no-properties stored)
     :start start :end end)))

(defun appkit-markup-codec--next-token-character (source used cursor)
  "Return an unused private character absent from SOURCE after CURSOR."
  (let ((character (or cursor #xE000)))
    (while (and (<= character #xF8FF)
                (or (gethash character used)
                    (string-match-p (regexp-quote (char-to-string character))
                                    source)))
      (setq character (1+ character)))
    (when (> character #xF8FF)
      (signal 'appkit-markup-codec-error '(object-token-space-exhausted)))
    (puthash character t used)
    character))

(defun appkit-markup-codec--classify-object (classifier occurrence)
  "Return validated CLASSIFIER disposition for OCCURRENCE."
  (let ((disposition
         (if classifier
             (funcall classifier
                      (appkit-markup-object-occurrence-value occurrence)
                      (appkit-markup-object-occurrence-text occurrence))
           'semantic)))
    (cond
     ((or (null disposition) (eq disposition 'semantic)) '(semantic))
     ((and (consp disposition) (eq (car disposition) 'side-channel)
           (cdr disposition))
      disposition)
     ((and (consp disposition) (eq (car disposition) 'reject)
           (symbolp (cdr disposition)))
      (signal 'appkit-markup-object-rejected
              (list (cdr disposition)
                    (appkit-markup-object-occurrence-start occurrence)
                    (appkit-markup-object-occurrence-end occurrence))))
     (t (signal 'appkit-markup-codec-error '(invalid-object-disposition))))))

(defun appkit-markup-codec--protect-objects (source classifier)
  "Protect structured objects in SOURCE according to CLASSIFIER."
  (let ((position 0)
        (finish (length source))
        (used (make-hash-table :test #'eql))
        (token-cursor #xE000)
        (count 0)
        pieces
        (mapping (list 0))
        semantic
        side-channels)
    (cl-labels
        ((emit-range
           (start end)
           (when (< start end)
             (push (substring source start end) pieces)
             (cl-loop for boundary from (1+ start) to end
                      do (push boundary mapping))))
         (emit-token
           (character original-end)
           (push (char-to-string character) pieces)
           (push original-end mapping)))
      (while (< position finish)
        (if-let* ((payload
                   (get-text-property
                    position appkit-chatbuf-input-object-property source)))
            (let* ((end (appkit-markup-codec--object-end source position))
                   (occurrence
                    (appkit-markup-codec--validate-object-run
                     source position end))
                   (disposition
                    (appkit-markup-codec--classify-object classifier occurrence)))
              (ignore payload)
              (cl-incf count)
              (when (> count appkit-markup-codec-object-limit)
                (signal 'appkit-markup-codec-error '(too-many-objects)))
              (pcase (car disposition)
                ('semantic
                 (let* ((character
                         (appkit-markup-codec--next-token-character
                          source used token-cursor))
                        (token (char-to-string character))
                        (body-end (1- end)))
                   (setq token-cursor (1+ character))
                   (emit-token character body-end)
                   ;; Preserve the boundary spacer as ordinary source text.
                   (emit-range body-end end)
                   (push (appkit-markup-codec--object-create
                          :token token :occurrence occurrence)
                         semantic)))
                ('side-channel
                 ;; Removing an occurrence advances the original coordinate at
                 ;; the current transformed boundary.
                 (setcar mapping end)
                 (let* ((key (cdr disposition))
                        (entry (assoc key side-channels)))
                   (if entry
                       (setcdr entry (cons occurrence (cdr entry)))
                     (push (list key occurrence) side-channels)))))
              (setq position end))
          (let ((next
                 (or (next-single-property-change
                      position appkit-chatbuf-input-object-property source finish)
                     finish)))
            (emit-range position next)
            (setq position next)))))
    (dolist (entry side-channels)
      (setcdr entry (nreverse (cdr entry))))
    (appkit-markup-codec--protected-create
     :source (apply #'concat (nreverse pieces))
     :mapping (vconcat (nreverse mapping))
     :semantic (nreverse semantic)
     :side-channels (nreverse side-channels))))

(defun appkit-markup-codec--mapping-position (mapping position)
  "Map transformed POSITION through boundary MAPPING."
  (aref mapping (min (max 0 position) (1- (length mapping)))))

(defun appkit-markup-codec--map-diagnostic (diagnostic mapping)
  "Return DIAGNOSTIC mapped to original coordinates through MAPPING."
  (appkit-markup-diagnostic
   (appkit-markup-diagnostic-kind diagnostic)
   (appkit-markup-diagnostic-severity diagnostic)
   (appkit-markup-codec--mapping-position
    mapping (appkit-markup-diagnostic-start diagnostic))
   (appkit-markup-codec--mapping-position
    mapping (appkit-markup-diagnostic-end diagnostic))))

(defun appkit-markup-codec--token-table (objects)
  "Return character-keyed token table for protected semantic OBJECTS."
  (let ((table (make-hash-table :test #'eql)))
    (dolist (object objects table)
      (puthash (aref (appkit-markup-codec--object-token object) 0)
               object table))))

(defun appkit-markup-codec--restore-text (node table seen)
  "Restore protected objects in text NODE using TABLE and SEEN list cell."
  (let* ((text (appkit-markup-text-text node))
         (styles (appkit-markup-text-styles node))
         (length (length text))
         (start 0)
         result)
    (cl-loop for position from 0 below length
             for object = (gethash (aref text position) table)
             when object do
             (when (< start position)
               (push (appkit-markup-text (substring text start position) styles)
                     result))
             (push object (car seen))
             (let ((occurrence (appkit-markup-codec--object-occurrence object)))
               (push (appkit-markup-object
                      (appkit-markup-object-occurrence-value occurrence)
                      (list (appkit-markup-text
                             (appkit-markup-object-occurrence-text occurrence)))
                      styles)
                     result))
             (setq start (1+ position)))
    (when (< start length)
      (push (appkit-markup-text (substring text start) styles) result))
    (nreverse result)))

(defun appkit-markup-codec--text-has-token-p (text table)
  "Return non-nil when TEXT contains one protected token in TABLE."
  (cl-loop for character across text
           thereis (gethash character table)))

(defun appkit-markup-codec--restore-inlines (children table seen link-label-p)
  "Restore protected objects in CHILDREN, rejecting LINK-LABEL-P contexts."
  (let (result)
    (dolist (node children (nreverse result))
      (cond
       ((appkit-markup-text-p node)
        (when (and link-label-p
                   (appkit-markup-codec--text-has-token-p
                    (appkit-markup-text-text node) table))
          (signal 'appkit-markup-codec-error '(object-inside-link)))
        (dolist (part (appkit-markup-codec--restore-text node table seen))
          (push part result)))
       ((appkit-markup-link-p node)
        (push (appkit-markup-link
               (appkit-markup-link-url node)
               (appkit-markup-codec--restore-inlines
                (appkit-markup-link-children node) table seen t))
              result))
       ((appkit-markup-object-p node)
        (push (appkit-markup-object
               (appkit-markup-object-value node)
               (appkit-markup-codec--restore-inlines
                (appkit-markup-object-fallback node) table seen nil)
               (appkit-markup-object-styles node))
              result))
       (t (push node result))))))

(defun appkit-markup-codec--restore-blocks (blocks table seen)
  "Restore protected objects recursively in BLOCKS."
  (mapcar
   (lambda (block)
     (cond
      ((appkit-markup-paragraph-p block)
       (appkit-markup-paragraph
        (appkit-markup-codec--restore-inlines
         (appkit-markup-paragraph-children block) table seen nil)))
      ((appkit-markup-heading-p block)
       (appkit-markup-heading
        (appkit-markup-heading-level block)
        (appkit-markup-codec--restore-inlines
         (appkit-markup-heading-children block) table seen nil)))
      ((appkit-markup-quote-p block)
       (appkit-markup-quote
        (appkit-markup-codec--restore-blocks
         (appkit-markup-quote-blocks block) table seen)))
      ((appkit-markup-list-p block)
       (appkit-markup-list
        (appkit-markup-list-style block)
        (mapcar
         (lambda (item)
           (appkit-markup-list-item
            (appkit-markup-codec--restore-blocks
             (appkit-markup-list-item-blocks item) table seen)))
         (appkit-markup-list-items block))
        :start (appkit-markup-list-start block)))
      ((appkit-markup-preformatted-p block)
       (when (appkit-markup-codec--text-has-token-p
              (appkit-markup-preformatted-text block) table)
         (signal 'appkit-markup-codec-error '(object-inside-preformatted)))
       block)
      ((appkit-markup-object-block-p block)
       (appkit-markup-object-block
        (appkit-markup-object-block-value block)
        (appkit-markup-codec--restore-blocks
         (appkit-markup-object-block-fallback block) table seen)))
      (t (signal 'appkit-markup-codec-error '(invalid-codec-document)))))
   blocks))

(defun appkit-markup-codec--restore-objects (document objects)
  "Restore protected OBJECTS into DOCUMENT exactly once and in source order."
  (if (null objects)
      document
    (let* ((table (appkit-markup-codec--token-table objects))
           (seen (list nil))
           (restored
            (appkit-markup-document
             (appkit-markup-codec--restore-blocks
              (appkit-markup-document-blocks document) table seen)))
           (found (nreverse (car seen))))
      (unless (and (= (length found) (length objects))
                   (cl-every #'eq found objects))
        (signal 'appkit-markup-codec-error '(object-placeholder-corrupted)))
      restored)))

(cl-defun appkit-markup-parse (name source &key context object-classifier)
  "Parse property-preserving SOURCE with codec NAME.

OBJECT-CLASSIFIER receives each opaque value and visible label.  It returns
`semantic' (the default), `(side-channel . KEY)', or `(reject . KIND)'."
  (let* ((codec (appkit-markup-codec name))
         (source (appkit-markup-codec--check-source source))
         (protected
          (appkit-markup-codec--protect-objects source object-classifier))
         (result
          (funcall (appkit-markup-codec-parse-function codec)
                   (appkit-markup-codec--protected-source protected)
                   context)))
    (unless (appkit-markup-parse-result-p result)
      (signal 'appkit-markup-codec-error '(invalid-parser-result)))
    (let* ((document
            (appkit-markup-codec--restore-objects
             (appkit-markup-normalize
              (appkit-markup-parse-result-document result))
             (appkit-markup-codec--protected-semantic protected)))
           (diagnostics
            (mapcar
             (lambda (diagnostic)
               (appkit-markup-codec--map-diagnostic
                diagnostic (appkit-markup-codec--protected-mapping protected)))
             (appkit-markup-parse-result-diagnostics result))))
      (appkit-markup-parse-result
       document
       :diagnostics diagnostics
       :side-channels
       (append (appkit-markup-codec--protected-side-channels protected)
               (appkit-markup-parse-result-side-channels result))))))

(defun appkit-markup-codec--collect-string-characters (document)
  "Return a character set containing every semantic string in DOCUMENT."
  (let ((characters (make-hash-table :test #'eql)))
    (cl-labels
        ((record (text)
           (when text
             (mapc (lambda (character) (puthash character t characters))
                   (string-to-list text))))
         (inlines (nodes)
           (dolist (node nodes)
             (cond
              ((appkit-markup-text-p node)
               (record (appkit-markup-text-text node)))
              ((appkit-markup-link-p node)
               (record (appkit-markup-link-url node))
               (inlines (appkit-markup-link-children node)))
              ((appkit-markup-object-p node)
               (inlines (appkit-markup-object-fallback node))))))
         (blocks (nodes)
           (dolist (node nodes)
             (cond
              ((appkit-markup-paragraph-p node)
               (inlines (appkit-markup-paragraph-children node)))
              ((appkit-markup-heading-p node)
               (inlines (appkit-markup-heading-children node)))
              ((appkit-markup-quote-p node)
               (blocks (appkit-markup-quote-blocks node)))
              ((appkit-markup-list-p node)
               (dolist (item (appkit-markup-list-items node))
                 (blocks (appkit-markup-list-item-blocks item))))
              ((appkit-markup-preformatted-p node)
               (record (appkit-markup-preformatted-text node))
               (record (appkit-markup-preformatted-language node)))
              ((appkit-markup-object-block-p node)
               (blocks (appkit-markup-object-block-fallback node)))))))
      (blocks (appkit-markup-document-blocks document)))
    characters))

(defun appkit-markup-codec--print-token (characters)
  "Return and reserve one private token absent from CHARACTERS."
  (let ((character #xE000))
    (while (and (<= character #xF8FF)
                (gethash character characters))
      (cl-incf character))
    (when (> character #xF8FF)
      (signal 'appkit-markup-codec-error '(object-token-space-exhausted)))
    (puthash character t characters)
    (char-to-string character)))

(defun appkit-markup-codec--prepare-print-inlines
    (children object-printer characters records)
  "Replace printable objects in CHILDREN with tokens and append RECORDS."
  (mapcar
   (lambda (node)
     (cond
      ((appkit-markup-link-p node)
       (appkit-markup-link
        (appkit-markup-link-url node)
        (appkit-markup-codec--prepare-print-inlines
         (appkit-markup-link-children node)
         object-printer characters records)))
      ((appkit-markup-object-p node)
       (if-let* ((source (funcall object-printer node)))
           (progn
             (unless (and (stringp source) (not (string-empty-p source)))
               (signal 'appkit-markup-codec-error
                       '(invalid-object-printer-output)))
             (let ((token (appkit-markup-codec--print-token characters)))
               (push (cons token (copy-sequence source)) (car records))
               (appkit-markup-text token (appkit-markup-object-styles node))))
         node))
      (t node)))
   children))

(defun appkit-markup-codec--prepare-print-blocks
    (blocks object-printer characters records)
  "Replace printable objects recursively in BLOCKS and append RECORDS."
  (mapcar
   (lambda (block)
     (cond
      ((appkit-markup-paragraph-p block)
       (appkit-markup-paragraph
        (appkit-markup-codec--prepare-print-inlines
         (appkit-markup-paragraph-children block)
         object-printer characters records)))
      ((appkit-markup-heading-p block)
       (appkit-markup-heading
        (appkit-markup-heading-level block)
        (appkit-markup-codec--prepare-print-inlines
         (appkit-markup-heading-children block)
         object-printer characters records)))
      ((appkit-markup-quote-p block)
       (appkit-markup-quote
        (appkit-markup-codec--prepare-print-blocks
         (appkit-markup-quote-blocks block)
         object-printer characters records)))
      ((appkit-markup-list-p block)
       (appkit-markup-list
        (appkit-markup-list-style block)
        (mapcar
         (lambda (item)
           (appkit-markup-list-item
            (appkit-markup-codec--prepare-print-blocks
             (appkit-markup-list-item-blocks item)
             object-printer characters records)))
         (appkit-markup-list-items block))
        :start (appkit-markup-list-start block)))
      ((appkit-markup-object-block-p block)
       (if-let* ((source (funcall object-printer block)))
           (progn
             (unless (and (stringp source) (not (string-empty-p source)))
               (signal 'appkit-markup-codec-error
                       '(invalid-object-printer-output)))
             (let ((token (appkit-markup-codec--print-token characters)))
               (push (cons token (copy-sequence source)) (car records))
               (appkit-markup-paragraph
                (list (appkit-markup-text token)))))
         block))
      (t block)))
   blocks))

(defun appkit-markup-codec--prepare-print
    (document object-printer)
  "Return `(DOCUMENT . RECORDS)' prepared through OBJECT-PRINTER."
  (if (not object-printer)
      (cons document nil)
    (unless (functionp object-printer)
      (signal 'appkit-markup-codec-error '(invalid-object-printer)))
    (let ((characters
           (appkit-markup-codec--collect-string-characters document))
          (records (list nil)))
      (cons
       (appkit-markup-document
        (appkit-markup-codec--prepare-print-blocks
         (appkit-markup-document-blocks document)
         object-printer characters records))
       (nreverse (car records))))))

(defun appkit-markup-codec--restore-printed-objects (source records)
  "Replace call-local tokens in SOURCE with property-preserving RECORDS."
  (if (null records)
      source
    (let ((table (make-hash-table :test #'equal))
          (regexp (regexp-opt (mapcar #'car records)))
          (position 0)
          found pieces)
      (dolist (record records)
        (puthash (car record) record table))
      (while (string-match regexp source position)
        (let* ((start (match-beginning 0))
               (end (match-end 0))
               (record (gethash (match-string 0 source) table)))
          (when (< position start)
            (push (substring source position start) pieces))
          (push record found)
          (push (cdr record) pieces)
          (setq position end)))
      (when (< position (length source))
        (push (substring source position) pieces))
      (setq found (nreverse found))
      (unless (and (= (length found) (length records))
                   (cl-every #'eq found records))
        (signal 'appkit-markup-codec-error
                '(object-print-placeholder-corrupted)))
      (let ((tokens (mapcar #'car records)))
        (dolist (record records)
          (when (seq-some
                 (lambda (token)
                   (string-match-p (regexp-quote token) (cdr record)))
                 tokens)
            (signal 'appkit-markup-codec-error
                    '(object-printer-output-contains-placeholder)))))
      (apply #'concat (nreverse pieces)))))

(cl-defun appkit-markup-print (name document &key context object-printer)
  "Print DOCUMENT through codec NAME and return a print result.

OBJECT-PRINTER receives each object node and returns exact
property-preserving source or nil.  Nil traverses the object's visible fallback
and records semantic loss."
  (let* ((codec (appkit-markup-codec name))
         (document (appkit-markup-normalize document))
         (prepared
          (appkit-markup-codec--prepare-print document object-printer))
         (result
          (funcall (appkit-markup-codec-print-function codec)
                   (car prepared) context)))
    (unless (appkit-markup-print-result-p result)
      (signal 'appkit-markup-codec-error '(invalid-printer-result)))
    (appkit-markup-print-result
     (appkit-markup-codec--restore-printed-objects
      (appkit-markup-print-result-source result) (cdr prepared))
     (appkit-markup-print-result-losses result))))

(provide 'appkit-markup-codec)

;;; appkit-markup-codec.el ends here
