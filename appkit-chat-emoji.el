;;; appkit-chat-emoji.el --- Shared Unicode emoji completion  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Protocol-neutral `:unicode_name:' composer completion backed by Emacs's
;; built-in emoji database when available.  Applications remain responsible
;; for custom stickers, server emoji, and protocol-specific send objects.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-chat-completion)

(declare-function emoji--init "emoji" (&optional force inhibit-adjust))
(defvar emoji--all-bases)
(defvar emoji--labels)

(defun appkit-chat-emoji--collect-category (node category table)
  "Record emoji glyphs below NODE as CATEGORY in TABLE."
  (dolist (item (cdr node))
    (cond
     ((consp item)
      (appkit-chat-emoji--collect-category item category table))
     ((stringp item)
      (puthash item category table)))))

(defun appkit-chat-emoji--emacs-category-map ()
  "Return an emoji glyph to built-in top-level category hash."
  (let ((table (make-hash-table :test #'equal)))
    (when (boundp 'emoji--labels)
      (dolist (group emoji--labels)
        (when (and (consp group) (stringp (car group)))
          (appkit-chat-emoji--collect-category
           group (format "Unicode · %s" (car group)) table))))
    table))

(defun appkit-chat-emoji--emacs-category-order ()
  "Return built-in top-level emoji categories in database order."
  (when (boundp 'emoji--labels)
    (delq nil
          (mapcar
           (lambda (group)
             (and (consp group)
                  (stringp (car group))
                  (format "Unicode · %s" (car group))))
           emoji--labels))))

(defun appkit-chat-emoji--emacs-source ()
  "Return Emacs's Unicode emoji name table when available."
  (when (require 'emoji nil t)
    (when (fboundp 'emoji--init)
      (ignore-errors (emoji--init)))
    (and (boundp 'emoji--all-bases)
         (hash-table-p emoji--all-bases)
         emoji--all-bases)))

(defcustom appkit-chat-emoji-source-function
  #'appkit-chat-emoji--emacs-source
  "Function returning a hash table of Unicode emoji names to glyphs.

The default adapter uses Emacs's built-in emoji database when available."
  :type 'function
  :group 'appkit)

(defvar appkit-chat-emoji--candidates nil
  "Cached shared Unicode emoji completion candidates.")

(defvar appkit-chat-emoji--initialized-p nil
  "Non-nil after the shared Unicode emoji source has been inspected.")

(defun appkit-chat-emoji-reset-cache ()
  "Forget the shared Unicode emoji candidate cache."
  (interactive)
  (setq appkit-chat-emoji--candidates nil
        appkit-chat-emoji--initialized-p nil))

(defun appkit-chat-emoji--token-name (name)
  "Return colon-delimited completion token for Unicode emoji NAME."
  (let* ((normalized
          (replace-regexp-in-string
           "[^[:alnum:]+-]+" "_" (downcase (string-trim name))))
         (trimmed (string-trim normalized "_+" "_+")))
    (unless (string-empty-p trimmed)
      (format ":%s:" trimmed))))

(defun appkit-chat-emoji--build-candidates
    (source &optional category-map category-order)
  "Build candidates from SOURCE, CATEGORY-MAP, and CATEGORY-ORDER."
  (let ((seen (make-hash-table :test #'equal))
        (category-ranks (make-hash-table :test #'equal))
        records
        candidates)
    (cl-loop for category in category-order
             for rank from 0
             do (puthash category rank category-ranks))
    (maphash
     (lambda (name glyph)
       (when (and (stringp name)
                  (not (string-empty-p name))
                  (stringp glyph)
                  (not (string-empty-p glyph)))
         (when-let* ((base (appkit-chat-emoji--token-name name)))
           (push (list :base base :name name :glyph glyph) records))))
     source)
    (setq records
          (sort records
                (lambda (left right)
                  (let ((left-base (plist-get left :base))
                        (right-base (plist-get right :base))
                        (left-name (plist-get left :name))
                        (right-name (plist-get right :name)))
                    (if (equal left-base right-base)
                        (if (equal left-name right-name)
                            (string-lessp (plist-get left :glyph)
                                          (plist-get right :glyph))
                          (string-lessp left-name right-name))
                      (string-lessp left-base right-base))))))
    (dolist (record records)
      (let* ((base (plist-get record :base))
             (name (plist-get record :name))
             (glyph (plist-get record :glyph))
             (count (1+ (gethash base seen 0)))
             (label (if (= count 1)
                        base
                      (concat (substring base 0 -1)
                              (format "_%d:" count)))))
        (puthash base count seen)
        (push
         (appkit-chat-completion-candidate-create
          :label label
          :insert glyph
          :prefix (concat glyph " ")
          :annotation (format "  %s" name)
          :search-terms (list name glyph (substring base 1 -1))
          :group (or (and category-map (gethash glyph category-map))
                     "Unicode")
          :value (list :kind 'unicode-emoji
                       :name name
                       :emoji glyph))
         candidates)))
    (sort
     candidates
     (lambda (left right)
       (let* ((left-rank
               (gethash
                (appkit-chat-completion-candidate-group left)
                category-ranks most-positive-fixnum))
              (right-rank
               (gethash
                (appkit-chat-completion-candidate-group right)
                category-ranks most-positive-fixnum)))
         (if (= left-rank right-rank)
             (string-lessp
              (appkit-chat-completion-candidate-label left)
              (appkit-chat-completion-candidate-label right))
           (< left-rank right-rank)))))))

(defun appkit-chat-emoji-candidates (&optional force)
  "Return shared Unicode emoji candidates.

With FORCE non-nil, rebuild the cache from Emacs's current emoji database.
Return nil on Emacs versions without the built-in emoji library."
  (when force
    (appkit-chat-emoji-reset-cache))
  (unless (or appkit-chat-emoji--initialized-p
              appkit-chat-emoji--candidates)
    (let ((source-function appkit-chat-emoji-source-function))
      (if (not (functionp source-function))
          (setq appkit-chat-emoji--initialized-p t)
        (condition-case error-data
            (let* ((source (funcall source-function))
                   (category-map
                    (and (eq source-function
                             #'appkit-chat-emoji--emacs-source)
                         (appkit-chat-emoji--emacs-category-map)))
                   (category-order
                    (and category-map
                         (appkit-chat-emoji--emacs-category-order))))
              (unless (or (null source) (hash-table-p source))
                (error "Emoji source must return a hash table or nil"))
              (setq appkit-chat-emoji--candidates
                    (and source
                         (appkit-chat-emoji--build-candidates
                          source category-map category-order)))
              (setq appkit-chat-emoji--initialized-p t))
          (error
           (message "Appkit: failed to initialize emoji completion: %s"
                    (error-message-string error-data)))))))
  appkit-chat-emoji--candidates)

(cl-defun appkit-chat-emoji-capf (&key sync-function)
  "Return a CAPF for a Unicode `:name:' token at point.

SYNC-FUNCTION runs after the selected Unicode glyph is committed."
  (when-let* ((token (appkit-chat-completion-delimited-token-bounds ?:))
              (candidates (appkit-chat-emoji-candidates)))
    (appkit-chat-completion-capf
     (plist-get token :start)
     (plist-get token :end)
     candidates
     :sync-function sync-function)))

(provide 'appkit-chat-emoji)

;;; appkit-chat-emoji.el ends here
