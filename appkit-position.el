;;; appkit-position.el --- Semantic point and window preservation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Stable-key position snapshots shared by full, EWOC, and chat views.

;;; Code:

(require 'cl-lib)

(cl-defstruct (appkit-position--location-snapshot
               (:constructor appkit-position--location-snapshot-create))
  line
  column
  anchor-value
  anchor-line-offset
  anchor-character-offset
  anchor-column-offset
  after-anchor-run-p)

(cl-defstruct (appkit-position--window-snapshot
               (:constructor appkit-position--window-snapshot-create))
  window
  point
  start)

(cl-defstruct (appkit-position-snapshot
               (:constructor appkit-position-snapshot--create))
  line
  column
  anchor-property
  anchor-value
  anchor-line-offset
  window-start-line
  window-start-column
  window-anchor-value
  window-anchor-line-offset
  window-anchor-character-offset
  window-anchor-column-offset
  window-after-anchor-run-p
  point-location
  window-snapshots)

(defun appkit-position-find-property-value (start end property value)
  "Return first position from START to END whose PROPERTY equals VALUE."
  (let ((position start)
        found)
    (while (and (< position end) (not found))
      (if (equal (get-text-property position property) value)
          (setq found position)
        (setq position
              (next-single-property-change position property nil end))))
    found))

(defun appkit-position--anchor-value-at (position property)
  "Return PROPERTY at POSITION, probing the previous character if needed."
  (or (get-text-property position property)
      (and (> position (point-min))
           (get-text-property (1- position) property))))

(defun appkit-position--anchor-line-start (position property value)
  "Return the start of VALUE's PROPERTY run on POSITION's logical line."
  (save-excursion
    (goto-char position)
    (let ((line-start (line-beginning-position))
          (run-start position))
      (while (and (> run-start line-start)
                  (equal (get-text-property (1- run-start) property) value))
        (setq run-start
              (or (previous-single-property-change
                   run-start property nil line-start)
                  line-start)))
      run-start)))

(defun appkit-position--capture-location (position property)
  "Capture semantic and absolute context for POSITION using PROPERTY."
  (when position
    (save-excursion
      (goto-char position)
      (let* ((line (line-number-at-pos))
             (column (current-column))
             (direct-anchor-value
              (and property (get-text-property position property)))
             (previous-anchor-value
              (and property
                   (null direct-anchor-value)
                   (> position (point-min))
                   (get-text-property (1- position) property)))
             (after-anchor-run-p (and previous-anchor-value t))
             (anchor-value (or direct-anchor-value previous-anchor-value))
             (anchor-position
              (if after-anchor-run-p (1- position) position))
             (anchor-target
              (and anchor-value
                   (appkit-position-find-property-value
                    (point-min) (point-max) property anchor-value)))
             (anchor-line-offset
              (and anchor-target
                   (max 0 (- (line-number-at-pos anchor-position)
                             (save-excursion
                               (goto-char anchor-target)
                               (line-number-at-pos))))))
             (anchor-line-start
              (and anchor-target
                   (appkit-position--anchor-line-start
                    anchor-position property anchor-value)))
             (anchor-character-offset
              (and anchor-line-start (- position anchor-line-start)))
             (anchor-column-offset
              (and anchor-line-start
                   (- column
                      (save-excursion
                        (goto-char anchor-line-start)
                        (current-column))))))
        (appkit-position--location-snapshot-create
         :line line
         :column column
         :anchor-value anchor-value
         :anchor-line-offset anchor-line-offset
         :anchor-character-offset anchor-character-offset
         :anchor-column-offset anchor-column-offset
         :after-anchor-run-p after-anchor-run-p)))))

(defun appkit-position--capture-window (window property)
  "Capture WINDOW's independent point and start using PROPERTY."
  (appkit-position--window-snapshot-create
   :window window
   :point (appkit-position--capture-location (window-point window) property)
   :start (appkit-position--capture-location (window-start window) property)))

(cl-defun appkit-position-capture (&key anchor-property preserve-window-start)
  "Capture semantic point context in the current buffer.
ANCHOR-PROPERTY identifies stable domain entries.  When PRESERVE-WINDOW-START
is non-nil, capture both the absolute and semantic viewport position."
  (let* ((point-location
          (appkit-position--capture-location (point) anchor-property))
         (anchor-value
          (appkit-position--location-snapshot-anchor-value point-location))
         (anchor-line-offset
          (appkit-position--location-snapshot-anchor-line-offset point-location))
         (window (and preserve-window-start
                      (get-buffer-window (current-buffer))))
         (window-start-position (and window (window-start window)))
         (legacy-window-start
          (and window-start-position
               (appkit-position--capture-location
                window-start-position anchor-property)))
         (window-snapshots
          (and preserve-window-start
               (mapcar
                (lambda (live-window)
                  (appkit-position--capture-window
                   live-window anchor-property))
                (get-buffer-window-list (current-buffer) nil t)))))
    (appkit-position-snapshot--create
     :line (line-number-at-pos)
     :column (current-column)
     :anchor-property anchor-property
     :anchor-value anchor-value
     :anchor-line-offset anchor-line-offset
     :window-start-line
     (and legacy-window-start
          (appkit-position--location-snapshot-line legacy-window-start))
     :window-start-column
     (and legacy-window-start
          (appkit-position--location-snapshot-column legacy-window-start))
     :window-anchor-value
     (and legacy-window-start
          (appkit-position--location-snapshot-anchor-value
           legacy-window-start))
     :window-anchor-line-offset
     (and legacy-window-start
          (appkit-position--location-snapshot-anchor-line-offset
           legacy-window-start))
     :window-anchor-character-offset
     (and legacy-window-start
          (appkit-position--location-snapshot-anchor-character-offset
           legacy-window-start))
     :window-anchor-column-offset
     (and legacy-window-start
          (appkit-position--location-snapshot-anchor-column-offset
           legacy-window-start))
     :window-after-anchor-run-p
     (and legacy-window-start
          (appkit-position--location-snapshot-after-anchor-run-p
           legacy-window-start))
     :point-location point-location
     :window-snapshots window-snapshots)))

(defun appkit-position--move-within-anchor (property value offset)
  "Move OFFSET lines while PROPERTY remains VALUE."
  (let ((remaining (max 0 (or offset 0))))
    (while (> remaining 0)
      (let ((next (save-excursion (forward-line 1) (point))))
        (if (or (= next (point))
                (not (equal (get-text-property next property) value)))
            (setq remaining 0)
          (goto-char next)
          (setq remaining (1- remaining)))))))

(defun appkit-position--promote-anchor-value (value anchor-value-map)
  "Return VALUE after applying its explicit ANCHOR-VALUE-MAP promotion."
  (if-let* ((mapping (assoc value anchor-value-map)))
      (cdr mapping)
    value))

(defun appkit-position--anchor-run-end (property value &optional limit)
  "Return the end of VALUE's PROPERTY run at point, bounded by LIMIT.

Adjacent text-property intervals whose values are `equal' remain one semantic
run even when their values are not the same Lisp object."
  (let ((cursor (point))
        (bound (or limit (point-max))))
    (while (and (< cursor bound)
                (equal (get-text-property cursor property) value))
      (setq cursor
            (or (next-single-property-change cursor property nil bound)
                bound)))
    cursor))

(defun appkit-position--anchor-line-end (property value)
  "Return the last position on this logical line inside PROPERTY's VALUE."
  (if (not (equal (get-text-property (point) property) value))
      (point)
    (let* ((line-end (line-end-position))
           (search-end (min (point-max) (1+ line-end)))
           (run-end
            (appkit-position--anchor-run-end property value search-end)))
      (if (<= run-end line-end)
          (max (point) (1- run-end))
        line-end))))

(defun appkit-position--move-within-anchor-line
    (property value character-offset column-offset)
  "Move within VALUE's current logical-line PROPERTY run.

CHARACTER-OFFSET preserves the buffer-position offset.  COLUMN-OFFSET is used
when it reaches a closer visual column.  Neither candidate may leave VALUE."
  (let* ((start (point))
         (start-column (current-column))
         (end (appkit-position--anchor-line-end property value))
         (character-position
          (min end (+ start (max 0 (or character-offset 0)))))
         (desired-column
          (+ start-column (max 0 (or column-offset 0))))
         (column-position
          (save-excursion
            (move-to-column desired-column)
            (min end (point))))
         (character-error
          (save-excursion
            (goto-char character-position)
            (abs (- (current-column) desired-column))))
         (column-error
          (save-excursion
            (goto-char column-position)
            (abs (- (current-column) desired-column)))))
    (goto-char (if (< column-error character-error)
                   column-position
                 character-position))))

(defun appkit-position--restore-location
    (location property anchor-value-map)
  "Restore LOCATION with PROPERTY and ANCHOR-VALUE-MAP, returning point."
  (let* ((captured
          (appkit-position--location-snapshot-anchor-value location))
         (value
          (appkit-position--promote-anchor-value captured anchor-value-map))
         (target
          (and property value
               (appkit-position-find-property-value
                (point-min) (point-max) property value))))
    (if target
        (progn
          (goto-char target)
          (if (appkit-position--location-snapshot-after-anchor-run-p location)
              (goto-char (appkit-position--anchor-run-end property value))
            (appkit-position--move-within-anchor
             property value
             (appkit-position--location-snapshot-anchor-line-offset location))
            (appkit-position--move-within-anchor-line
             property value
             (appkit-position--location-snapshot-anchor-character-offset
              location)
             (appkit-position--location-snapshot-anchor-column-offset
              location))))
      (goto-char (point-min))
      (forward-line
       (max 0
            (1- (or (appkit-position--location-snapshot-line location) 1))))
      (move-to-column
       (max 0 (or (appkit-position--location-snapshot-column location) 0))))
    (point)))

(defun appkit-position--restore-window-snapshot
    (window-snapshot property anchor-value-map)
  "Restore WINDOW-SNAPSHOT using PROPERTY and ANCHOR-VALUE-MAP independently."
  (let ((window (appkit-position--window-snapshot-window window-snapshot)))
    (when (and (window-live-p window)
               (eq (window-buffer window) (current-buffer)))
      (let ((window-point
             (save-excursion
               (appkit-position--restore-location
                (appkit-position--window-snapshot-point window-snapshot)
                property anchor-value-map)))
            (window-start
             (save-excursion
               (appkit-position--restore-location
                (appkit-position--window-snapshot-start window-snapshot)
                property anchor-value-map))))
        (set-window-point window window-point)
        (set-window-start window window-start 'noforce)))))

(defun appkit-position-restore (snapshot &optional anchor-value-map)
  "Restore point and viewport from SNAPSHOT.

ANCHOR-VALUE-MAP maps explicit old keys to promoted keys."
  (let ((property (appkit-position-snapshot-anchor-property snapshot)))
    (if-let* ((point-location
               (appkit-position-snapshot-point-location snapshot)))
        (appkit-position--restore-location
         point-location property anchor-value-map)
      (let* ((captured (appkit-position-snapshot-anchor-value snapshot))
             (value (appkit-position--promote-anchor-value
                     captured anchor-value-map))
             (target (and property value
                          (appkit-position-find-property-value
                           (point-min) (point-max) property value))))
        (if target
            (progn
              (goto-char target)
              (appkit-position--move-within-anchor
               property value
               (appkit-position-snapshot-anchor-line-offset snapshot)))
          (goto-char (point-min))
          (forward-line
           (max 0 (1- (or (appkit-position-snapshot-line snapshot) 1)))))
        (move-to-column
         (max 0 (or (appkit-position-snapshot-column snapshot) 0)))))
    (if-let* ((window-snapshots
               (appkit-position-snapshot-window-snapshots snapshot)))
        (dolist (window-snapshot window-snapshots)
          (appkit-position--restore-window-snapshot
           window-snapshot property anchor-value-map))
      ;; Snapshots made before per-window capture only carry these fields.
      (when-let* ((line (appkit-position-snapshot-window-start-line snapshot))
                  (window (get-buffer-window (current-buffer))))
        (let* ((legacy-location
                (appkit-position--location-snapshot-create
                 :line line
                 :column
                 (appkit-position-snapshot-window-start-column snapshot)
                 :anchor-value
                 (appkit-position-snapshot-window-anchor-value snapshot)
                 :anchor-line-offset
                 (appkit-position-snapshot-window-anchor-line-offset snapshot)
                 :anchor-character-offset
                 (appkit-position-snapshot-window-anchor-character-offset
                  snapshot)
                 :anchor-column-offset
                 (appkit-position-snapshot-window-anchor-column-offset
                  snapshot)
                 :after-anchor-run-p
                 (appkit-position-snapshot-window-after-anchor-run-p
                  snapshot)))
               (window-start
                (save-excursion
                  (appkit-position--restore-location
                   legacy-location property anchor-value-map))))
          (set-window-start window window-start 'noforce))))))

(cl-defun appkit-position-render-preserving
    (render-function &key anchor-property preserve-window-start after-restore)
  "Call RENDER-FUNCTION and restore semantic point and viewport context.

ANCHOR-PROPERTY and PRESERVE-WINDOW-START are passed to
`appkit-position-capture'.  Call AFTER-RESTORE after restoration when it is a
function."
  (let ((snapshot
         (appkit-position-capture
          :anchor-property anchor-property
          :preserve-window-start preserve-window-start)))
    (funcall render-function)
    (when snapshot
      (appkit-position-restore snapshot))
    (when (functionp after-restore)
      (funcall after-restore))))

(provide 'appkit-position)

;;; appkit-position.el ends here
