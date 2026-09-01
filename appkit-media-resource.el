;;; appkit-media-resource.el --- Atomic media resource acquisition  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/emacs-im/appkit.el

;;; Commentary:

;; Protocol-neutral resource naming, classification, transfer, opening, and
;; in-Emacs video playback.  Applications adapt their wire objects into the
;; canonical shape constructed by `appkit-media-resource-create': an alist
;; containing only `file', `url', `name', and `mime-type'.  Media opens never
;; fall back to a browser; explicit non-media page links may still use one.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'image-mode)
(require 'plz)
(require 'seq)
(require 'subr-x)
(require 'url-parse)
(require 'appkit-core)
(require 'appkit-media-card)
(require 'appkit-media-image)
(require 'video)

(defcustom appkit-media-video-cache-directory
  (locate-user-emacs-file "appkit-video-cache/")
  "Directory holding complete opportunistic video playback caches.

Remote playback starts immediately.  GStreamer's sparse session cache is
promoted here only if it naturally becomes complete."
  :type 'directory
  :group 'appkit-media)

(defcustom appkit-media-video-cache-policy 'automatic
  "Persistent cache policy for remote video playback.

`automatic' streams immediately and retains the sparse playback cache only
when it becomes complete.  `none' keeps playback buffering session-local."
  :type '(choice (const automatic) (const none))
  :group 'appkit-media)

(defcustom appkit-media-open-file-function #'find-file
  "Function used to open downloaded files and images.

The function receives one local filename."
  :type 'function
  :group 'appkit-media)

(defcustom appkit-media-animate-gifs t
  "When non-nil, start GIF animation after opening it in `image-mode'."
  :type 'boolean
  :group 'appkit-media)

(defcustom appkit-media-transfer-concurrency 6
  "Maximum number of shared media transfers active at once."
  :type 'integer
  :group 'appkit-media)

(defconst appkit-media--cache-extensions
  '("webp" "png" "jpg" "jpeg" "gif" "bmp" "heic" "heif"
    "tif" "tiff" "svg" "svgz" "img")
  "Media cache extension candidates.")

(defconst appkit-media-image-accept-headers
  '(("Accept" . "image/png,image/webp,image/*;q=0.8,*/*;q=0.1"))
  "HTTP headers used when acquiring image previews.")

(defconst appkit-media--curl-security-arguments
  '("--proto" "=https" "--proto-redir" "=https" "--max-redirs" "5")
  "Curl arguments restricting remote media and redirects to HTTPS.")

(defvar appkit-media--pending-transfers nil
  "FIFO list of remote media transfers waiting to start.")

(defvar appkit-media--active-transfer-count 0
  "Number of remote media transfers currently active.")

(defvar appkit-media--scheduling-transfers-p nil
  "Non-nil while the shared media transfer scheduler is running.")

(defvar appkit-media--inflight-transfers (make-hash-table :test #'equal)
  "Remote resource transfers indexed by their absolute target filename.")

(cl-defstruct (appkit-media--transfer
               (:constructor appkit-media--transfer-create))
  "One atomic remote transfer and all of its interested callers."
  target
  identity
  url
  headers
  staging-directory
  part-file
  listeners
  process
  active-p
  done-p)

(cl-defstruct (appkit-media--transfer-handle
               (:constructor appkit-media--transfer-handle-create))
  "One caller's independently cancelable interest in a remote transfer."
  transfer
  success
  error
  done-p)

(cl-defstruct (appkit-media-video-session
               (:constructor appkit-media--video-session-create))
  "Canonical application resource metadata around one video.el session."
  resource
  label
  source
  video-session)

(cl-defstruct (appkit-media-video-inline
               (:constructor appkit-media--video-inline-create))
  "One inline surface borrowing an Appkit video session."
  session
  inline)

(defvar-local appkit-media--video-buffer-session nil
  "Appkit video session borrowed by the current dedicated buffer.")

(defvar-local appkit-media--video-buffer-owner-handle nil
  "Lifecycle handle owning the current dedicated video buffer.")

(defun appkit-media-transfer-p (object)
  "Return non-nil when OBJECT is an appkit media transfer handle."
  (appkit-media--transfer-handle-p object))

(defconst appkit-media-resource-keys '(file url name mime-type)
  "Keys accepted in a canonical appkit media resource.")

(defun appkit-media-url-present-p (url)
  "Return non-nil when URL is a non-empty string."
  (and (stringp url)
       (not (string-empty-p url))))

(defun appkit-media--https-url-p (url)
  "Return non-nil when URL is a curl-config-safe HTTPS URL.

The authority must contain a host and no credentials.  Literal whitespace,
control characters, quotes, and backslashes are rejected before URL parsing
because Plz 0.9 serializes request data through curl's stdin config format."
  (and (appkit-media-url-present-p url)
       (not (string-match-p "[[:space:]\"\\\\]" url))
       (condition-case nil
           (let ((parsed (url-generic-parse-url url)))
             (and (string-equal (url-type parsed) "https")
                  (stringp (url-host parsed))
                  (not (string-empty-p (url-host parsed)))
                  (null (url-user parsed))
                  (null (url-password parsed))))
         (error nil))))

(defun appkit-media--local-or-https-source-p (source)
  "Return non-nil when SOURCE is an existing local file or HTTPS URL."
  (or (appkit-media--https-url-p source)
      (appkit-media-file-present-p source)))

(defun appkit-media--validate-headers (headers)
  "Return HEADERS after validating their curl-config-safe syntax."
  (unless (listp headers)
    (error "Appkit media headers must be an alist"))
  (dolist (header headers)
    (let ((name (and (consp header) (car header)))
          (value (and (consp header) (cdr header))))
      (unless (and (stringp name)
                   (string-match-p
                    "\\`[!#$%&'*+.^_`|~0-9A-Za-z-]+\\'" name)
                   (stringp value)
                   (not (string-match-p "[[:cntrl:]\"\\\\]" value)))
        (error "Appkit media header is unsafe: %S" header))))
  headers)

(defun appkit-media-file-present-p (file)
  "Return non-nil when FILE names an existing regular file."
  (and (stringp file) (file-regular-p file)))

(defun appkit-media-readable-file-p (file)
  "Return non-nil when FILE names a readable regular file."
  (and (appkit-media-file-present-p file)
       (file-readable-p file)))

(defun appkit-media-read-file-name
    (prompt &optional dir default-filename initial)
  "Read and return a readable regular file name using PROMPT.

DIR, DEFAULT-FILENAME, and INITIAL have the same meanings as in
`read-file-name'.  Directories remain navigable completion candidates but
cannot finish the prompt.  The returned value is checked again because some
graphical file dialogs do not enforce `read-file-name' acceptance functions."
  (let* ((base-directory (or dir default-directory))
         (acceptable-p
          (lambda (file)
            (appkit-media-readable-file-p
             (expand-file-name file base-directory))))
         (file
          (read-file-name
           prompt dir default-filename acceptable-p initial)))
    (unless (funcall acceptable-p file)
      (user-error "Media attachment is not a readable regular file: %s" file))
    file))

(defun appkit-media-url-filename (url)
  "Extract a best-effort filename from URL."
  (let* ((base (car (split-string (or url "") "[?#]")))
         (name (file-name-nondirectory base)))
    (and (appkit-media-url-present-p name) name)))

(defun appkit-media-image-file-name-p (filename)
  "Return non-nil when FILENAME looks like an image file."
  (and (stringp filename)
       (string-match-p
        (rx "." (or "png" "jpg" "jpeg" "gif" "webp" "bmp" "svg"
                    "svgz" "heic" "heif" "tif" "tiff") string-end)
        (downcase (car (split-string filename "[?#]"))))))

(defun appkit-media-video-file-name-p (filename)
  "Return non-nil when FILENAME looks like a video file."
  (and (stringp filename)
       (string-match-p
        (rx "." (or "mp4" "mov" "mkv" "webm" "avi" "flv" "m4v")
            string-end)
        (downcase (car (split-string filename "[?#]"))))))

(defun appkit-media-gif-file-name-p (filename)
  "Return non-nil when FILENAME looks like a GIF image."
  (and (stringp filename)
       (string-match-p
        (rx ".gif" string-end)
        (downcase (car (split-string filename "[?#]"))))))

(defun appkit-media-resource-normalize (resource)
  "Validate RESOURCE and return a fresh canonical media resource.

RESOURCE must be an alist whose keys are drawn exclusively from
`appkit-media-resource-keys'.  Each key may occur at most once and each value
must be nil or a non-empty string.  Nil entries are omitted from the returned
alist.  This function deliberately does not translate backend field names;
applications must perform that adaptation at their protocol boundary."
  (unless (listp resource)
    (error "Appkit media resource must be an alist: %S" resource))
  (let (seen normalized)
    (dolist (entry resource)
      (unless (and (consp entry)
                   (memq (car entry) appkit-media-resource-keys))
        (error "Non-canonical appkit media resource entry: %S" entry))
      (let ((key (car entry))
            (value (cdr entry)))
        (when (memq key seen)
          (error "Duplicate appkit media resource key: %S" key))
        (push key seen)
        (unless (or (null value)
                    (and (stringp value) (not (string-empty-p value))))
          (error "Appkit media resource %S must be a non-empty string: %S"
                 key value))
        (when value
          (push (cons key value) normalized))))
    (nreverse normalized)))

(cl-defun appkit-media-resource-create (&key file url name mime-type)
  "Construct a canonical media resource.

FILE and URL identify local and remote representations.  NAME is a display
and filename hint, and MIME-TYPE is an optional Internet media type.  Values
must be nil or non-empty strings.  At least one source is required only by
operations that transfer or open the resource."
  (appkit-media-resource-normalize
   `((file . ,file)
     (url . ,url)
     (name . ,name)
     (mime-type . ,mime-type))))

(defun appkit-media-resource-name (resource)
  "Return the best filename hint from canonical RESOURCE."
  (let* ((resource (appkit-media-resource-normalize resource))
         (file (alist-get 'file resource)))
    (or (alist-get 'name resource)
        (and file (file-name-nondirectory file))
        (appkit-media-url-filename (alist-get 'url resource)))))

(defun appkit-media--validate-kind (kind)
  "Return KIND when it is a supported semantic media kind."
  (unless (memq kind '(image video file))
    (error "Appkit media kind must be image, video, or file: %S" kind))
  kind)

(defun appkit-media-resource-kind (resource &optional kind)
  "Return explicit KIND or infer a supported kind from canonical RESOURCE.

The only valid results are `image', `video', and `file'."
  (let* ((resource (appkit-media-resource-normalize resource))
         (name (appkit-media-resource-name resource))
         (mime-type (downcase (or (alist-get 'mime-type resource) ""))))
    (appkit-media--validate-kind
     (or kind
         (cond
          ((string-prefix-p "video/" mime-type) 'video)
          ((string-prefix-p "image/" mime-type) 'image)
          ((appkit-media-video-file-name-p name) 'video)
          ((appkit-media-image-file-name-p name) 'image)
          (t 'file))))))

(defun appkit-media-sanitize-filename (filename)
  "Return a filesystem-safe variant of FILENAME."
  (replace-regexp-in-string
   "[[:cntrl:]/\\]+"
   "_"
   (or filename "media.bin")))

(defun appkit-media-command-arguments (command)
  "Normalize COMMAND into an argument list, or nil when unusable."
  (cond
   ((and (listp command)
         command
         (seq-every-p #'stringp command))
    command)
   ((and (stringp command)
         (not (string-empty-p command)))
    (split-string-and-unquote command))
   (t nil)))

(defun appkit-media-command-runnable-p (command)
  "Return non-nil when COMMAND resolves to an executable program."
  (when-let* ((argv (appkit-media-command-arguments command))
              (program (car argv)))
    (or (and (file-name-absolute-p program)
             (file-executable-p program))
        (executable-find program))))

(defun appkit-media-video-session-live-p (session)
  "Return non-nil when SESSION owns a live video.el session."
  (and (appkit-media-video-session-p session)
       (video-session-live-p
        (appkit-media-video-session-video-session session))))

(defun appkit-media-video-session-player (session)
  "Return SESSION's canonical video.el player."
  (video-session-player
   (appkit-media-video-session-video-session session)))

(defun appkit-media-video-session-close (session)
  "Close SESSION through its video.el presentation lifecycle."
  (when (appkit-media-video-session-p session)
    (video-session-close
     (appkit-media-video-session-video-session session)))
  nil)

(defun appkit-media-video-inline-closed-p (surface)
  "Return non-nil when Appkit video inline SURFACE is closed."
  (or (not (appkit-media-video-inline-p surface))
      (let ((inline (appkit-media-video-inline-inline surface)))
        (or (not (video-inline-p inline))
            (video-inline-closed inline)))))

(defun appkit-media--video-inline-finished
    (surface close-function _inline)
  "Notify CLOSE-FUNCTION after video.el retires Appkit SURFACE."
  (when close-function
    (condition-case error-data
        (funcall close-function surface)
      (error
       (message "Appkit media inline close callback failed: %s"
                (error-message-string error-data))))))

(cl-defun appkit-media-video-inline-create
    (session width height
             &key poster (fit 'contain) buffer
             canvas canvas-width canvas-height
             (destination-x 0) (destination-y 0)
             visible-function alive-function activate-function close-function)
  "Create an inline surface borrowing SESSION at WIDTH by HEIGHT.

POSTER, FIT, BUFFER, CANVAS, CANVAS-WIDTH, CANVAS-HEIGHT, DESTINATION-X,
DESTINATION-Y, VISIBLE-FUNCTION, ALIVE-FUNCTION, and ACTIVATE-FUNCTION carry
video.el's presentation contracts.  Audio state remains on SESSION's player.
CLOSE-FUNCTION is called once with the returned surface after it closes."
  (unless (appkit-media-video-session-live-p session)
    (error "Cannot create an inline surface for a closed video session"))
  (when (and close-function (not (functionp close-function)))
    (error "Appkit inline video close function is not callable"))
  (let* ((surface (appkit-media--video-inline-create :session session))
         (video-session
          (appkit-media-video-session-video-session session)))
    (condition-case error-data
        (let ((inline
               (video-session-inline-create
                video-session width height
                :poster poster :fit fit :buffer buffer
                :canvas canvas :canvas-width canvas-width
                :canvas-height canvas-height
                :destination-x destination-x :destination-y destination-y
                :visible-function visible-function
                :alive-function alive-function
                :activate-function activate-function
                :close-function
                (lambda (inline)
                  (appkit-media--video-inline-finished
                   surface close-function inline)))))
          (setf (appkit-media-video-inline-inline surface) inline)
          surface)
      ((error quit)
       (when (zerop (video-session-presentation-count video-session))
         (video-session-close video-session))
       (signal (car error-data) (cdr error-data))))))

(defun appkit-media-video-inline-play (surface)
  "Start or resume Appkit video inline SURFACE."
  (when (appkit-media-video-inline-closed-p surface)
    (error "Appkit inline video surface is closed"))
  (video-inline-play (appkit-media-video-inline-inline surface))
  surface)

(defun appkit-media-video-inline-toggle (surface)
  "Toggle playback for Appkit video inline SURFACE."
  (when (appkit-media-video-inline-closed-p surface)
    (error "Appkit inline video surface is closed"))
  (video-inline-toggle-occurrence (appkit-media-video-inline-inline surface))
  surface)

(defun appkit-media-video-inline-muted-p (surface)
  "Return Appkit video inline SURFACE's canonical mute state."
  (when (appkit-media-video-inline-closed-p surface)
    (error "Appkit inline video surface is closed"))
  (video-inline-muted-p (appkit-media-video-inline-inline surface)))

(defun appkit-media-video-inline-toggle-muted (surface)
  "Toggle Appkit video inline SURFACE's canonical mute state."
  (when (appkit-media-video-inline-closed-p surface)
    (error "Appkit inline video surface is closed"))
  (video-inline-toggle-muted (appkit-media-video-inline-inline surface))
  surface)

(defun appkit-media-video-inline-set-muted (surface muted)
  "Set Appkit video inline SURFACE audio MUTED state."
  (when (appkit-media-video-inline-closed-p surface)
    (error "Appkit inline video surface is closed"))
  (video-inline-set-muted (appkit-media-video-inline-inline surface) muted)
  surface)

(defun appkit-media-video-inline-bind-controls (surface map)
  "Bind Appkit video inline SURFACE transport controls into MAP."
  (when (appkit-media-video-inline-closed-p surface)
    (error "Appkit inline video surface is closed"))
  (video-inline-bind-controls (appkit-media-video-inline-inline surface) map)
  surface)

(defun appkit-media-video-inline-close (surface)
  "Close Appkit video inline SURFACE."
  (when (and (appkit-media-video-inline-p surface)
             (not (appkit-media-video-inline-closed-p surface)))
    (video-inline-close (appkit-media-video-inline-inline surface)))
  nil)

(defun appkit-media--kill-video-buffer (buffer)
  "Kill video viewer BUFFER when it is still live."
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

(defun appkit-media--release-video-buffer-session ()
  "Release Appkit owner metadata for the current video.el presentation."
  (when-let* ((handle appkit-media--video-buffer-owner-handle))
    (setq appkit-media--video-buffer-owner-handle nil)
    (when (appkit-handle-alive-p handle)
      (appkit-retire-handle handle)))
  (setq appkit-media--video-buffer-session nil))

(cl-defun appkit-media-present-video-session
    (session &optional client-label
             &key owner buffer start display-function)
  "Present SESSION in a dedicated video buffer without replacing its player.

CLIENT-LABEL names a generated BUFFER.  OWNER owns that buffer when non-nil.
START explicitly requests playback; nil preserves the shared player's exact
state.  DISPLAY-FUNCTION is forwarded to video.el."
  (let* ((label (or client-label
                    (appkit-media-video-session-label session)
                    "media"))
         (buffer (or (and (buffer-live-p buffer) buffer)
                     (generate-new-buffer (format "*%s Video*" label))))
         handle
         opened-p)
    (unless (or (null owner) (appkit-owner-live-p owner))
      (user-error "%s: media owner is no longer live" label))
    (unless (appkit-media-video-session-live-p session)
      (error "%s: video session is closed" label))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (if (eq appkit-media--video-buffer-session session)
                (setq handle appkit-media--video-buffer-owner-handle)
              (appkit-media--release-video-buffer-session)))
          (when (and owner (not handle))
            (setq handle
                  (appkit-register-handle
                   owner 'buffer buffer #'appkit-media--kill-video-buffer)))
          (let ((opened
                 (video-session-present
                  (appkit-media-video-session-video-session session)
                  :buffer buffer :display-function display-function)))
            (unless (and (eq opened buffer)
                         (buffer-live-p buffer)
                         (or (null owner) (appkit-owner-live-p owner)))
              (error "%s: video.el did not return its live viewer buffer"
                     label)))
          (with-current-buffer buffer
            (setq-local video-quit-function #'kill-current-buffer
                        appkit-media--video-buffer-session session
                        appkit-media--video-buffer-owner-handle handle)
            (add-hook 'kill-buffer-hook
                      #'appkit-media--release-video-buffer-session nil t)
            (add-hook 'change-major-mode-hook
                      #'appkit-media--release-video-buffer-session nil t))
          (when start
            (video-player-play
             (appkit-media-video-session-player session)))
          (setq opened-p t)
          (message
           "%s: %s video in Emacs"
           label
           (if (appkit-media-file-present-p
                (appkit-media-video-session-source session))
               "playing local"
             "streaming"))
          buffer)
      (unless opened-p
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (when (and handle (appkit-handle-alive-p handle))
          (appkit-retire-handle handle))))))

(cl-defun appkit-media-present-video-inline
    (surface &optional client-label
             &key owner buffer display-function)
  "Present inline SURFACE in a dedicated buffer without changing playback.

CLIENT-LABEL, OWNER, BUFFER, and DISPLAY-FUNCTION have the same meanings as in
`appkit-media-present-video-session'."
  (when (appkit-media-video-inline-closed-p surface)
    (error "Cannot present a closed Appkit inline video surface"))
  (appkit-media-present-video-session
   (appkit-media-video-inline-session surface) client-label
   :owner owner :buffer buffer :start nil
   :display-function display-function))

(cl-defun appkit-media-video-session-create
    (resource &optional client-label
              &key owner cache-key cache-directory cache-update-function
              (cache-policy appkit-media-video-cache-policy) muted live
              request-headers)
  "Create one Appkit playback session for canonical video RESOURCE.

CLIENT-LABEL identifies errors and messages.  OWNER limits cache callbacks to a
live Appkit lifecycle.  CACHE-KEY, CACHE-DIRECTORY, CACHE-UPDATE-FUNCTION, and
CACHE-POLICY have the same meanings as in `appkit-media-play-video-source'.
MUTED controls the initial player audio state.  LIVE enforces live-stream
semantics; REQUEST-HEADERS are video transport headers and do not become part
of RESOURCE identity.  The caller must promptly create an inline or dedicated
surface, or close the returned session."
  (let* ((label (or client-label "media"))
         (resource (appkit-media-resource-normalize resource))
         (file (alist-get 'file resource))
         (url (alist-get 'url resource))
         source
         cache-file
         cache-complete-function)
    (unless (or (null owner) (appkit-owner-live-p owner))
      (user-error "%s: media owner is no longer live" label))
    (unless (memq cache-policy '(automatic none))
      (user-error "%s: invalid video cache policy: %S" label cache-policy))
    (cl-labels
        ((remember-cache
          (_player local-file)
          (when (or (null owner) (appkit-owner-live-p owner))
            (setf (alist-get 'file resource nil nil #'eq) local-file)
            (when (functionp cache-update-function)
              (funcall cache-update-function (copy-tree resource)))
            (message "%s: retained complete video playback cache" label))))
      (cond
       ((appkit-media-file-present-p file)
        (setq source file))
       ((appkit-media--https-url-p url)
        (if (eq cache-policy 'none)
            (setq source url)
          (setq cache-file
                (appkit-media--video-cache-target
                 (or cache-key (format "video-url:%s" url))
                 (or cache-directory
                     appkit-media-video-cache-directory)))
          (if (appkit-media-file-present-p cache-file)
              (progn
                (remember-cache nil cache-file)
                (setq source cache-file
                      cache-file nil))
            (setq source url
                  cache-complete-function #'remember-cache))))
       (t
        (user-error "%s: video resource has neither local file nor HTTPS URL"
                    label)))
      (appkit-media--video-session-create
       :resource resource :label label :source source
       :video-session
       (video-session-create
        source :kind 'video :muted muted :live live :auto-close t
        :request-headers request-headers
        :cache-file cache-file
        :cache-complete-function cache-complete-function)))))

(cl-defun appkit-media--play-video-resource
    (resource label
              &key owner cache-key cache-directory cache-update-function
              (cache-policy appkit-media-video-cache-policy) live
              request-headers)
  "Stream canonical video RESOURCE for LABEL in an Appkit-owned session.

OWNER owns the dedicated buffer.  CACHE-KEY and CACHE-DIRECTORY select the
persistent target, CACHE-UPDATE-FUNCTION receives a completed resource, and
CACHE-POLICY controls persistent promotion.  LIVE and REQUEST-HEADERS configure
the video transport without changing RESOURCE identity."
  (let (session opened-p)
    (unwind-protect
        (prog1
            (appkit-media-present-video-session
             (setq session
                   (appkit-media-video-session-create
                    resource label
                    :owner owner :cache-key cache-key
                    :cache-directory cache-directory
                    :cache-update-function cache-update-function
                    :cache-policy cache-policy :live live
                    :request-headers request-headers))
             label :owner owner :start t)
          (setq opened-p t))
      (unless opened-p
        (appkit-media-video-session-close session)))))

(cl-defun appkit-media-play-video-source
    (source &optional client-label
            &key owner cache-key cache-directory cache-update-function
            (cache-policy appkit-media-video-cache-policy) live
            request-headers)
  "Stream local file or HTTPS URL SOURCE through video.el.

Remote playback starts immediately.  LIVE enforces non-seekable live-stream
semantics.  REQUEST-HEADERS are applied to the playback transport but excluded
from canonical resource and cache identity.  Under automatic CACHE-POLICY, a
complete progressive playback cache is retained under CACHE-KEY; `none' leaves
buffering session-local.  CACHE-DIRECTORY overrides
`appkit-media-video-cache-directory'.  CACHE-UPDATE-FUNCTION receives the
canonical resource after a complete cache is retained.  CLIENT-LABEL names the
viewer, and OWNER owns its buffer.  Explicit downloads use
`appkit-media-copy-or-download-resource-async' instead."
  (let ((label (or client-label "media")))
    (unless (appkit-media--local-or-https-source-p source)
      (user-error "%s: video source must be a local file or HTTPS URL" label))
    (appkit-media--play-video-resource
     (if (appkit-media-file-present-p source)
         (appkit-media-resource-create
          :file source :name (file-name-nondirectory source))
       (appkit-media-resource-create
        :url source :name (appkit-media-url-filename source)))
     label
     :owner owner
     :cache-key cache-key
     :cache-directory cache-directory
     :cache-update-function cache-update-function
     :cache-policy cache-policy
     :live live
     :request-headers request-headers)))

(cl-defun appkit-media-play-video-url
    (url &optional client-label
         &key owner cache-key cache-directory cache-update-function
         (cache-policy appkit-media-video-cache-policy) live request-headers)
  "Stream video URL for CLIENT-LABEL.

OWNER, CACHE-KEY, CACHE-DIRECTORY, CACHE-UPDATE-FUNCTION, CACHE-POLICY, LIVE,
and REQUEST-HEADERS have the same meanings as in
`appkit-media-play-video-source'."
  (appkit-media-play-video-source
   url client-label
   :owner owner
   :cache-key cache-key
   :cache-directory cache-directory
   :cache-update-function cache-update-function
   :cache-policy cache-policy
   :live live
   :request-headers request-headers))

(cl-defun appkit-media-play-video-file
    (path &optional client-label &key owner)
  "Play local PATH for CLIENT-LABEL, optionally lifecycle-owned by OWNER."
  (unless (appkit-media-file-present-p path)
    (user-error "%s: local video file does not exist: %s"
                (or client-label "media") path))
  (appkit-media-play-video-source path client-label :owner owner))

(defun appkit-media--maybe-start-gif-animation (file)
  "Start GIF animation for FILE when its `image-mode' buffer is idle."
  (when (and appkit-media-animate-gifs
             (appkit-media-gif-file-name-p file))
    (when-let* ((buffer (get-file-buffer file)))
      (with-current-buffer buffer
        (when (derived-mode-p 'image-mode)
          (let ((image (image-get-display-property)))
            (when (and image
                       (image-multi-frame-p image)
                       (not (image-animate-timer image)))
              (setq-local image-animate-loop t)
              (image-toggle-animation))))))))

(defun appkit-media-open-file (file)
  "Open local FILE through `appkit-media-open-file-function'."
  (unless (appkit-media-file-present-p file)
    (user-error "Media: local file does not exist: %s" file))
  (when-let* ((buffer (get-file-buffer file)))
    (with-current-buffer buffer
      (unless (buffer-modified-p)
        (set-visited-file-modtime))))
  (prog1
      (funcall appkit-media-open-file-function file)
    (appkit-media--maybe-start-gif-animation file)))

(defun appkit-media-add-open-url-properties (start end url)
  "Attach mouse and keyboard handlers to open URL between START and END."
  (when (and (appkit-media-url-present-p url)
             (< start end))
    (appkit-media-add-action-properties
     start end
     (lambda () (browse-url url t))
     (format "Open media: %s" url))))

(cl-defun appkit-media-add-open-image-properties
    (start end resource &key cache-key cache-directory
           cache-update-function client-label)
  "Attach browser-free image open handlers between START and END.

RESOURCE follows the canonical appkit media resource shape.  CACHE-KEY,
CACHE-DIRECTORY, CACHE-UPDATE-FUNCTION, and CLIENT-LABEL are forwarded to
`appkit-media-open-resource'."
  (let ((resource (appkit-media-resource-normalize resource)))
    (when (and (or (appkit-media-file-present-p (alist-get 'file resource))
                   (appkit-media-url-present-p (alist-get 'url resource)))
               (< start end))
      (appkit-media-add-action-properties
       start end
       (lambda ()
         (appkit-media-open-resource
          resource
          :kind 'image
          :cache-key cache-key
          :cache-directory cache-directory
          :cache-update-function cache-update-function
          :client-label client-label))
       "Open image in Emacs"))))

(cl-defun appkit-media-add-play-video-properties
    (start end video-source &optional client-label
           &key owner cache-key cache-directory cache-update-function
           (cache-policy appkit-media-video-cache-policy) live request-headers)
  "Attach a video action between START and END.

VIDEO-SOURCE, CLIENT-LABEL, OWNER, CACHE-KEY, CACHE-DIRECTORY,
CACHE-UPDATE-FUNCTION, CACHE-POLICY, LIVE, and REQUEST-HEADERS are forwarded to
`appkit-media-play-video-source'."
  (when (and (appkit-media-url-present-p video-source)
             (< start end))
    (appkit-media-add-action-properties
     start end
     (lambda ()
       (appkit-media-play-video-source
        video-source client-label
        :owner owner
        :cache-key cache-key
        :cache-directory cache-directory
        :cache-update-function cache-update-function
        :cache-policy cache-policy
        :live live
        :request-headers request-headers))
     (format "Play video: %s" video-source))))

(defun appkit-media--open-cache-file-base (directory key)
  "Return cache file base in DIRECTORY for logical KEY."
  (expand-file-name (md5 (format "%s" key)) directory))

(defun appkit-media-image-cache-existing-file (cache-base)
  "Return an existing image cache file rooted at CACHE-BASE, or nil."
  (seq-find
   #'file-exists-p
   (mapcar (lambda (extension) (format "%s.%s" cache-base extension))
           appkit-media--cache-extensions)))

(defun appkit-media--delete-open-cache-files (cache-base &optional keep)
  "Delete cached media files rooted at CACHE-BASE except KEEP."
  (dolist (extension appkit-media--cache-extensions)
    (let ((file (format "%s.%s" cache-base extension)))
      (when (and (file-exists-p file)
                 (not (and keep
                           (string-equal (expand-file-name file)
                                         (expand-file-name keep)))))
        (ignore-errors (delete-file file))))))

(defun appkit-media--read-file-prefix (file limit)
  "Return up to LIMIT literal bytes from FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 0 limit)
    (buffer-string)))

(defun appkit-media--resource-image-cache-key (resource cache-key)
  "Return a disk-cache key for image RESOURCE and optional CACHE-KEY."
  (or cache-key
      (when-let* ((url (alist-get 'url resource)))
        (format "open-image-url:%s" url))))

(defun appkit-media--cache-image-resource-for-open
    (resource cache-key cache-directory client-label callback errback)
  "Cache RESOURCE under CACHE-KEY in CACHE-DIRECTORY.

Pass its local path to CALLBACK, report failures to ERRBACK, and use
CLIENT-LABEL in synchronous setup errors."
  (unless (and (stringp cache-directory)
               (not (string-empty-p cache-directory)))
    (user-error "%s: image cache directory is required" client-label))
  (let* ((url (alist-get 'url resource))
         (key (appkit-media--resource-image-cache-key resource cache-key))
         (existing
          (and key
               (appkit-media-image-cache-existing-file
                (appkit-media--open-cache-file-base cache-directory key)))))
    (if existing
        (funcall callback existing)
      (unless (appkit-media-url-present-p url)
        (user-error "%s: image resource has no URL" client-label))
      (let ((cache-base
             (appkit-media--open-cache-file-base cache-directory key)))
        (make-directory cache-directory t)
        (appkit-media-cache-image-resource-async
         resource cache-base
         (lambda (downloaded)
           (funcall callback downloaded))
         errback)))))

(defun appkit-media--async-error-text (error-data)
  "Return a readable message for asynchronous ERROR-DATA."
  (condition-case nil
      (error-message-string error-data)
    ((error quit) (format "%s" error-data))))

(defun appkit-media--transfer-staging (target)
  "Create and return an exclusive staging directory beside TARGET."
  (let* ((directory (or (file-name-directory target) default-directory))
         (prefix (expand-file-name
                  (format ".%s.part-" (file-name-nondirectory target))
                  directory)))
    (make-temp-file prefix t)))

(defun appkit-media--cleanup-transfer (transfer)
  "Delete partial state belonging to TRANSFER."
  (when-let* ((part (appkit-media--transfer-part-file transfer)))
    (when (file-exists-p part)
      (ignore-errors (delete-file part))))
  (when-let* ((directory
               (appkit-media--transfer-staging-directory transfer)))
    (when (file-directory-p directory)
      (ignore-errors (delete-directory directory t)))))

(defun appkit-media--invoke-transfer-callback (callback side value)
  "Invoke CALLBACK for SIDE with VALUE while isolating errors and quits."
  (when (functionp callback)
    (condition-case err
        (funcall callback value)
      ((error quit)
       (message "Appkit media %s callback failed: %s"
                side (appkit-media--async-error-text err))))))

(defun appkit-media--notify-transfer-handle (handle side value)
  "Notify caller HANDLE on SIDE with VALUE exactly once."
  (unless (appkit-media--transfer-handle-done-p handle)
    (setf (appkit-media--transfer-handle-done-p handle) t)
    (let ((callback (if (eq side 'success)
                        (appkit-media--transfer-handle-success handle)
                      (appkit-media--transfer-handle-error handle))))
      (setf (appkit-media--transfer-handle-transfer handle) nil
            (appkit-media--transfer-handle-success handle) nil
            (appkit-media--transfer-handle-error handle) nil)
      (appkit-media--invoke-transfer-callback callback side value))))

(defun appkit-media--add-transfer-handle (transfer success error)
  "Add and return a caller handle on TRANSFER for SUCCESS and ERROR."
  (let ((handle (appkit-media--transfer-handle-create
                 :transfer transfer :success success :error error)))
    (setf (appkit-media--transfer-listeners transfer)
          (append (appkit-media--transfer-listeners transfer)
                  (list handle)))
    handle))

(defun appkit-media--finish-transfer (transfer side value)
  "Finish TRANSFER on SIDE and broadcast VALUE exactly once."
  (unless (appkit-media--transfer-done-p transfer)
    (setf (appkit-media--transfer-done-p transfer) t)
    (let ((target (appkit-media--transfer-target transfer))
          (listeners (appkit-media--transfer-listeners transfer)))
      (setf (appkit-media--transfer-listeners transfer) nil)
      (when (eq transfer (gethash target appkit-media--inflight-transfers))
        (remhash target appkit-media--inflight-transfers))
      (appkit-media--cleanup-transfer transfer)
      (dolist (handle listeners)
        (appkit-media--notify-transfer-handle handle side value)))))

(defun appkit-media--commit-transfer (transfer)
  "Atomically commit TRANSFER, or report its commit failure."
  (condition-case err
      (let ((part (appkit-media--transfer-part-file transfer))
            (target (appkit-media--transfer-target transfer)))
        (unless (file-regular-p part)
          (error "Remote transfer produced no regular partial file"))
        (rename-file part target t)
        (appkit-media--finish-transfer transfer 'success target))
    ((error quit)
     (appkit-media--finish-transfer
      transfer 'error (appkit-media--async-error-text err)))))

(defun appkit-media--release-active-transfer (transfer)
  "Release TRANSFER's active scheduler slot exactly once."
  (when (appkit-media--transfer-active-p transfer)
    (setf (appkit-media--transfer-active-p transfer) nil)
    (setq appkit-media--active-transfer-count
          (max 0 (1- appkit-media--active-transfer-count)))))

(defun appkit-media--remote-transfer-succeeded (transfer _file)
  "Finish a successful remote TRANSFER producing _FILE."
  (unless (appkit-media--transfer-done-p transfer)
    (appkit-media--release-active-transfer transfer)
    (appkit-media--commit-transfer transfer))
  (appkit-media--schedule-transfers))

(defun appkit-media--remote-transfer-failed (transfer error-data)
  "Finish a failed remote TRANSFER with ERROR-DATA."
  (unless (appkit-media--transfer-done-p transfer)
    (appkit-media--release-active-transfer transfer)
    (appkit-media--finish-transfer
     transfer 'error (appkit-media--async-error-text error-data)))
  (appkit-media--schedule-transfers))

(defun appkit-media--start-transfer (transfer)
  "Start pending remote TRANSFER, containing setup errors locally."
  (setf (appkit-media--transfer-active-p transfer) t)
  (cl-incf appkit-media--active-transfer-count)
  (condition-case err
      (let ((plz-curl-default-args
             (append
              '("--disable")
              plz-curl-default-args
              appkit-media--curl-security-arguments)))
        (setf (appkit-media--transfer-process transfer)
              (plz 'get (appkit-media--transfer-url transfer)
                :headers (appkit-media--transfer-headers transfer)
                :as `(file ,(appkit-media--transfer-part-file transfer))
                :noquery t
                :then (lambda (file)
                        (appkit-media--remote-transfer-succeeded transfer file))
                :else (lambda (error-data)
                        (appkit-media--remote-transfer-failed
                         transfer error-data)))))
    ((error quit)
     (appkit-media--release-active-transfer transfer)
     (appkit-media--finish-transfer
      transfer 'error (appkit-media--async-error-text err)))))

(defun appkit-media--schedule-transfers ()
  "Start pending media transfers up to the configured global limit."
  (unless appkit-media--scheduling-transfers-p
    (let ((appkit-media--scheduling-transfers-p t)
          (limit (max 1 appkit-media-transfer-concurrency)))
      (while (and appkit-media--pending-transfers
                  (< appkit-media--active-transfer-count limit))
        (let ((transfer (pop appkit-media--pending-transfers)))
          (unless (appkit-media--transfer-done-p transfer)
            (appkit-media--start-transfer transfer)))))))

(defun appkit-media--enqueue-transfer (transfer)
  "Append remote TRANSFER to the shared scheduler."
  (setq appkit-media--pending-transfers
        (append appkit-media--pending-transfers (list transfer)))
  (appkit-media--schedule-transfers))

(defun appkit-media--cancel-underlying-transfer (transfer)
  "Cancel TRANSFER after its last interested caller detached."
  (unless (appkit-media--transfer-done-p transfer)
    (setq appkit-media--pending-transfers
          (delq transfer appkit-media--pending-transfers))
    (appkit-media--release-active-transfer transfer)
    (when-let* ((process (appkit-media--transfer-process transfer)))
      (when (process-live-p process)
        (delete-process process)))
    (appkit-media--finish-transfer transfer 'error "transfer canceled")
    (appkit-media--schedule-transfers)))

(defun appkit-media-cancel-transfer (handle)
  "Cancel one opaque caller HANDLE returned by the transfer API.

Other callers sharing the same byte-identical transfer remain attached.  The
underlying request is canceled only after its final caller detaches."
  (unless (appkit-media--transfer-handle-p handle)
    (error "Appkit media transfer handle is invalid: %S" handle))
  (unless (appkit-media--transfer-handle-done-p handle)
    (let ((transfer (appkit-media--transfer-handle-transfer handle)))
      (when transfer
        (setf (appkit-media--transfer-listeners transfer)
              (delq handle (appkit-media--transfer-listeners transfer))))
      (appkit-media--notify-transfer-handle
       handle 'error "transfer canceled")
      (when (and transfer
                 (null (appkit-media--transfer-listeners transfer)))
        (appkit-media--cancel-underlying-transfer transfer))
      t)))

(defun appkit-media--resource-transfer-identity (resource headers)
  "Return byte-source identity for canonical RESOURCE and HEADERS."
  (if-let* ((file (alist-get 'file resource)))
      (list 'file (expand-file-name file))
    (list 'url (alist-get 'url resource) (copy-tree headers))))

(defun appkit-media--copy-local-resource-atomically (file target)
  "Copy FILE to TARGET without exposing partial final contents."
  (if (and (file-exists-p target)
           (fboundp 'file-equal-p)
           (file-equal-p file target))
      target
    (let* ((staging-directory (appkit-media--transfer-staging target))
           (part (expand-file-name "copy.part" staging-directory))
           completed)
      (unwind-protect
          (progn
            (copy-file file part)
            (rename-file part target t)
            (setq completed t)
            target)
        (unless completed
          (when (file-exists-p part)
            (ignore-errors (delete-file part))))
        (when (file-directory-p staging-directory)
          (ignore-errors (delete-directory staging-directory t)))))))

(cl-defun appkit-media-copy-or-download-resource-async
    (resource target success error &key headers)
  "Atomically copy or download canonical RESOURCE into TARGET.

Remote bytes are written to an exclusive partial file beside TARGET and only
become visible at TARGET through an atomic, overwriting rename.  Concurrent
requests share work only when target, source, and headers all match; a
different source targeting an active destination is rejected.  Each caller
receives an independently cancelable handle.  Local copies complete
synchronously and return nil.  SUCCESS receives the completed target path;
ERROR receives a readable reason.  HEADERS are forwarded for remote requests."
  (unless (functionp success)
    (error "Appkit media SUCCESS callback must be a function"))
  (unless (functionp error)
    (error "Appkit media ERROR callback must be a function"))
  (appkit-media--validate-headers headers)
  (let* ((resource (appkit-media-resource-normalize resource))
         (target (expand-file-name target))
         (file (alist-get 'file resource))
         (url (alist-get 'url resource))
         (identity (appkit-media--resource-transfer-identity resource headers))
         (existing (gethash target appkit-media--inflight-transfers)))
    (condition-case err
        (progn
          (when (and (appkit-media-url-present-p url)
                     (not (appkit-media-file-present-p file))
                     (not (appkit-media--https-url-p url)))
            (error "Remote media URL must use HTTPS"))
          (make-directory
           (or (file-name-directory target) default-directory) t)
          (cond
           ((appkit-media-file-present-p file)
            (if existing
                (appkit-media--invoke-transfer-callback
                 error 'error
                 "target already has an active transfer from another source")
              (appkit-media--invoke-transfer-callback
               success 'success
               (appkit-media--copy-local-resource-atomically file target)))
            nil)
           ((appkit-media-url-present-p url)
            (if existing
                (if (equal identity
                           (appkit-media--transfer-identity existing))
                    (appkit-media--add-transfer-handle
                     existing success error)
                  (appkit-media--invoke-transfer-callback
                   error 'error
                   "target already has an active transfer from another source")
                  nil)
              (let* ((staging-directory
                      (appkit-media--transfer-staging target))
                     (part (expand-file-name "download.part"
                                             staging-directory))
                     (transfer
                      (appkit-media--transfer-create
                       :target target
                       :identity identity
                       :url url
                       :headers (copy-tree headers)
                       :staging-directory staging-directory
                       :part-file part))
                     handle
                     handed-off-p)
                (unwind-protect
                    (progn
                      (setq handle
                            (appkit-media--add-transfer-handle
                             transfer success error))
                      (puthash target transfer
                               appkit-media--inflight-transfers)
                      (appkit-media--enqueue-transfer transfer)
                      (setq handed-off-p t)
                      handle)
                  (unless handed-off-p
                    (setf (appkit-media--transfer-listeners transfer) nil)
                    (appkit-media--cancel-underlying-transfer transfer))))))
           (t
            (appkit-media--invoke-transfer-callback
             error 'error "resource has neither local file nor URL")
            nil)))
      ((error quit)
       (appkit-media--invoke-transfer-callback
        error 'error (appkit-media--async-error-text err))
       nil))))

(defun appkit-media--normalize-image-cache-file (file)
  "Remove a recognized leading newline sequence from image cache FILE."
  (let* ((prefix (appkit-media--read-file-prefix file 64))
         (normalized (appkit-media-normalize-image-bytes prefix))
         (skip (- (length prefix) (length normalized))))
    (when (< (length normalized) (length prefix))
      (let* ((staging-directory (appkit-media--transfer-staging file))
             (part (expand-file-name "normalized.part" staging-directory))
             completed)
        (unwind-protect
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally file nil skip)
              (let ((coding-system-for-write 'binary))
                (write-region (point-min) (point-max) part nil 'silent))
              (rename-file part file t)
              (setq completed t))
          (unless completed
            (when (file-exists-p part)
              (ignore-errors (delete-file part))))
          (when (file-directory-p staging-directory)
            (ignore-errors (delete-directory staging-directory t))))))
    file))

(cl-defun appkit-media-cache-image-resource-async
    (resource cache-base success error &key headers)
  "Acquire canonical image RESOURCE below CACHE-BASE atomically.

Detect the image suffix from downloaded bytes, remove stale sibling cache
files, and call SUCCESS with the final local filename.  ERROR receives a
readable reason.  Return the opaque transfer handle from the shared transfer
runtime, or nil after synchronous work.  HEADERS defaults to
`appkit-media-image-accept-headers'."
  (unless (and (stringp cache-base) (not (string-empty-p cache-base)))
    (error "Appkit media CACHE-BASE must be a non-empty string"))
  (unless (functionp success)
    (error "Appkit media SUCCESS callback must be a function"))
  (unless (functionp error)
    (error "Appkit media ERROR callback must be a function"))
  (let* ((resource (appkit-media-resource-normalize resource))
         (cache-base (expand-file-name cache-base))
         (name (or (appkit-media-resource-name resource) ""))
         (hint (downcase (or (file-name-extension name) "img")))
         (fallback-extension
          (if (member hint appkit-media--cache-extensions) hint "img"))
         (download-target (format "%s.img" cache-base)))
    (appkit-media-copy-or-download-resource-async
     resource download-target
     (lambda (downloaded)
       (when-let* ((target
                    (condition-case err
                        (let* ((source
                                (cond
                                 ((file-exists-p downloaded) downloaded)
                                 ((appkit-media-image-cache-existing-file
                                   cache-base))
                                 (t
                                  (error
                                   "Downloaded image cache entry disappeared"))))
                               (source
                                (appkit-media--normalize-image-cache-file
                                 source))
                               (bytes
                                (appkit-media-normalize-image-bytes
                                 (appkit-media--read-file-prefix source 64)))
                               (extension
                                (appkit-media-bytes-to-extension
                                 bytes fallback-extension))
                               (target
                                (format "%s.%s" cache-base extension)))
                          (unless (string-equal (expand-file-name source)
                                                (expand-file-name target))
                            (rename-file source target t))
                          (appkit-media--delete-open-cache-files
                           cache-base target)
                          target)
                      ((error quit)
                       (funcall error (error-message-string err))
                       nil))))
         (funcall success target)))
     error
     :headers (or headers appkit-media-image-accept-headers))))

(defun appkit-media--video-cache-target (cache-key cache-directory)
  "Return stable video target for CACHE-KEY in CACHE-DIRECTORY."
  (unless (and (stringp cache-directory)
               (not (string-empty-p cache-directory)))
    (user-error "Media: video cache directory is required"))
  (expand-file-name
   (md5 (format "%s" cache-key))
   (expand-file-name "video" cache-directory)))

(defun appkit-media--open-file-cache-target
    (resource cache-key cache-directory)
  "Return RESOURCE target for CACHE-KEY in CACHE-DIRECTORY."
  (unless (and (stringp cache-directory)
               (not (string-empty-p cache-directory)))
    (user-error "Media: file cache directory is required"))
  (let* ((url (alist-get 'url resource))
         (name (appkit-media-sanitize-filename
                (or (appkit-media-resource-name resource) "media.bin")))
         (key (or cache-key (format "open-file-url:%s" url)))
         (directory (expand-file-name "open" cache-directory)))
    (expand-file-name
     (format "%s-%s"
             (substring (md5 (format "%s" key)) 0 10)
             name)
     directory)))

(defun appkit-media--cache-file-resource-for-open
    (resource cache-key cache-directory callback errback)
  "Cache RESOURCE under CACHE-KEY in CACHE-DIRECTORY.

Pass its path to CALLBACK, or a reason to ERRBACK."
  (let ((target
         (appkit-media--open-file-cache-target
          resource cache-key cache-directory)))
    (if (appkit-media-file-present-p target)
        (funcall callback target)
      (appkit-media-copy-or-download-resource-async
       resource target callback errback))))


(defun appkit-media--start-owned-open-transfer
    (owner start success error)
  "Run START and bind its asynchronous transfer to OWNER.

START receives guarded success and error callbacks.  A dead OWNER cancels the
transfer, and callbacks arriving after owner death have no visible effect."
  (unless (or (null owner) (appkit-owner-live-p owner))
    (user-error "Media owner is no longer live"))
  (let (transfer lifecycle-handle completed-p)
    (cl-labels
        ((finish (callback value)
           (unless completed-p
             (setq completed-p t)
             (when lifecycle-handle
               (appkit-retire-handle lifecycle-handle))
             (when (or (null owner) (appkit-owner-live-p owner))
               (funcall callback value)))))
      (setq transfer
            (funcall start
                     (lambda (value) (finish success value))
                     (lambda (value) (finish error value))))
      (when (and owner
                 (appkit-media-transfer-p transfer)
                 (not completed-p))
        (if (or (null owner) (appkit-owner-live-p owner))
            (condition-case err
                (setq lifecycle-handle
                      (appkit-register-handle
                       owner 'function transfer
                       #'appkit-media-cancel-transfer))
              ((error quit)
               (appkit-media-cancel-transfer transfer)
               (signal (car err) (cdr err))))
          (appkit-media-cancel-transfer transfer)))
      transfer)))

(cl-defun appkit-media-open-resource
    (resource &key kind cache-key cache-directory cache-update-function
              (cache-policy appkit-media-video-cache-policy)
              (client-label "media") owner)
  "Open canonical RESOURCE according to semantic KIND without a browser.

Images and non-video remote files complete an atomic transfer before opening.
Videos stream immediately; automatic CACHE-POLICY retains their progressive
cache only if complete, while `none' keeps it session-local.  CACHE-DIRECTORY
and CACHE-KEY identify reusable storage.  CACHE-UPDATE-FUNCTION receives a copy
after a local cache is known, CLIENT-LABEL prefixes messages, and OWNER owns
the transfer or viewer buffer."
  (let* ((resource (appkit-media-resource-normalize resource))
         (kind (appkit-media-resource-kind resource kind))
         (file (alist-get 'file resource)))
    (cl-labels
        ((remember (local-file)
           (setf (alist-get 'file resource nil nil #'eq) local-file)
           (when (functionp cache-update-function)
             (funcall cache-update-function (copy-tree resource)))
           local-file)
         (open-local (local-file)
           (when (or (null owner) (appkit-owner-live-p owner))
             (let ((remembered (remember local-file)))
               (when (or (null owner) (appkit-owner-live-p owner))
                 (appkit-media-open-file remembered))))))
      (pcase kind
        ('video
         (appkit-media--play-video-resource
          resource client-label
          :owner owner
          :cache-key cache-key
          :cache-directory cache-directory
          :cache-update-function cache-update-function
          :cache-policy cache-policy))
        ('image
         (if (appkit-media-file-present-p file)
             (open-local file)
           (appkit-media--start-owned-open-transfer
            owner
            (lambda (success error)
              (appkit-media--cache-image-resource-for-open
               resource cache-key cache-directory client-label success error))
            #'open-local
            (lambda (reason)
              (message "%s: failed to open image: %s"
                       client-label reason)))
           nil))
        (_
         (if (appkit-media-file-present-p file)
             (open-local file)
           (appkit-media--start-owned-open-transfer
            owner
            (lambda (success error)
              (appkit-media--cache-file-resource-for-open
               resource cache-key cache-directory success error))
            #'open-local
            (lambda (reason)
              (message "%s: failed to open media: %s"
                       client-label reason)))
           nil))))))

(provide 'appkit-media-resource)

;;; appkit-media-resource.el ends here
