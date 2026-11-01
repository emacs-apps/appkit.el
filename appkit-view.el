;;; appkit-view.el --- Composable views for generated buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; A view is a value, with no state or lifecycle of its own.  Ordinary functions
;; compose nodes; `appkit-view' is their authoring syntax.  Project keyed roots
;; with `appkit-view-project', print them with `appkit-view-insert', and use
;; `appkit-view-anchor' as the projection's position property.  A keyed child
;; preserves its position within a root even when preceding content grows.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-projection)
(require 'appkit-ui)

(cl-defstruct (appkit-view-node (:constructor appkit-view--node))
  "A view description.  Treat its properties and children as immutable."
  kind properties children)

(defun appkit-view--nodes (value)
  "Flatten a node or nested lists of nodes in VALUE, omitting nil."
  (cond ((null value) nil)
        ((appkit-view-node-p value) (list value))
        ((and (consp value) (proper-list-p value))
         (mapcan #'appkit-view--nodes value))
        (t (error "Expected an Appkit view, got %S" value))))

(defun appkit-view-create (kind properties &rest children)
  "Construct a KIND view from PROPERTIES and CHILDREN.
Containers card, group and actions accept nodes or lists returned by components.
Title, text and button take one string (or nil); field takes a label and value.
Properties are :key, :face, :help-echo, :action and :compact.  Keys identify
siblings; root keys identify projection rows.  Actions are ordinary Appkit
function/argument values.  No expression is evaluated by the renderer."
  (unless (and (proper-list-p properties) (zerop (% (length properties) 2)))
    (error "Expected view property pairs: %S" properties))
  (let (seen)
    (cl-loop for (key _value) on properties by #'cddr do
             (unless (and (memq key '(:key :face :help-echo :action :compact))
                          (not (memq key seen)))
               (error "Unknown or duplicate view property: %S" key))
             (push key seen)))
  (pcase kind
    ((or 'card 'group 'actions)
     (setq children (appkit-view--nodes children))
     (let ((keys (make-hash-table :test #'equal)))
       (cl-labels ((claim (child)
                     (if-let* ((key (plist-get (appkit-view-node-properties child) :key)))
                         (progn
                           (when (gethash key keys) (error "Duplicate child view key: %S" key))
                           (puthash key t keys))
                       (when (memq (appkit-view-node-kind child) '(card group actions))
                         (mapc #'claim (appkit-view-node-children child))))))
         (mapc #'claim children))))
    ((or 'title 'text 'button)
     (unless (and (= (length children) 1)
                  (or (null (car children)) (stringp (car children))))
       (error "%s requires one string or nil" kind)))
    ('field
     (unless (and (= (length children) 2) (stringp (car children)))
       (error "Field requires a label string and value")))
    (_ (error "Unknown view kind: %S" kind)))
  (when-let* ((action (plist-get properties :action)))
    (unless (appkit-ui-action-p action) (error "Invalid view action: %S" action)))
  (when (and (eq kind 'button) (car children) (null (plist-get properties :action)))
    (error "Button requires an action"))
  (appkit-view--node :kind kind :properties properties :children children))

(eval-and-compile
  (defun appkit-view--expand (form)
    "Compile structural FORM to ordinary node construction."
    (pcase form
      (`(expr ,expression) expression)
      (`(,kind . ,tail)
       (unless (memq kind '(card group actions title text field button))
         (error "Unknown view form; use (expr FORM) for Lisp: %S" kind))
       (let (properties)
         (while (keywordp (car tail))
           (unless (cdr tail) (error "Missing view property value: %S" (car tail)))
           (push (pop tail) properties)
           (push (pop tail) properties))
         `(appkit-view-create ',kind (list ,@(nreverse properties))
                              ,@(if (memq kind '(card group actions))
                                    (mapcar #'appkit-view--expand tail)
                                  tail))))
      ('nil nil)
      (_ (error "Invalid view form: %S" form)))))

(defmacro appkit-view (form)
  "Build a view from FORM: (KIND :PROPERTY VALUE ... CHILD ...).
Property values and leaf contents are ordinary Lisp expressions, evaluated
once when constructing the view.  Container children are view forms; use
(expr FORM) to include nodes returned by a function, loop or conditional.
Components are ordinary functions returning nodes or lists of nodes."
  (declare (indent 0) (debug (form)))
  (appkit-view--expand form))

(defun appkit-view-project (views)
  "Turn keyed root VIEWS into rows for an existing Appkit projection."
  (mapcar (lambda (node)
            (let ((key (plist-get (appkit-view-node-properties node) :key)))
              (unless key (error "Projected view requires a non-nil key"))
              (appkit-projection-row-create :key key :payload node)))
          (appkit-view--nodes views)))

(defun appkit-view-text (view)
  "Return the searchable text of VIEW, excluding keys, actions and styling."
  (mapconcat
   (lambda (node)
     (let ((children (appkit-view-node-children node)))
       (pcase (appkit-view-node-kind node)
         ((or 'card 'group 'actions) (appkit-view-text children))
         ('field (format "%s：%s" (car children) (cadr children)))
         (_ (or (car children) "")))))
   (appkit-view--nodes view) "\n"))

(defun appkit-view-insert (view)
  "Insert VIEW as ordinary buffer text, native buttons and semantic anchors.
The caller owns the generated-buffer mutation boundary.  Root anchors use
root keys; keyed descendants use paths scoped by their keyed ancestors.
Unkeyed groups are transparent.  Text stays searchable and copyable."
  (let (spans)
    (cl-labels
        ((emit (node parent)
           (let* ((kind (appkit-view-node-kind node))
                  (props (appkit-view-node-properties node))
                  (children (appkit-view-node-children node))
                  (key (plist-get props :key))
                  (anchor (if (and parent key) (vector 'appkit-view parent key)
                            (or key parent)))
                  (start (point)))
             (pcase kind
               ((or 'card 'group)
                (dolist (child children) (emit child anchor))
                (when (eq kind 'card)
                  (unless (bolp) (insert "\n"))
                  (unless (plist-get props :compact) (insert "\n"))))
               ('actions
                (dolist (child children)
                  (unless (= start (point)) (insert "  "))
                  (emit child anchor))
                (unless (= start (point)) (insert "\n")))
               ((or 'title 'text)
                (when-let* ((text (car children)))
                  (unless (and (eq kind 'title) (string-empty-p text))
                    (insert text "\n"))))
               ('field (insert (format "%s：%s\n" (car children) (cadr children))))
               ('button
                (when (car children)
                  (appkit-ui-insert-action-button
                   (concat "[" (car children) "]") (plist-get props :action)))))
             (when-let* ((face (or (plist-get props :face) (and (eq kind 'title) 'bold))))
               (add-face-text-property start (point) face t))
             (when-let* ((help (plist-get props :help-echo)))
               (put-text-property start (point) 'help-echo help))
             (unless (eq kind 'button)
               (appkit-ui-add-fallback-action start (point) (plist-get props :action)))
             ;; Parents are applied first, then child anchors take precedence.
             (when anchor (push (list start (point) anchor) spans)))))
      (dolist (node (appkit-view--nodes view)) (emit node nil)))
    (dolist (span spans)
      (put-text-property (nth 0 span) (nth 1 span) 'appkit-view-anchor (nth 2 span)))))

(provide 'appkit-view)
;;; appkit-view.el ends here
