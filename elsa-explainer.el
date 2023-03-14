;;; elsa-explainer.el --- Elsa explainer -*- lexical-binding: t -*-

(require 'dash)

;; (elsa--flat-to-tree :: (function ((list mixed) (or int nil)) (list mixed)))
(defun elsa--flat-to-tree (lst &optional depth)
  "Convert a flat list LST into a tree.

LST is a list of items (depth message) which are converted to a
tree structure.  All the messages on the same depth are also
inverted (relative to their parent).

The input to this function is created by pushing messages to
`elsa-explainer'."
  (setq depth (or depth 0))
  (let ((layer nil)
        (block nil))
    (while lst
      (push (pop lst) block)
      (while (and lst (> (caar lst) depth))
        (push (pop lst) block))
      (push (reverse block) layer)
      (setq block nil))
    (setq layer (reverse layer))
    (mapcar (lambda (l)
              (append
               (car l)
               (reverse (elsa--flat-to-tree (cdr l) (1+ depth)))))
            layer)))

;; (elsa--graphify :: (function ((list mixed) (or string nil)) string))
(defun elsa--graphify (tree &optional prefix)
  "Convert TREE to a graphical tree.

PREFIX is the current string prefix for the processed row.

Return a multi-line string with rendered graph."
  (setq prefix (or prefix ""))
  (cond
   ((consp (car tree))
    (-map (lambda (item) (cons prefix (elsa--graphify item prefix))) tree))
   ((null tree)
    (cdr tree))
   (t (let ((head (seq-take tree 2))
            (children (nthcdr 2 tree)))
        (append
         head
         (-map-indexed
          (lambda (index node)
            (cond
             ((= index (1- (length children)))
              (cons (concat prefix "└─ ")
                    (elsa--graphify node (concat prefix "   "))))
             (t (cons (concat prefix "├─ ")
                      (elsa--graphify node (concat prefix "│  "))))))
          children))))))

(defclass elsa-explainer-message ()
  ((message
    :type string
    :initarg :message
    :accessor elsa-get-message)
   (depth
    :type integer
    :initarg :depth
    :accessor elsa-get-depth
    :accessor "Absolute depth.")
   (depthrel
    :type integer
    :initarg :depthrel
    :accessor elsa-get-depthrel
    :documentation "Depth relative to the previous message.

This is used when the explainer is converted to string to resolve real
depth in case one explainer was appended to another.")
   (prefix
    :type string
    :initarg :prefix
    :initform "")))

(cl-defmethod elsa-tostring ((this elsa-explainer-message))
  (format "%s%s" (oref this prefix) (oref this message)))

(cl-defmethod cl-print-object ((this elsa-explainer-message) stream)
  (princ (format "#<elsa-explainer-message %s %s %s %s>"
                 (oref this prefix)
                 (oref this depth)
                 (oref this depthrel)
                 (oref this message))
         stream))

(defclass elsa-explainer ()
  ((messages
    :type (list-of elsa-explainer-message)
    :initform nil
    :accessor elsa-get-messages)
   (depth
    :type integer
    :initform 0
    :accessor elsa-get-depth)))

(cl-defmethod cl-print-object ((this elsa-explainer) stream)
  (princ (format "#<elsa-explainer\n%s>"
                 (mapconcat
                  #'cl-prin1-to-string
                  (elsa-get-messages this)
                  "\n"))
         stream))

(cl-defmethod elsa-tostring ((this elsa-explainer))
  (let ((initial-prefix (make-string (max (* 2 (1- (elsa-get-depth this))) 0) ? )))
    (->> (elsa-get-messages this)
         (--map (list (oref it depth) (oref it message)))
         (funcall (-rpartial #'elsa--flat-to-tree (elsa-get-depth this)))
         (nreverse)
         ;; (cons "")
         ;; (cons (elsa-get-depth this))
         ;; (list)
         (funcall (-rpartial #'elsa--graphify initial-prefix))
         (-flatten)
         (-partition 3)
         (--map (elsa-explainer-message
                 :prefix (car it)
                 :depth (cadr it)
                 :message (nth 2 it)))
         (-map #'elsa-tostring)
         (funcall (-flip #'string-join) "\n")
         (replace-regexp-in-string (concat "^" initial-prefix "\n") ""))))

(cl-defmethod elsa--reset-depth ((this elsa-explainer))
  (let* ((messages (elsa-get-messages this))
         (min-depth-message (--min-by (> (oref it depth)
                                         (oref other depth))
                                      messages))
         (min-depth (or (and min-depth-message
                             (elsa-get-depth min-depth-message))
                        0)))
    (--each messages (cl-decf (elsa-get-depth it) min-depth))
    (setf (elsa-get-depth this) 0))
  this)

(cl-defmethod elsa--append-explainer ((this elsa-explainer)
                                      (other elsa-explainer))
  "Append OTHER explainer to THIS on the same level."
  (let* ((this-messages (copy-sequence (elsa-get-messages this)))
         (other-messages (mapcar #'clone (elsa-get-messages other)))
         (prev-level (elsa-get-depth this)))
    (--each (reverse other-messages)
      (setf (elsa-get-depth it)
            (setq prev-level (+ prev-level (elsa-get-depthrel it)))))
    (setf (elsa-get-messages this)
          (append other-messages this-messages))
    this))

(cl-defmethod elsa--append-explainer ((this elsa-explainer)
                                      (other string))
  (elsa--append-explainer
   this
   (elsa-with-temp-explainer explainer
     (elsa-explain explainer other)
     explainer)))

(cl-defmethod elsa--attach-explainer ((this elsa-explainer)
                                      (other elsa-explainer))
  "Attach OTHER explainer to THIS indented by one extra level."
  (let* ((this-messages (copy-sequence (elsa-get-messages this)))
         (other-messages (mapcar #'clone (elsa-get-messages other)))
         (prev-level (1+ (elsa-get-depth this))))
    (--each (reverse other-messages)
      (setf (elsa-get-depth it)
            (setq prev-level (+ prev-level (elsa-get-depthrel it)))))
    (setf (elsa-get-messages this)
          (if this-messages
              (append (list (car this-messages))
                      other-messages
                      (cdr this-messages))
            other-messages))
    this))

(defmacro elsa-explain (explainer fmt &rest args)
  "Push new message to EXPLAINER formatted using FMT.

ARGS are fed to `format' as arguments to format string FMT."
  (declare (indent 1)
           (debug (form form body)))
  (let ((exp (if (symbolp explainer) explainer (make-symbol "exp"))))
    `(,@(if (symbolp explainer) '(progn) `(let ((,exp ,explainer))))
      (when ,exp
        (push (elsa-explainer-message
               :message (format ,fmt ,@args)
               :depth (elsa-get-depth ,exp)
               :depthrel (let ((msg (car (elsa-get-messages ,exp))))
                           (- (elsa-get-depth ,exp)
                              (or (and msg (elsa-get-depth msg)) 0))))
              (elsa-get-messages ,exp)))
      ,exp)))

(defmacro elsa-explain-and-indent (explainer fmt &rest body)
  "Append FMT then indent the EXPLAINER, call BODY, and dedent.

Return value of the last form in BODY."
  (declare (indent 1)
           (debug (form (sexp &rest form) body)))
  (let ((exp (if (symbolp explainer) explainer (make-symbol "exp"))))
    `(,@(if (symbolp explainer) '(progn) `(let ((,exp ,explainer))))
      (cl-incf (elsa-get-depth ,exp))
      (prog1 (progn ,@body)
        (cl-decf (elsa-get-depth ,exp))
        (elsa-explain ,exp ,@fmt)))))

(defmacro elsa-with-explainer (explainer fmt &rest body)
  "Use EXPLAINER with format FMT to explain BODY.

FMT is a list with `car' being the format string and `cdr'
optional arguments to the format string.

The message is only added to the explainer if BODY return nil."
  (declare (indent 1)
           (debug (form (sexp &rest form) body)))
  (let ((exp (if (symbolp explainer) explainer (make-symbol "exp"))))
    `(,@(if (symbolp explainer) '(progn) `(let ((,exp ,explainer))))
       (if (progn
             (when ,exp (cl-incf (elsa-get-depth ,exp)))
             (prog1 (progn
                      ,@body)
               (when ,exp (cl-decf (elsa-get-depth ,exp)))))
           t
         (elsa-explain ,exp ,@fmt)
         nil))))

(defmacro elsa-with-temp-explainer (explainer &rest body)
  "Run BODY with a temporary explainer.

EXPLAINER is a symbol to which the new instance is bound.

Return value of the last form in BODY."
  (declare (indent 1)
           (debug (sexp body)))
  `(let ((,explainer (elsa-explainer)))
     ,@body))

(provide 'elsa-explainer)
;;; elsa-explainer.el ends here
