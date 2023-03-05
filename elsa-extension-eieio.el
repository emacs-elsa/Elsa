;; -*- lexical-binding: t -*-

(require 'elsa-explainer)
(require 'elsa-analyser)
(require 'elsa-type-helpers)
(require 'elsa-state)

(require 'elsa-typed-eieio)

(defun elsa--eieio-analyse-obj-slot (instance slot-form state)
  (when slot-form
    (let ((slot-name (elsa-get-name slot-form))
          (inst-type (elsa-get-type instance)))
      (when (elsa-class-type-p inst-type)
        (when-let* ((class (get (oref inst-type name) 'elsa-defclass)))
          (let* ((slots (elsa-get-slots class))
                 (slot-names (--map (oref it name) slots))
                 (slot (--find (eq slot-name (oref it name)) slots)))
            (oset slot-form type (or (and slot (oref slot type))
                                     (elsa-type-mixed)))
            (unless (memq slot-name slot-names)
              (elsa-state-add-message state
                (elsa-make-error slot-form
                  "Type `%s' has no slot `%s', has %s"
                  :code "eieio-invalid-slot"
                  (elsa-type-describe inst-type)
                  slot-name
                  (or slot-names "no slots"))))))))))

(defun elsa--eieio-assert-struct-for-obj (instance state)
  (let ((type (elsa-get-type instance)))
    ;; This test instead of `elsa-class-type-p' is so that mixed can
    ;; be used as a struct.
    (unless (elsa-type-assignable-p (elsa-make-type (class nil)) type)
      (elsa-state-add-message state
        (elsa-make-error instance
          "Type `%s' has no properties because it is not a class."
          :code "eieio-not-a-class"
          (elsa-type-describe type))))))

(defun elsa--analyse:oref (form scope state)
  (let* ((instance (elsa-cadr form))
         (slot-form (elsa-nth 2 form)))
    (elsa--analyse-macro form (list t nil) scope state)
    (elsa--eieio-assert-struct-for-obj instance state)
    (elsa--eieio-analyse-obj-slot instance slot-form state)
    (oset form type (oref slot-form type))))

(defun elsa--analyse:oset (form scope state)
  (let* ((instance (elsa-cadr form))
         (slot-form (elsa-nth 2 form))
         (value-form (elsa-nth 3 form))
         (slot-name (elsa-get-name slot-form)))
    (elsa--analyse-macro form (list t nil t) scope state)
    (elsa--eieio-assert-struct-for-obj instance state)
    (elsa--eieio-analyse-obj-slot instance slot-form state)
    (unless (elsa-type-assignable-p
             (oref slot-form type)
             (oref value-form type))
      (elsa-state-add-message state
        (elsa-make-error slot-form
          (elsa-with-temp-explainer explainer
            (elsa-explain-and-indent explainer
              ("Property `%s' can not accept type `%s', has type `%s'"
               slot-name
               (elsa-tostring (oref value-form type))
               (elsa-tostring (oref slot-form type)))
              (elsa-type-accept (oref slot-form type) (oref value-form type) explainer))
            explainer)
          :code "eieio-invalid-type"
          :compact t)))))

(defun elsa-eieio--create-slots (slots)
  (let ((ht (make-hash-table)))
    (--each slots
      (let ((slot (elsa-structure-slot
                   :name (car it)
                   :type (plist-get (cdr it) :type))))
        (puthash (oref slot name) slot ht)))
    ht))

(defun elsa--analyse:defclass (form _scope state)
  (let* ((name (elsa-get-name (elsa-cadr form)))
         (parents (elsa-nth 2 form))
         (parents-names (when (elsa-form-list-p parents)
                          (elsa-form-map parents #'elsa-get-name)))
         (slots (elsa-nth 3 form))
         (slot-names (elsa-form-map slots #'elsa-get-name))
         (slot-types (elsa-form-map slots
                       (lambda (slot)
                         (let* ((next-is-type nil)
                                (type (catch 'type
                                        (elsa-form-foreach (elsa-cdr slot)
                                          (lambda (key)
                                            (if next-is-type
                                                (throw 'type (elsa-form-to-lisp key))
                                              (when (eq (elsa-get-name key) :type)
                                                (setq next-is-type t))))))))
                           (or (elsa--cl-type-to-elsa-type type)
                               (elsa-type-mixed))))))
         (parents-tree (-fix
                        (lambda (parents)
                          (-mapcat
                           (lambda (p)
                             (if (symbolp p)
                                 (let* ((str (elsa-state-get-defclass state p))
                                        (pars (and str (oref str parents)))
                                        (pts (cdr (assq p pars))))
                                   (cons (cons p (copy-sequence pts))
                                         (copy-sequence pts)))
                               (list p)))
                           parents))
                        parents-names)))
    (elsa-state-add-defclass state
      (elsa-defclass
       :name name
       :slots (elsa-eieio--create-slots
               (-zip-with
                (lambda (name type)
                  (list name :type type))
                slot-names slot-types))
       :parents (let ((-compare-fn (-on #'eq #'car)))
                  (-uniq (cons (cons name parents-names) parents-tree)))))

    ;; add the constructor
    (elsa-state-add-defun state
      (elsa-defun
       :name name
       :type (elsa--make-type `(function (&rest mixed) (class ,name)))
       :arglist (list '&rest 'args)))

    ;; add the type predicate
    (elsa-state-add-defun state
      (elsa-defun
       :name (intern (concat (symbol-name name) "-p"))
       :type (elsa--make-type `(function (mixed) (is (class ,name))))
       :arglist (list 'x)))))

(provide 'elsa-extension-eieio)
