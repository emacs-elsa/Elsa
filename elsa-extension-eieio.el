(require 'elsa-analyser)
(require 'elsa-state)

(require 'elsa-typed-eieio)
(require 'elsa-extension-cl)

(defun elsa--eieio-analyse-obj-slot (instance slot-form state)
  (let ((slot-name (elsa-get-name slot-form))
        (inst-type (elsa-get-type instance)))
    (when (elsa-struct-type-p inst-type)
      (when-let* ((struct (get (oref inst-type name) 'elsa-cl-structure)))
        (let ((slots (-mapcat
                      (lambda (par)
                        (when-let ((par-struct (get par 'elsa-cl-structure)))
                          (oref par-struct slots)))
                      (mapcar #'car (oref struct parents)))))
          (unless (memq slot-name slots)
            (elsa-state-add-message state
              (elsa-make-error slot-form
                "Type `%s' has no slot `%s', has %s"
                (elsa-type-describe inst-type)
                slot-name
                (or slots "no slots")))))))))

(defun elsa--eieio-assert-struct-for-obj (instance state)
  (let ((type (elsa-get-type instance)))
    (unless (elsa-type-accept (elsa-make-type (struct nil)) type)
      (elsa-state-add-message state
        (elsa-make-error instance
          "Type `%s' has no properties because it is not a struct."
          (elsa-type-describe type))))))

(defun elsa--analyse:oref (form scope state)
  (let* ((instance (elsa-cadr form))
         (slot-form (elsa-nth 2 form))
         (slot-name (elsa-get-name slot-form)))
    (elsa--analyse-macro form (list t nil) scope state)
    (elsa--eieio-assert-struct-for-obj instance state)
    (elsa--eieio-analyse-obj-slot instance slot-form state)))

(defun elsa--analyse:oset (form scope state)
  (let* ((instance (elsa-cadr form))
         (slot-form (elsa-nth 2 form))
         (slot-name (elsa-get-name slot-form)))
    (elsa--analyse-macro form (list t nil t) scope state)
    (elsa--eieio-assert-struct-for-obj instance state)
    (elsa--eieio-analyse-obj-slot instance slot-form state)))

(defun elsa--analyse:defclass (form scope state)
  (let* ((name (elsa-get-name (elsa-cadr form)))
         (parents (elsa-nth 2 form))
         (parents-names (when (elsa-form-list-p parents)
                          (elsa-form-map parents #'elsa-get-name)))
         (slots (elsa-nth 3 form))
         (slot-names (elsa-form-map slots #'elsa-get-name))
         (parents-tree (-fix
                        (lambda (parents)
                          (-mapcat
                           (lambda (p)
                             (if (symbolp p)
                                 (let* ((str (elsa-state-get-structure state p))
                                        (pars (and str (oref str parents)))
                                        (pts (cdr (assq p pars))))
                                   (cons (cons p (copy-sequence pts)) (copy-sequence pts)))
                               (list p)))
                           parents))
                        parents-names)))
    (elsa-state-add-structure state
      (elsa-cl-structure
       :name name
       :slots slot-names
       :parents (let ((-compare-fn (-on #'eq #'car)))
                  (-uniq (cons (cons name parents-names) parents-tree)))))

    ;; add the type predicate
    (elsa-state-add-defun state
      (elsa-defun
       :name (intern (concat (symbol-name name) "-p"))
       :type (elsa--make-type `(function (mixed) (is (struct ,name))))
       :arglist (list 'x)))))

(provide 'elsa-extension-eieio)
