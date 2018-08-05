(require 'elsa-analyser)

(defun elsa-dash--update-anaphora-scope (scope)
  "Add the `it' variable to SCOPE."
  (let ((it-var (elsa-variable
                 :name 'it
                  ;; TODO: derive type based on the list argument type
                 :type (elsa-make-type 'mixed))))
    (elsa-scope-add-variable scope it-var)
    it-var))

(defun elsa-dash--restore-anaphora-scope (it-var scope)
  "Remove the IT-VAR variable representing `it' from SCOPE."
  (elsa-scope-remove-variable scope it-var))

(defun elsa-dash--analyse-anaphora (form scope state)
  (let ((it-var (elsa-dash--update-anaphora-scope scope)))
    (elsa--analyse-function-call form scope state)
    (elsa-dash--restore-anaphora-scope it-var scope)))

(defun elsa--analyse:-when-let (form scope state)
  (let ((binding (cadr (oref form sequence)))
        (body (cddr (oref form sequence)))
        (var))
    (when binding
      (setq var (elsa--analyse-variable-from-binding binding scope state))
      (elsa-scope-add-variable scope var))
    (elsa--analyse-body body scope state)
    (when var
      (elsa-scope-remove-variable scope var))))

(defun elsa--analyse:--> (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--all-p (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--all? (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--annotate (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--any (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--any-p (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--any? (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--count (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--dotimes (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--drop-while (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--each (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--each-indexed (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--each-while (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--every-p (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--every? (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--filter (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--find (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--find-index (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--find-indices (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--find-last-index (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--first (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--fix (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--group-by (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--if-let (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--iterate (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--keep (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--last (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--map (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--map-first (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--map-indexed (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--map-last (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--map-when (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--mapcat (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--max-by (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--min-by (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--none-p (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--none? (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--only-some-p (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--only-some? (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--partition-by (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--partition-by-header (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--reduce (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--reduce-from (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--reduce-r (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--reduce-r-from (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--reject (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--reject-first (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--reject-last (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--remove (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--remove-first (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--remove-last (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--replace-where (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--select (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--separate (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--some (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--some-p (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--some? (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--sort (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--splice (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--splice-list (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--split-when (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--split-with (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--take-while (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--tree-map (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--tree-map-nodes (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--tree-mapreduce (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--tree-mapreduce-from (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--tree-reduce (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--tree-reduce-from (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--tree-seq (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--unfold (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--update-at (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--when-let (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(defun elsa--analyse:--zip-with (form scope state)
  (elsa-dash--analyse-anaphora form scope state))

(provide 'elsa-extension-dash)
