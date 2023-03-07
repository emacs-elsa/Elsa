(require 'dash)

;;    (elsa--find-dependency :: (function (string) (or nil string)))
(defun elsa--find-dependency (library-name)
  "Find the implementation file of dependency LIBRARY-NAME.

LIBRARY-NAME should be the feature name (not symbol)."
  (let* ((load-suffixes (list ".el" ".el.gz"))
         (load-file-rep-suffixes (list "")))
    (locate-library library-name)))

(defun elsa--get-dep-alist (file &optional current-library state)
  "Get dependencies of FILE and all its dependencies recursively.

STATE is a plist with two keys:

- :visited is a list that keeps track of visited files so we do not
  add the same library multiple times.
- :deps is an alist of dependencies with `car' being the library name
  and `cdr' its requires.

Return the state."
  (unless state
    (setq state (list :visited nil :deps nil)))
  (setq current-library (or current-library file))
  (with-temp-buffer
    (let ((jka-compr-verbose nil)) (insert-file-contents file))
    (emacs-lisp-mode)
    (goto-char (point-min))
    (while (re-search-forward
            (rx
             "(" (*? whitespace)
             "require" (*? whitespace)
             "'" (group (+? (or word (syntax symbol))))
             (*? whitespace) ")")
            nil t)
      (unless (or (nth 4 (syntax-ppss))
                  ;(< 0 (car (syntax-ppss)))
                  )
        (let* ((library-name (match-string 1))
               (library (elsa--find-dependency library-name)))
          (when library
            (let ((deps (plist-get state :deps)))
              (push library-name (alist-get current-library deps nil nil #'equal))
              (setq state (plist-put state :deps deps)))
            (unless (member library (plist-get state :visited))
              (setq state (plist-put state :visited
                                     (cons library (plist-get state :visited))))
              (setq state (elsa--get-dep-alist library library-name state)))))))
    state))

;; (elsa--fold-alist-to-tree :: (function (list string (or (list string) nil) (or (list string) nil) (or int nil)) mixed))
(defun elsa--fold-alist-to-tree (deps start &optional visited parents depth)
  "Fold DEPS alist to tree from START.

Repeated dependencies are not expanded, that is, each feature
only lists its dependencies exactly once.

Circular dependencies are disambiguated to prevent infinite
loops."
  (setq visited (or visited (list nil)))
  (setq parents (or parents nil))
  (setq depth (or depth 0))
  (if (member start (car visited))
      (if (member start parents)
          (list (concat start "-CIRCULAR"))
        (list start))
    (push start (car visited))
    (let ((dependencies (cdr (assoc start deps))))
      (cons start
            (--map (elsa--fold-alist-to-tree
                    deps it
                    visited
                    (cons start parents)
                    (1+ depth))
                   (reverse dependencies))))))

;;    (elsa-get-dep-tree :: (function (string) mixed))
(defun elsa-get-dep-tree (file)
  "Recursively crawl require forms starting from FILE.

Only top-level `require' forms are considered."
  (elsa--fold-alist-to-tree
   (plist-get (elsa--get-dep-alist file) :deps)
   file))

(defun elsa--tree-to-deps (tree &optional deps)
  (setq deps (or deps nil))
  (when (and (listp tree) (< 1 (length tree)))
    (progn
      (let ((head-dependencies (-map #'car (cdr tree))))
        (dolist (hd head-dependencies)
          (push hd (alist-get (car tree) deps))))
      (dolist (p (cdr tree))
        (setq deps (elsa--tree-to-deps p deps)))))
  deps)

(defun elsa-tree-to-deps (tree)
  "Convert TREE of dependencies to adjacency list graph representationn."
  (let ((deps (elsa--tree-to-deps tree)))
    (mapcar
     (lambda (dep)
       (cons (car dep) (-uniq (cdr dep))))
     deps)))

(defun elsa-topo-sort (deps start)
  "Topologically sort DEPS starting at START node."
  (let ((candidates (list start))
        (result nil)
        (deps (copy-sequence deps)))
    (while candidates
      (let* ((current (pop candidates))
             (current-deps (cdr (assoc current deps))))
        (setq deps (assoc-delete-all current deps))
        (push current result)
        (dolist (dependency current-deps)
          (unless (--some (member dependency (cdr it)) deps)
            (push dependency candidates)))))
    result))

;;    (elsa-get-dependencies :: (function (string (or int nil)) mixed))
(defun elsa-get-dependencies (file &optional min-depth)
  "Get all recursive dependencies of FILE.

The order is such that if we load the features in order we will
satisfy all inclusion relationships.

MIN-DEPTH will fold all the dependencies of greater depth only."
  (setq min-depth (or min-depth 0))
  (let ((folded-deps (elsa-get-dep-tree file)))
    (if (> min-depth 0)
        (--map (elsa-topo-sort (elsa-tree-to-deps it) (car it))
               (cdr folded-deps))
      (elsa-topo-sort (elsa-tree-to-deps folded-deps) file))))

(provide 'elsa-dependencies)
