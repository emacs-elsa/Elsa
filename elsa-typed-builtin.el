(require 'elsa-types)
(require 'elsa-type-helpers)

;; boolean functions
(put 'not 'elsa-type (elsa-make-type-fn mixed -> bool))

;; type specifiers
(put 'stringp 'elsa-type (elsa-make-type-fn mixed -> bool))
(put 'vectorp 'elsa-type (elsa-make-type-fn mixed -> bool))
(put 'integerp 'elsa-type (elsa-make-type-fn mixed -> bool))
(put 'numberp 'elsa-type (elsa-make-type-fn mixed -> bool))
(put 'floatp 'elsa-type (elsa-make-type-fn mixed -> bool))
(put 'consp 'elsa-type (elsa-make-type-fn mixed -> bool))
(put 'listp 'elsa-type (elsa-make-type-fn mixed -> bool))

;; type conversion
(put 'string-to-number 'elsa-type (elsa-make-type-fn string -> number))
(put 'number-to-string 'elsa-type (elsa-make-type-fn number -> string))

;; list functions
(put 'list 'elsa-type (elsa-make-type-fn mixed -> list)) ;; TODO: variadic args?
(put 'car 'elsa-type (elsa-make-type-fn [&or cons nil] -> mixed))
(put 'cdr 'elsa-type (elsa-make-type-fn [&or cons nil] -> mixed))
(put 'nth 'elsa-type (elsa-make-type-fn int -> list -> mixed))

;; string functions
(put 'split-string'elsa-type (elsa-make-type-fn string -> string -> [string]))

;; sequence functions

;; built-in variables
(put 'command-line-args-left 'elsa-type-var (elsa-make-type [&or [string] nil]))

;; help.el
(put 'help-function-arglist 'elsa-type (elsa-make-type-fn symbol -> [symbol]))

(provide 'elsa-typed-builtin)
