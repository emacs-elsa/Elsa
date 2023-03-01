(package "elsa"
         "0.1.0"
         "Emacs Lisp Static Analyser")

(website-url "https://github.com/emacs-elsa/Elsa")
(keywords "languages" "lisp")

(package-file "elsa.el")

(files "elsa.el" "elsa-*.el" "bin/elsa*")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "26.1")
(depends-on "dash" "2.14")
(depends-on "seq")
(depends-on "f")
(depends-on "trinary" "1.2.1")
(depends-on "lsp-mode")
(depends-on "ansi")

(development
 (depends-on "elsa")
 (depends-on "undercover")
 (depends-on "buttercup"))
