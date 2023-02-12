(package "elsa"
         "0.1.0"
         "Emacs Lisp Static Analyser")

(website-url "https://github.com/emacs-elsa/Elsa")
(keywords "languages" "lisp")

(package-file "elsa.el")

(files "elsa-*.el" "bin/elsa*")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "26.1")
(depends-on "dash" "2.14")
(depends-on "seq" "0")
(depends-on "f" "0")
(depends-on "trinary" "1.0.0")

(development
 (depends-on "elsa")
 (depends-on "undercover")
 (depends-on "buttercup"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
