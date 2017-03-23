#  Elsa - Emacs Lisp Static Analyser

(Your favourite princess now in Emacs!)

Elsa is a tool that analyses your code without loading or running it.  It can track types and provide helpful hints when things don't match up before you even try to run the code.

# How do I run it

**(work in progress)**

Load the `elsa.el` file, then open the target file (e.g. `examples.el`) and eval `(elsa-process-file (buffer-file-name))` in `M-:`.  It will dump the analysis state.

# How can I help Elsa to be better at the analysis?

Elsa works out of the box without you having to annotate anything, however, the results will be largely useless.  You can help by adding metadata to your code which will get picked up and help refine the analysis.

## Functions

You can annotate your functions with the native `declare` macro:

``` emacs-lisp
(defun my-function (first second)
   "Some function"
   (declare (elsa-args string string)
            (elsa-return int))
   (+ (string-to-number first) (string-to-number second)))
```

This declaration does not slow down your code at all because it is thrown out when you byte-compile your code (or eval the function).  The `declare` form is also used in Emacs lisp for macro instrumentation and for attaching other metadata.

## Variables defined as `defcustom`

Elsa (will be able) can parse `:type` declarations from `defcustom` forms so you don't have to do anything!

## Variables defined as `defvar`

Wrap the `defvar` (or `defvar-local`) form in `elsa-type` (see below.)

## Annotate any other form with `elsa-type`

You can wrap any form in `elsa-type` macro with second argument being the requested type.  This is a manual "cast" operation.  You shouldn't need to use it very often, but one case where it is handy is wrapping of `defvar`s.

# How can I contribute to this project

Open an issue if you want to work on something (not necessarily listed below in the roadmap) so we won't duplicate work.  Or just give us feedback or helpful tips.

## Roadmap

- [ ] Make it use Cask
- [ ] Setup buttercup tests
    - [ ] And then write tests :)
- [ ] Make a command that will analyze current buffer and show the errors somehow
    - [ ] Make a flycheck extension.
- [ ] Keep adding more built-in types
- [ ] Support type narrowing for all the special forms like `and`, `or` and the built-in predicates (you can discard some parts of sum types when you are sure something passes e.g. `stringp`).
- [ ] Add some way to declare types of Emacs built-ins (I don't expect anyone any time soon going in and annotating the Emacs internal functions).  Can be also used for external packages not yet annotated.
- [ ] Make it load files according to `require` forms and scoop the definitions from the other files.
- [ ] Add some sort of type inference (e.g. return type of function from last/return forms)
- [ ] Allow generic types
- [ ] Make the analysis better
    - [ ] Track local variables from let-bindings
    - [ ] Add undeclared variable warning (e.g. `setq` on not-declared variable or just using it)
    - [ ] Add unused variable warnings
    - [ ] Analyze all calls not just top-level
