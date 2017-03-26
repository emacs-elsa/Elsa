#  Elsa - Emacs Lisp Static Analyser [![Build Status](https://travis-ci.org/Fuco1/Elsa.svg?branch=master)](https://travis-ci.org/Fuco1/Elsa)

(Your favourite princess now in Emacs!)

Elsa is a tool that analyses your code without loading or running it.
It can track types and provide helpful hints when things don't match
up before you even try to run the code.

# How do I run it

**(work in progress)**

Load the `elsa.el` file, then open the target file
(e.g. `examples.el`) and eval `(elsa-process-file (buffer-file-name))`
in `M-:`.  It will dump the analysis state.

# How can I help Elsa to be better at the analysis?

Elsa works out of the box without you having to annotate anything,
however, the results will be largely useless.  You can help by adding
type metadata to your code which will get picked up and help refine
the analysis.

## How to construct the "type" name

The types are symbols constructed according to the following guidelines:

- For built-in types with test predicates, drop the `p` or `-p` suffix to get the type:
    - `stringp` → `string`
    - `integerp` → `integer`
    - `markerp` → `marker`
    - `hash-table-p` → `hash-table`
- There are some built-in sum-types like `buffer-or-string` and
  `marker-or-integer` given that many Emacs functions accept these
  combinations.  For complete list refer to the documentation.
- Unqualified `alist` and `plist` as subtypes of lists (Note: it is
  not yet decided how to do container types).
- For `cl-defstruct` use the structure name, so `(cl-defstruct
  my-package-foo bar)` will have type `my-package-foo`.
- For EIEIO `defclass` use the class name.
- Sum types can be specified with `&or` syntax similar to `edebug`
  instrumentation, so `[&or string integer]` is a type accepting both
  strings or integers.

All types are by default non-nullable.  That means they do not accept
`nil` or a nullable version of itself as a value.  You can turn any
type into a nullable type by suffixing its symbol name with `?`.

To use a nullable type safely you will be forced to perform a
null-check; the analyzer can pick up the condition and restrict the
type in the "if" or "else" branch to the (non-)nullable variant.  In
some languages a nullable type is called `Maybe`, `Option` or similar.

Some more ideas, not fully fleshed out:

- A type that is a constant value.  In combination with sum types this
  can represent enumerations.
- Nested type specifications, so we can have a "list of (integer or
  lists of string)".

## Functions

You can annotate your functions with the native `declare` macro:

``` emacs-lisp
(defun my-function (first second)
   "Some function"
   (declare (elsa-args string string)
            (elsa-return int))
   (+ (string-to-number first) (string-to-number second)))
```

This declaration does not slow down your code at all because it is
thrown out when you byte-compile your code (or eval the function).
The `declare` form is also used in Emacs lisp for macro
instrumentation and for attaching other metadata.

## Variables defined as `defcustom`

Elsa (will be able) can parse `:type` declarations from `defcustom`
forms so you don't have to do anything!

## Variables defined as `defvar`

Wrap the `defvar` (or `defvar-local`) form in `elsa-type` (see below.)

## Annotate any form with `elsa-cast`

You can wrap any form with `elsa-cast` macro with second argument
being the requested type.  This is a manual "cast" operation.  You
shouldn't need to use it very often, but it is there for convenience around unannotated code.

This macro does nothing except wraps the `&rest` with a `progn` so it
shouldn't have any impact on performance.

# How can I contribute to this project

Open an issue if you want to work on something (not necessarily listed
below in the roadmap) so we won't duplicate work.  Or just give us
feedback or helpful tips.

## Roadmap

- [x] Make it use Cask
- [x] Setup buttercup tests
    - [-] And then write tests :)
- [-] Make a command that will analyze current buffer and show the
  errors somehow
    - [-] Make a flycheck extension.
- [-] Keep adding more built-in types
- [ ] Support type narrowing for all the special forms like `and`,
  `or` and the built-in predicates (you can discard some parts of sum
  types when you are sure something passes e.g. `stringp`).
- [ ] Add some way to declare types of Emacs built-ins (I don't expect
  anyone any time soon going in and annotating the Emacs internal
  functions).  Can be also used for external packages not yet
  annotated.
- [ ] Make it load files according to `require` forms and scoop the
  definitions from the other files.
- [ ] Add some sort of type inference (e.g. return type of function
  from last/return forms)
- [ ] Allow generic types
- [ ] Make the analysis better
    - [ ] Track local variables from let-bindings
    - [ ] Add undeclared variable warning (e.g. `setq` on not-declared
      variable or just using it)
    - [ ] Add unused variable warnings
    - [ ] Analyze all calls not just top-level
    - [ ] Parse `defcustom` types
