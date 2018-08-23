#  Elsa - Emacs Lisp Static Analyser

[![Build Status](https://travis-ci.org/Fuco1/Elsa.svg?branch=master)](https://travis-ci.org/Fuco1/Elsa) [![Coverage Status](https://coveralls.io/repos/github/Fuco1/Elsa/badge.svg?branch=master)](https://coveralls.io/github/Fuco1/Elsa?branch=master)[![Paypal logo](https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=A5PMGVKCQBT88)
[![Patreon](https://c5.patreon.com/external/logo/logomarkOrange.svg)](https://www.patreon.com/user?u=3282358&ty=h)

(Your favourite princess now in Emacs!)

Elsa is a tool that analyses your code without loading or running it.
It can track types and provide helpful hints when things don't match
up before you even try to run the code.

# State of the project

We are currently in a very early *ALPHA* phase.  API is somewhat
stable but the type system and annotations are under constant
development.  Things might break at any point.

# How do I run it

Currently we only support running Elsa with Cask.

1. `git clone https://github.com/Fuco1/Elsa.git` somewhere to your computer.
2. Add `(depends-on "elsa")` to `Cask` file of your project
3. Run `cask link elsa <path-to-elsa-repo>`
4. `cask exec elsa <file-to-analyse>` to analyse the file.  Currently
   only one file at a time can be analysed.

## Flycheck integration

If you use [flycheck](https://github.com/flycheck/flycheck) you can use the [flycheck-elsa](https://github.com/emacs-elsa/flycheck-elsa) package which integrates Elsa with Flycheck.

# Configuration

By default Elsa core comes with very little built-in logic, only
understanding the elisp [special
forms](https://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Forms.html).

There are multiple ways to extend the capabilities of Elsa.

## Analysis extension

One is by providing special analysis rules for more forms and
functions where we can exploit the knowledge of how the function
behaves to narrow the analysis down more.

For example, we can say that if the input of `not` is `t`, the return
value is always `nil`.  This encodes our domain knowledge in form of
an analysis rule.

All the rules are added in form of extensions.  Elsa has few core
extensions for most common built-in functions such as list
manupulation (`car`, `nth`...), predicates (`stringp`, `atomp`...),
logical functions (`not`, ...) and so on.  These are automatically
loaded because the functions are so common virtually every project is
going to use them.

Additional extensions are provided for popular external packages such
as [dash.el](https://github.com/magnars/dash.el).  To use them, add to
your `Elsafile.el` the `register-extensions` form, like so

``` emacs-lisp
(register-extensions
 dash
 ;; more extensions here
 )
```

The `Elsafile.el` should be located next to the `Cask` file of your project.

## Rulesets

After analysis of the forms is done we have all the type information
and the AST ready to be further processed by various checks and rules.

These can be:

* Stylistic, such as checking that a variable uses lisp-case for
  naming instead of snake_case.
* Syntactic, such as checking we are not wrapping the else branch of
  `if` with a useless `progn`.
* Semantic, such as checking that the condition of `if` does not
  always evaluate to `non-nil` (in which case the `if` form is
  useless).

Elsa provides some built-in rulesets and more can also be used by loading extensions.

To register a ruleset, add the following form to `Elsafile.el`

``` emacs-lisp
(register-ruleset
 if
 symbol
 ;; more rulesets here
 )
```

# Type annotations

In Elisp users are not required to provide type annotations to their
code.  While at many places the types can be inferred there are
places, especially in user-defined functions, where we can not guess
the correct type (we can only infer what we see during runtime).

Users can annotate their `defun` definitions like this:

``` emacs-lisp
;; (elsa-pluralize :: String -> Int -> String)
(defun elsa-pluralize (word n)
  "Return singular or plural of WORD based on N."
  (if (= n 1)
      word
    (concat word "s")))
```

The `(elsa-pluralise :: ...)` inside a comment form provides
additional information to the Elsa analysis.  Here we say that the
function following such a comment takes two arguments, string and int,
and returns a string.

The syntax of the type annotation is somewhat modeled after Haskell
but there are some special constructs available to Elsa

Here are general guidelines on how the types are constructed.

- For built-in types with test predicates, drop the `p` or `-p` suffix and PascalCase to get the type:
    - `stringp` → `String`
    - `integerp` → `Integer` (`Int` is also accepted)
    - `markerp` → `Marker`
    - `hash-table-p` → `HashTable`
- A type for everything is called `Mixed`.  It accepts anything and is
  always nullable.  This is the default type for when we lack type
  information.
- Sum types can be specified with `|` syntax, so `String | Integer` is
  a type accepting both strings or integers.
- Cons types are specified by prefixing wrapping the `car` and `cdr`
  types with a `Cons` constructor, so `Cons Int Int` is a type where
  the `car` is an int and `cdr` is also an int, for example `(1 . 3)`.
- List types are specified by wrapping a type in a vector `[]`
  constructor, so `[Int]` is a list of integers and `[String | Int]`
  is a list of items where each item is either a string or an integer.
  A type constructor `List` is also supported.
- Function types are created by separating argument types and the
  return type with `->` token.
- To make variadic types (for the `&rest` keyword) add three dots
  `...` after the type, so `String... -> String` is a function taking
  any number of strings and returning a string, such as `concat`.
  Note: a variadic type is internally just a list of the same base
  type but it has a flag that allows the function be of variable
  arity.  A `Variadic` type constructor is also available to construct
  complex types.
- To mark type as nullable you can attach `?` to the end of it, so
  that `Int?` accepts any integer and also a `nil`.  A `Maybe` type
  constructor is also available to construct complex types.

Some type constructors have optional arguments, for example writing
just `Cons` will assume the `car` and `cdr` are of type `Mixed`.

# How can I contribute to this project

Open an issue if you want to work on something (not necessarily listed
below in the roadmap) so we won't duplicate work.  Or just give us
feedback or helpful tips.

You can provide type definitions for built-in functions by extending
`elsa-typed-builtin.el`.  There is plenty to go.  Some of the types
necessary to express what we want might not exist or be supported yet,
open an issue so we can discuss how to model things.

# For developers

After calling `(require 'elsa-font-lock)` there is a function
`elsa-setup-font-lock` which can be called from `emacs-lisp-mode-hook`
to set up some additional font-locking for Elsa types.

## How to write an extension for your-favourite-package

## How to write a ruleset
