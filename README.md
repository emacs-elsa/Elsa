# <img align="right" src="https://raw.githubusercontent.com/nashamri/elsa-logo/master/elsa-logo-transparent.png" width="133" height="100"> Elsa - Emacs Lisp Static Analyser [![Build Status](https://travis-ci.org/emacs-elsa/Elsa.svg?branch=master)](https://travis-ci.org/emacs-elsa/Elsa)

<p align="center">(Your favourite princess now in Emacs!)</p>

[![Coverage Status](https://coveralls.io/repos/github/Fuco1/Elsa/badge.svg?branch=master)](https://coveralls.io/github/Fuco1/Elsa?branch=master) [![Paypal logo](https://img.shields.io/badge/PayPal-Donate-orange.svg?logo=paypal)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=A5PMGVKCQBT88) [![Patreon](https://img.shields.io/badge/Patreon-Become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/user?u=3282358)

Elsa is a tool that analyses your code without loading or running it.
It can track types and provide helpful hints when things don't match
up before you even try to run the code.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [State of the project](#state-of-the-project)
- [Non-exhaustive list of features](#non-exhaustive-list-of-features)
    - [Detect dead code](#detect-dead-code)
    - [Enforce style rules](#enforce-style-rules)
    - [Look for suspicious code](#look-for-suspicious-code)
    - [Track types of expressions](#track-types-of-expressions)
- [How do I run it](#how-do-i-run-it)
    - [Flycheck integration](#flycheck-integration)
- [Configuration](#configuration)
    - [Analysis extension](#analysis-extension)
    - [Rulesets](#rulesets)
- [Type annotations](#type-annotations)
- [How can I contribute to this project](#how-can-i-contribute-to-this-project)
- [F.A.Q.](#faq)
    - [What's up with the logo?](#whats-up-with-the-logo)
- [For developers](#for-developers)
    - [How to write an extension for your-favourite-package](#how-to-write-an-extension-for-your-favourite-package)
    - [How to write a ruleset](#how-to-write-a-ruleset)

<!-- markdown-toc end -->

# State of the project

We are currently in a very early *ALPHA* phase.  API is somewhat
stable but the type system and annotations are under constant
development.  Things might break at any point.

# Non-exhaustive list of features

Here comes a non-exhaustive list of some more interesting features.

The error highlightings in the screenshots are provided by [Elsa
Flycheck extension](https://github.com/emacs-elsa/flycheck-elsa).

Everything you see here actually works, this is not just for show!

## Detect dead code

### Detect suspicious branching logic

![](./images/dead-code-1.png)

![](./images/dead-code-2.png)

### Find unreachable code in short-circuiting forms

![](./images/unreachable-code-1.png)

## Enforce style rules

### Provide helpful tips for making code cleaner

![](./images/useless-code-1.png)

![](./images/useless-code-2.png)

### Add custom rules for your own project with rulesets

![](./images/custom-ruleset-1.png)

### Make formatting consistent

![](./images/formatting-1.png)

## Look for suspicious code

### Find references to free/unbound variables

![](./images/unbound-variable-1.png)

### Don't assign to free variables

![](./images/unbound-variable-2.png)

### Detect conditions which are always true or false

![](./images/always-nil-1.png)

![](./images/always-non-nil-1.png)

### Make sure functions are passed enough arguments

![](./images/number-of-args-1.png)

### Make sure functions are not passed too many arguments

![](./images/number-of-args-2.png)

## Track types of expressions

### Check types of arguments passed to functions for compatibility

![](./images/type-inference-1.png)

![](./images/type-inference-2.png)

![](./images/type-inference-3.png)

# How do I run it

Currently we only support running Elsa with [Cask](https://github.com/cask/cask).

### [RECOMMENDED] Using packaged version

The easiest and fastest way to install Elsa is through
[MELPA](http://melpa.org/#/) and [Cask](https://github.com/cask/cask).

1. Add `(depends-on "elsa")` to `Cask` file of your project.
2. Run `cask install`.
3. `cask exec elsa FILE-TO-ANALYSE [ANOTHER-FILE...]` to analyse the file.

### Using development version

To use the development version you can clone the repository and use
`cask link` feature to use the code from the clone.

1. `git clone https://github.com/emacs-elsa/Elsa.git` somewhere to your computer.
2. Add `(depends-on "elsa")` to `Cask` file of your project.
3. Run `cask link elsa <path-to-elsa-repo>`.
4. `cask exec elsa FILE-TO-ANALYSE [ANOTHER-FILE...]` to analyse the file.

## Flycheck integration

If you use [flycheck](https://github.com/flycheck/flycheck) you can
use the [flycheck-elsa](https://github.com/emacs-elsa/flycheck-elsa)
package which integrates Elsa with Flycheck.

# Configuration

By default Elsa core comes with very little built-in logic, only
understanding the elisp [special
forms](https://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Forms.html).

However, we ship a large number of extensions for popular packages
such as `eieio`, `cl`, `dash` or even `elsa` itself.

You can configure Elsa by adding an `Elsafile.el` to your project.
The `Elsafile.el` should be located next to the `Cask` file.

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

## Rulesets

After analysis of the forms is done we have all the type information
and the AST ready to be further processed by various checks and rules.

These can be (non-exhaustive list):

* Stylistic, such as checking that a variable uses `lisp-case` for
  naming instead of `snake_case`.
* Syntactic, such as checking we are not wrapping the else branch of
  `if` with a useless `progn`.
* Semantic, such as checking that the condition of `if` does not
  always evaluate to `non-nil` (in which case the `if` form is
  useless).

Elsa provides some built-in rulesets and more can also be used by loading extensions.

To register a ruleset, add the following form to `Elsafile.el`

``` emacs-lisp
(register-ruleset
 dead-code
 style
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

# F.A.Q.

## What's up with the logo?

See the [discussion](https://github.com/emacs-elsa/Elsa/issues/80).

# For developers

After calling `(require 'elsa-font-lock)` there is a function
`elsa-setup-font-lock` which can be called from `emacs-lisp-mode-hook`
to set up some additional font-locking for Elsa types.

## How to write an extension for your-favourite-package

## How to write a ruleset
