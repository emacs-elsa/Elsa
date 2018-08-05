#  Elsa - Emacs Lisp Static Analyser [![Build Status](https://travis-ci.org/Fuco1/Elsa.svg?branch=master)](https://travis-ci.org/Fuco1/Elsa) [![Coverage Status](https://coveralls.io/repos/github/Fuco1/Elsa/badge.svg?branch=master)](https://coveralls.io/github/Fuco1/Elsa?branch=master)

(Your favourite princess now in Emacs!)

Elsa is a tool that analyses your code without loading or running it.
It can track types and provide helpful hints when things don't match
up before you even try to run the code.

# How do I run it

Currently we only support running Elsa with Cask.

1. `git clone https://github.com/Fuco1/Elsa.git` somewhere to your computer.
2. Add `(depends-on "elsa")` to `Cask` file of your project
3. Run `cask link elsa <path-to-elsa-repo>`
4. `cask exec elsa <file-to-analyse>` to analyse the file.  Currently
   only one file at a time can be analysed.

If you use [flycheck](https://github.com/flycheck/flycheck) you can use the following checker

``` emacs-lisp
(flycheck-define-checker emacs-lisp-elsa
  "Checker for Elsa."
  :command ("<path-to-cask-binary>" ;; usuall something like "/home/matus/.cask/bin/cask"
            "exec"
            "elsa"
            source)
  :error-filter flycheck-increment-error-columns
  :error-patterns
  ((error line-start line ":" column ":error:" (message))
   (warning line-start line ":" column ":warning:" (message))
   (info line-start line ":" column ":notice:" (message)))
  :modes (emacs-mode-lisp))

(add-to-list 'flycheck-checkers 'emacs-lisp-elsa)
```

Then in the buffer (must be inside a `cask` powered project) you might
need to enable the checker with `C-u C-c ! x`.

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

# How can I contribute to this project

Open an issue if you want to work on something (not necessarily listed
below in the roadmap) so we won't duplicate work.  Or just give us
feedback or helpful tips.

You can provide type definitions for built-in functions by extending
`elsa-typed-builtin.el`.  There is plenty to go.  Some of the types
necessary to express what we want might not exist or be supported yet,
open an issue so we can discuss how to model things.

# For developers

## How to write an extension for <package>

## How to write a ruleset
