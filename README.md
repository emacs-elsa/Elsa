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
  "Checker for PHPStan"
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

# How can I contribute to this project

Open an issue if you want to work on something (not necessarily listed
below in the roadmap) so we won't duplicate work.  Or just give us
feedback or helpful tips.

You can provide type definitions for built-in functions by extending
`elsa-typed-builtin.el`.  There is plenty to go.  Some of the types
necessary to express what we want might not exist or be supported yet,
open an issue so we can discuss how to model things.
