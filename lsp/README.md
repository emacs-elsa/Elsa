# Elsa LSP

Emacs lisp LSP, based on Elsa

This is a very alpha version and requires some knowledge of JavaScript
ecosystem to run.  In the future we will probably implement the server
in Emacs itself.

# Installation

To use this you need to have the [node.js](https://nodejs.org/en/)
platform available on your machine.  Starting with
[nvm](https://github.com/nvm-sh/nvm) is probably the easiest way to do
it.

Once you install nvm, run:

``` shell
npm i -g yarn
```

Next, in this repo, run `yarn` to install the javascript dependencies.
Then run:

``` shell
yarn compile
```

to compile the server.

You will need to register the lsp client with Emacs.  Make sure you
have [lsp-mode](https://github.com/emacs-lsp/lsp-mode) installed in
Emacs.  Paste this somewhere and evaluate.  You will need to change
the path to where the script actually is on your computer.

``` emacs-lisp
(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection
   (lambda () (cons "node"
                    (list
                     "/home/matus/.emacs.d/projects/Elsa/lsp/dist/server.js"
                     "--stdio"
                     ))))
  :major-modes '(emacs-lisp-mode)
  :priority 100
  :server-id 'elsa))
```

That's it.  If you have `lsp-mode` configured everything should just work.
