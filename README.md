# nickel-mode

[![MELPA](https://melpa.org/packages/nickel-mode-badge.svg)](https://melpa.org/#/nickel-mode)
[![Build Status](https://github.com/nickel-lang/nickel-mode/actions/workflows/check.yaml/badge.svg)](https://github.com/nickel-lang/nickel-mode/actions/workflows/check.yaml/badge.svg)

An emacs mode for the [Nickel](https://github.com/tweag/nickel) programming language.

## Installation
Install from [MELPA](https://melpa.org/#/nickel-mode)

## LSP Integration
First, ensure you have the Nickel language server, [nls](https://github.com/tweag/nickel/tree/master/lsp), installed.

### With [`lsp-mode`](https://github.com/emacs-lsp/lsp-mode)
Add the following to your `init.el`:

```emacs-lisp
(require 'nickel-mode)
(require 'lsp-mode)

(add-to-list 'lsp-language-id-configuration '(nickel-mode . "nickel"))
  (lsp-register-client (make-lsp-client
                           :new-connection (lsp-stdio-connection "nls")
                           :activation-fn (lsp-activate-on "nickel")
                           :server-id 'nls
                           :major-modes '(nickel-mode)
                           :initialization-options (lambda ()
						     ;; pass empty object to use default config
						     (list :eval (make-hash-table)))))
(add-hook 'nickel-mode-hook #'lsp-deferred)

```
