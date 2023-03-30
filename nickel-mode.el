;;; nickel-mode.el --- A major mode for editing Nickel source code -*-lexical-binding: t-*-

;; Copyright (c) Tweag I/O Limited.

;; Version: 0.1
;; Author: The Nickel Team (nickel-lang@tweag.io)
;; Url: https://github.com/nickel-lang/nickel-mode
;; Created: 7 March 2023
;; Keywords: languages configuration-language configuration nickel infrastructure
;; Homepage: https://nickel-lang.org/
;; Package-Requires: ((emacs "24.1"))

;; This file is distributed under the terms of MIT license.

;;; Commentary:

;; This package implements a major-mode for editing Nickel source code.


;;; Code:

(defgroup nickel nil
  "Major mode for editing Nickel source code."
  :group 'languages
  :link '(url-link :tag "Site" "https://nickel-lang.org/")
  :link '(url-link :tag "Repository" "https://github.com/nickel-lang/nickel-mode"))

(defconst nickel-mode-keywords
  (regexp-opt '("if" "then" "else" "forall" "in" "let"
                "rec" "match" "fun" "import" "merge"
                "default" "doc" "optional" "priority" "force" "not_exported")
              'symbols))

(defconst nickel-mode-constants
  (regexp-opt '("true" "false" "null")
              'symbols))

(defconst nickel-mode-types
  (regexp-opt '("Dyn" "Num" "Str" "Array")
              'symbols))

(defconst nickel-mode-primops
  (regexp-opt '("%typeof%" "%assume%" "%array_lazy_assume%" "%dictionary_assume%"
                "%blame%" "%chng_pol%" "%polarity%" "%go_dom%" "%go_codom%" "%go_field%"
                "%go_array%" "%go_dict%" "%seal%" "%unseal%" "%embed%" "%record_map%"
                "%record_insert%" "%record_remove%" "%record_empty_with_tail%" "%record_seal_tail%"
                "%record_unseal_tail%" "%seq%" "%deep_seq%" "%force%" "%head%" "%tail%" "%length%"
                "%fields%" "%values%" "%pow%" "%trace%" "%has_field%" "%map%" "%elem_at%" "%generate%"
                "%rec_force%" "%rec_default%")
              'symbols))


(defconst nickel-mode-identifiers "\\_<?[a-zA-Z][_a-zA-Z0-9-']*\\_>")
;; Same as identifiers, but starting with a `
;; TODO: Not sure if the ` will work yet...
(defconst nickel-mode-enum-tags "\\_<`_?[a-zA-Z][_a-zA-Z0-9-']*\\_>")
(defconst nickel-mode-numbers "\\_<[0-9]*\\.?[0-9]+\\_>")
(defconst nickel-mode-operators
  (regexp-opt
    '("->" "\\[" "]" "," "|" ":" "=" "==" "|>"
      "!=" ")" "&&" "||" "{" "}" "(" "?" ";" "$" "&" "."
      "\"" "+" "-" "*" "/" "%" "@" "!" ".." "=>" "_" "<" ">"
      "<=" ">=" "[|" "|]")))
;; TODO
(defconst nickel-mode-strings "")

(defconst nickel-mode-font-lock-keywords
  `(
    (,nickel-mode-keywords . font-lock-keyword-face)
    (,nickel-mode-types . font-lock-type-face)
    (,nickel-mode-constants . font-lock-constant-face)
    (,nickel-mode-primops . font-lock-builtin-face)
    (,nickel-mode-enum-tags . font-lock-constant-face)
    (,nickel-mode-identifiers . (1 font-lock-variable-name-face))
    (,nickel-mode-operators . font-lock-builtin-face)
    (,nickel-mode-numbers . font-lock-constant-face)))

;; Use Nickel mode for .ncl files
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ncl\\'" . nickel-mode))

(defvar nickel-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Handle comments
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st))


;;;###autoload
(define-derived-mode nickel-mode prog-mode
  "Nickel"
  "Major mode for editing Nickel source code."
  
  (setq font-lock-defaults '((nickel-mode-font-lock-keywords)))

  ;; (setq-local comment-start "# ")
  ;; (setq-local comment-end "")
  (set-syntax-table nickel-mode-syntax-table)
   
  :group 'nickel)

(provide 'nickel-mode)

;;; nickel-mode.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
