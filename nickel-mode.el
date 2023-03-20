;;; nickel-mode.el --- A major mode for editing Nickel source code -*-lexical-binding: t-*-

;; Copyright (c) Tweag Holding and its affiliates.

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

(define-derived-mode nickel-mode prog-mode
  "Nickel"
  "Major mode for editing Nickel source code."
  :group 'nickel)

;; Use Nickel mode for .ncl files
(add-to-list 'auto-mode-alist '("\\.ncl\\'" . nickel-mode))

(provide 'nickel-mode)

;;; nickel-mode.el ends here
