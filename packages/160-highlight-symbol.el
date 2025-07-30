;;; 51-highlight-symbol.el --- Symbol highlighting and navigation -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration enables highlight-symbol for highlighting all
;; occurrences of the symbol at point throughout the buffer.. Useful
;; for quickly identifying variable usage and code structure.
;; Bound to F3 for quick access.
;; highlight-symbol: https://github.com/nschum/highlight-symbol.el

;;; Code:

(use-package highlight-symbol
  :ensure t
  :init (highlight-symbol-mode))

;;; 160-highlight-symbol.el ends here
