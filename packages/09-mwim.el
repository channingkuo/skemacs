;;; 09-mwim.el --- Smart beginning and end of line movement -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration replaces the default C-a and C-e behavior with
;; smarter movement commands:
;; - C-a: first press moves to beginning of code, second press to line start
;; - C-e: first press moves to end of code (before comments), second to line end

;;; Code:

(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;;; 09-mwim.el ends here
