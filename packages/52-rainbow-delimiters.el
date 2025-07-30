;;; 52-rainbow-delimiters.el --- Colorful bracket and parentheses highlighting -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration enables rainbow-delimiters mode for programming
;; modes, which colors matching delimiters (parentheses, brackets, braces)
;; with different colors to make nested structures easier to read and
;; navigate.
;; rainbow-delimiters: https://github.com/Fanael/rainbow-delimiters

;;; Code:

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;; 52-rainbow-delimiters.el ends here
