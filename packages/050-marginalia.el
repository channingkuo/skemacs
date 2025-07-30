;;; 03-marginalia.el --- Enhanced minibuffer annotations -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration enables Marginalia, which adds helpful annotations
;; to minibuffer completions.. It provides context-aware information
;; about commands, variables, files, and other completion candidates.

;;; Code:

;; 为 Emacs minibuffer 中的选项添加注解
(use-package marginalia
  :ensure t
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle)))

;;; 050-marginalia.el ends here
