;;; 100-flycheck.el --- On-the-fly syntax checking -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration enables Flycheck for real-time syntax checking
;; and error highlighting in programming modes. Provides immediate
;; feedback on syntax errors, warnings, and style issues across
;; many programming languages.
;; flycheck: https://www.flycheck.org/en/latest/
;; flycheck languages: https://www.flycheck.org/en/latest/languages.html#flycheck-languages

;;; Code:

(use-package flycheck
  :ensure t
  :config
  ;; 如果单行信息很长会自动换行
  (setq truncate-lines nil)
  :hook
  (prog-mode . flycheck-mode))

;;; 100-flycheck.el ends here
  