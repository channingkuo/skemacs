;;; 30-company.el --- Auto-completion with Company and YASnippet -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration sets up the Company completion framework with
;; yasnippet integration for comprehensive auto-completion. Includes
;; optimized settings for responsive completion and snippet expansion
;; in all programming modes.
;; company: https://github.com/company-mode/company-mode

;;; Code:

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  ;; 禁用 snippet 展开后的钩子中可能触发的格式化
  (setq yas-after-exit-snippet-hook nil)
  (setq yas-before-expand-snippet-hook nil)
  (setq yas-keymap-disable-hook nil)
  (setq yas-buffer-local-condition 'always))
;; TODO 设置自定义 snippets
;; (setq yas-snippet-dirs '((expand-file-name "snippets" user-emacs-directory))))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  ;; 只需敲1个字母就补全
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
  (setq company-show-numbers t)
  (setq company-selection-wrap-around t)
  ;; 根据选择的频率进行排序
  (setq company-transformers '(company-sort-by-occurrence)))

;;; 30-company.el ends here
