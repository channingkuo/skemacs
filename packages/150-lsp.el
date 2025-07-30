;;; 31-lsp.el --- Language Server Protocol configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration sets up LSP (Language Server Protocol) support
;; for intelligent code features like completion, navigation, and
;; diagnostics.. Optimized for performance with reasonable defaults
;; and Flycheck integration.

;;; Code:

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l" lsp-file-watch-threshold 2000)
  :hook (
         ;;  (typescript-mode . lsp-deferred)
         ;;  (js-mode . lsp-deferred)
         ;;  (web-mode . lsp-deferred)
         ;;  (vue-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; 调整超时和格式化设置
  (setq lsp-response-timeout 30)
  (setq lsp-enable-on-type-formatting nil)
  ;; (setq lsp-enable-file-watchers nil)      ; 禁用文件监视（防止递归）
  (setq lsp-enable-folding t)              ; 保持代码折叠
  (setq lsp-enable-links t)                ; 保持代码链接
  ;; (setq lsp-enable-snippet t)              ; 保持代码片段
  (setq lsp-diagnostic-package :flycheck)  ; 使用 flycheck 做错误检查
  ;; (setq lsp-eldoc-enable-hover nil)        ; 禁用悬停文档（减少递归风险）
  ;; (setq lsp-signature-render-documentation nil) ; 禁用签名文档
  ;; 性能优化
  (setq lsp-idle-delay 0.5
        lsp-completion-provider :company-capf)
  ;; 日志控制
  (setq lsp-log-io nil) ; 生产环境关闭，调试时可开启
  )

;;; 150-lsp.el ends here
