;;; init-lsp.el --- LSP configuration with lsp-bridge -*- lexical-binding: t -*-
;;; Commentary:

;; lsp-bridge for LSP support with embedded Node.js and Python.
;; Node.js v22 LTS at ~/.emacs.d/nodejs/ (LSP servers installed there).
;; Python venv at ~/.emacs.d/.venv/ (lsp-bridge async backend).
;; Uses Volar + vtsls multiserver for Vue 3 + TypeScript.
;;
;; 首次使用前请运行: bash setup-lsp.sh

;;; Code:

;; ============================================================================
;; 内置 Node.js: 将 nodejs/bin 加入 exec-path 和 PATH
;; ============================================================================

(let ((node-bin (expand-file-name "nodejs/bin" user-emacs-directory)))
  (when (file-directory-p node-bin)
    (add-to-list 'exec-path node-bin)
    (setenv "PATH" (concat node-bin ":" (getenv "PATH"))))
  (message "[init-lsp] Node.js 路径: %s (存在: %s)" node-bin (file-directory-p node-bin)))

;; ============================================================================
;; lsp-bridge 依赖
;; ============================================================================

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  (message "[init-lsp] >>> yasnippet :config 执行中 (yas-reload-all)...")
  (yas-reload-all)
  (message "[init-lsp] >>> yasnippet :config 完成"))
(message "[init-lsp] >>> yasnippet use-package 完成")

(use-package markdown-mode
  :ensure t
  :defer t)

;; ============================================================================
;; lsp-bridge (via git clone at ~/.emacs.d/lsp-bridge/)
;; ============================================================================

(message "[init-lsp] >>> 加载 lsp-bridge... (目录存在: %s)"
         (file-directory-p (expand-file-name "lsp-bridge" user-emacs-directory)))
(use-package lsp-bridge
  :load-path "lsp-bridge"
  :hook
  ((web-mode . lsp-bridge-mode)
   (typescript-mode . lsp-bridge-mode)
   (js-mode . lsp-bridge-mode))
  :bind
  (("M-." . lsp-bridge-find-def)
   ("M-," . lsp-bridge-find-def-return)
   ("M-?" . lsp-bridge-find-references)
   :map lsp-bridge-mode-map
   ("C-j l r" . lsp-bridge-rename)
   ("C-j l a" . lsp-bridge-code-action)
   ("C-j l d" . lsp-bridge-show-documentation)
   ("C-j l f" . lsp-bridge-find-def)
   ("C-j l i" . lsp-bridge-find-impl)
   ("C-j l e" . lsp-bridge-diagnostic-list))
  :init
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements "C-j l" "lsp"))
  :config
  ;; Python: 使用内置 venv
  (setq lsp-bridge-python-command
        (expand-file-name ".venv/bin/python" user-emacs-directory))
  ;; 用户自定义 langserver 目录（vtsls.json 路径覆盖）
  (setq lsp-bridge-user-langserver-dir
        (expand-file-name "lsp-bridge-langserver" user-emacs-directory))
  ;; Vue 文件: 使用 Volar + vtsls 多服务器融合
  (setq lsp-bridge-multi-lang-server-extension-list
        '((("vue") . "volar_vtsls")))
  ;; 启用诊断提示
  (setq lsp-bridge-enable-diagnostics t))

(provide 'init-lsp)
;;; init-lsp.el ends here
