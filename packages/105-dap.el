;; https://emacs-lsp.github.io/dap-mode/page/configuration/
;; https://zhuanlan.zhihu.com/p/467681146
;; 把光标移入它生成的模板的括号内，按下 C-M-x使得模版生效
(use-package dap-mode
  :ensure t
  :after hydra lsp-mode
  :defer t
  :config
  (dap-ui-mode 1))
