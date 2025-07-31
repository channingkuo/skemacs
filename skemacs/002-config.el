;;; 002-config.el --- Basic Emacs behavior and UI configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file contains basic Emacs configuration including UI settings,
;; behavior modifications, backup configuration, and platform-specific
;; adjustments. Sets up fundamental editing preferences and appearance.

;;; Code:

;; macOS specific params
(defconst *is-a-mac* (eq system-type 'darwin))
;; Emacs 禁用启动的欢迎界面
(setq inhibit-startup-message t)
;; 在关闭 Emacs 前询问是否确认关闭，防止误触
(setq confirm-kill-emacs #'yes-or-no-p)
;; 自动补全括号
(electric-pair-mode t)
;; 编程模式下，光标在括号上时高亮另一个括号
(add-hook 'prog-mode-hook #'show-paren-mode)
;; 在 Mode line 上显示列号
(column-number-mode t)
;; 当另一程序修改了文件时，让 Emacs 及时刷新 Buffer
(global-auto-revert-mode t)
;; (setq debug-on-error t)
(setq max-lisp-eval-depth 10000)
;; (setq max-specpdl-size 20000)
;; 选中文本后输入文本会替换文本（更符合我们习惯了的其它编辑器的逻辑）
(delete-selection-mode t)
;; 编程模式下，可以折叠代码块
(add-hook 'prog-mode-hook #'hs-minor-mode)
;; 行号设定
;; (global-display-line-numbers-mode -1)
;; 关闭 Tool bar
(tool-bar-mode -1)
;; 隐藏菜单栏
(menu-bar-mode -1)
;; 图形界面时关闭滚动条
(when (display-graphic-p) (toggle-scroll-bar -1))
;; 平滑滚动
(pixel-scroll-precision-mode t)
;; 静音铃声
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;;; 002-config.el ends here
