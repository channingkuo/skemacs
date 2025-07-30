;;; 05-amx.el --- Enhanced M-x with usage-based sorting -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration enables AMX, which enhances M-x by tracking
;; command usage frequency and displaying the most frequently used
;; commands first.. This makes command discovery and execution more
;; efficient over time.

;;; Code:

;; 记录我们每次调用 M-x 时输入的命令历史，然后每次将最常用的显示在前面
(use-package amx
  :ensure t
  :init (amx-mode))

;;; 080-amx.el ends here
