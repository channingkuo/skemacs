;;; init-mwim.el --- 智能行首/行尾跳转 -*- lexical-binding: t -*-
;;; Commentary:

;; 使用 mwim (Move Where I Mean) 增强 C-a / C-e 行为：
;; - C-a: 第一次按跳到代码开头（跳过缩进），再按一次跳到行首
;; - C-e: 第一次按跳到代码末尾（注释之前），再按一次跳到行尾

;;; Code:

;; ============================================================================
;; mwim：智能行首/行尾跳转
;; ============================================================================

(use-package mwim
  :ensure t
  :defer t
  :bind
  (("C-a" . mwim-beginning)
   ("C-e" . mwim-end)))

(provide 'init-mwim)
;;; init-mwim.el ends here
