;;; init-rainbow-delimiters.el --- 彩虹括号 -*- lexical-binding: t -*-
;;; Commentary:

;; 使用 rainbow-delimiters 为嵌套括号/方括号/花括号着色，
;; 通过颜色区分括号层级，提升代码可读性。
;;
;; 特别适合 Lisp 系语言（Emacs Lisp、Clojure、Scheme 等），
;; 也对 C/Java/Python 等语言的嵌套结构有帮助。
;;
;; 功能：
;;   - 每层嵌套的括号使用不同颜色
;;   - 未匹配的括号以醒目颜色警告
;;   - 仅在编程模式下启用，不影响文本编辑

;;; Code:

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-rainbow-delimiters)
;;; init-rainbow-delimiters.el ends here
