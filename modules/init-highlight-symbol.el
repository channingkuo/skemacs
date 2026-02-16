;;; init-highlight-symbol.el --- 符号高亮 -*- lexical-binding: t -*-
;;; Commentary:

;; 使用 highlight-symbol 在 buffer 中高亮当前光标所在符号的所有出现位置，
;; 方便快速定位变量/函数的引用。
;;
;; 功能：
;;   - 自动高亮光标所在符号（idle 延迟触发）
;;   - 在同一符号的多处出现间快速跳转
;;   - 支持一键重命名符号（在当前 buffer 内）
;;
;; 快捷键：
;;   C-j h n — 跳转到下一个同名符号
;;   C-j h p — 跳转到上一个同名符号
;;   C-j h h — 切换高亮当前符号
;;   C-j h r — 重命名当前符号（buffer 内）

;;; Code:

(use-package highlight-symbol
  :ensure t
  :defer t
  :hook (prog-mode . highlight-symbol-mode)
  :bind
  (("C-j h n"     . highlight-symbol-next)
   ("C-j h p"     . highlight-symbol-prev)
   ("C-j h h" . highlight-symbol-at-point)
   ("C-j h r" . highlight-symbol-query-replace))
  :init
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-j h"   "Highlight"
      "C-j h h" "Toggle symbol"
      "C-j h n" "Next symbol"
      "C-j h p" "Prev symbol"
      "C-j h r" "Rename symbol"))
  :config
  ;; 空闲 0.3 秒后自动高亮
  (setq highlight-symbol-idle-delay 0.3)
  ;; 高亮时不在 mode-line 显示出现次数（避免干扰）
  (setq highlight-symbol-on-navigation-p t))

(provide 'init-highlight-symbol)
;;; init-highlight-symbol.el ends here
