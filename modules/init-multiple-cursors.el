;;; init-multiple-cursors.el --- 多光标编辑 -*- lexical-binding: t -*-
;;; Commentary:

;; 使用 multiple-cursors 实现多光标同时编辑。
;;
;; 快捷键（C-j m 前缀）：
;;   C-j m n   — 标记下一个相同选区
;;   C-j m p   — 标记上一个相同选区
;;   C-j m a   — 标记所有相同选区
;;   C-j m l   — 每行一个光标
;;   C-j m s   — 正则搜索标记
;;   C-j m N   — 跳过下一个
;;   C-j m P   — 跳过上一个
;;   C-j m 0   — 插入数字序列
;;   C-j m |   — 垂直对齐
;;   C-S-click — 在点击处切换光标

;;; Code:

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind
  (("C-j m n" . mc/mark-next-like-this)
   ("C-j m p" . mc/mark-previous-like-this)
   ("C-j m a" . mc/mark-all-like-this)
   ("C-j m l" . mc/edit-lines)
   ("C-j m s" . mc/mark-all-in-region-regexp)
   ("C-j m N" . mc/skip-to-next-like-this)
   ("C-j m P" . mc/skip-to-previous-like-this)
   ("C-j m 0" . mc/insert-numbers)
   ("C-j m |" . mc/vertical-align)
   ("C-S-<mouse-1>" . mc/toggle-cursor-on-click))
  :init
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-j m"   "Multiple Cursors"
      "C-j m n" "Mark next"
      "C-j m p" "Mark prev"
      "C-j m a" "Mark all"
      "C-j m l" "Edit lines"
      "C-j m s" "Search regexp"
      "C-j m N" "Skip next"
      "C-j m P" "Skip prev"
      "C-j m 0" "Insert numbers"
      "C-j m |" "Align")))

(provide 'init-multiple-cursors)
;;; init-multiple-cursors.el ends here
