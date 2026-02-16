;;; init-multiple-cursors.el --- 多光标编辑 -*- lexical-binding: t -*-
;;; Commentary:

;; 使用 multiple-cursors 实现多光标同时编辑，配合 hydra 提供可视化操作面板。
;;
;; 支持向上/向下标记、跳过、取消标记、全部标记、正则搜索标记、
;; 插入数字/字母序列、垂直对齐等操作。
;;
;; 快捷键：
;;   C-j m     — 打开多光标 Hydra 操作面板
;;   C-S-click — 在点击处切换光标

;;; Code:

(use-package multiple-cursors
  :ensure t
  :defer t
  :after hydra
  :commands (mc/edit-lines
             mc/mark-all-like-this
             mc/mark-next-like-this
             mc/mark-previous-like-this
             mc/skip-to-next-like-this
             mc/skip-to-previous-like-this
             mc/unmark-next-like-this
             mc/unmark-previous-like-this
             mc/vertical-align
             mc/mark-all-in-region-regexp
             mc/insert-numbers
             mc/insert-letters
             mc/add-cursor-on-click
             mc/toggle-cursor-on-click)
  :bind
  (("C-j m"          . hydra-multiple-cursors/body)
   ("C-S-<mouse-1>"  . mc/toggle-cursor-on-click))
  :init
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-j m" "Multiple Cursors"))
  :config
  (defhydra hydra-multiple-cursors (:hint nil :color amaranth)
    "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
 ─────────────────────────────────────────────────────────────────────
  [_p_]   Prev     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
  [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
  [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
  [_|_] Align with input CHAR       [Click] Cursor at point
"
    ("l"   mc/edit-lines :exit t)
    ("a"   mc/mark-all-like-this :exit t)
    ("n"   mc/mark-next-like-this)
    ("N"   mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p"   mc/mark-previous-like-this)
    ("P"   mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("|"   mc/vertical-align)
    ("s"   mc/mark-all-in-region-regexp :exit t)
    ("0"   mc/insert-numbers :exit t)
    ("A"   mc/insert-letters :exit t)
    ("<mouse-1>"      mc/add-cursor-on-click)
    ("<down-mouse-1>" ignore)
    ("<drag-mouse-1>" ignore)
    ("q"   nil :color blue)))

(provide 'init-multiple-cursors)
;;; init-multiple-cursors.el ends here
