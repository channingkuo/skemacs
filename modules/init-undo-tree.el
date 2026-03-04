;;; init-undo-tree.el --- 撤销/重做树 -*- lexical-binding: t -*-
;;; Commentary:

;; 使用 undo-tree 替代 Emacs 默认的线性撤销机制，提供树形撤销历史。
;; 通过 hydra 面板实现快速撤销/重做操作（C-j u）。
;; - p: 撤销一次后关闭 hydra
;; - P: 连续撤销（保持 hydra 打开）
;; - n: 重做一次后关闭 hydra
;; - N: 连续重做（保持 hydra 打开）
;; - v: 可视化撤销树

;;; Code:

;; ============================================================================
;; undo-tree：树形撤销/重做
;; ============================================================================

(use-package undo-tree
  :ensure t
  :after hydra
  :init (global-undo-tree-mode)
  :bind
  (("C-j u" . hydra-undo-tree/body))
  :init
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-j u" "Undo Tree"))
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (defhydra hydra-undo-tree (:hint nil)
    "
  _p_: Undo once  _P_: Undo  _n_: Redo once  _N_: Redo   "
    ("p"   undo-tree-undo nil :color blue)
    ("P"   undo-tree-undo)
    ("n"   undo-tree-redo nil :color blue)
    ("N"   undo-tree-redo)
    ("v"   undo-tree-visualize "Visualize" :color blue)
    ("q"   nil "Quit hydra" :color blue)))

(provide 'init-undo-tree)
;;; init-undo-tree.el ends here
