;;; init-hydra.el --- Hydra 操作面板 -*- lexical-binding: t -*-
;;; Commentary:

;; 引入 hydra 包，提供可重复执行的操作面板。
;; 目前包含：窗口大小调整面板（C-x w r）。

;;; Code:

(use-package hydra
  :ensure t
  :defer t
  :commands (hydra-window-resize/body)
  :bind
  (("C-x w r" . hydra-window-resize/body))
  :init
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-x w r" "Resize Window"))
  :config
  (defhydra hydra-window-resize (:hint nil :color amaranth)
    "
 窗口大小调整
 ─────────────────────────────────
 _h_: ← 水平缩小   _l_: → 水平放大
 _j_: ↓ 垂直缩小   _k_: ↑ 垂直放大
 _q_: 退出
"
    ("h" skemacs/shrink-window-horizontally)
    ("l" skemacs/enlarge-window-horizontally)
    ("j" skemacs/shrink-window)
    ("k" skemacs/enlarge-window)
    ("q" nil :color blue)))

(provide 'init-hydra)
;;; init-hydra.el ends here
