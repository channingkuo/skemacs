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
      "C-x w r" "Resize Window"))   ;; 调整窗口大小
  :config
  (defhydra hydra-window-resize (:hint nil :color amaranth)
    "
  _h_: Shrink H  _l_: Enlarge H  _j_: Shrink V  _k_: Enlarge V   "
    ("h" skemacs/shrink-window-horizontally)
    ("l" skemacs/enlarge-window-horizontally)
    ("j" skemacs/shrink-window)
    ("k" skemacs/enlarge-window)
    ("q" nil "Quit" :color blue)))

(provide 'init-hydra)
;;; init-hydra.el ends here
