;;; init-which-key.el --- Which-Key 按键提示 -*- lexical-binding: t -*-
;;; Commentary:

;; 按下前缀键后自动弹出可用后续键的提示面板，
;; 帮助记忆和发现快捷键。

;;; Code:

(use-package which-key
  :ensure nil                             ; Emacs 30+ 内置，无需从 MELPA 下载
  :diminish which-key-mode
  :init
  ;; 按下前缀键后 0.3 秒弹出提示（默认 1 秒太慢）
  (setq which-key-idle-delay 0.3)
  ;; 次级前缀的弹出延迟（按下一层后再等多久显示下一层）
  (setq which-key-idle-secondary-delay 0.05)
  ;; 提示面板从底部弹出
  (setq which-key-side-window-location 'bottom)
  ;; 面板最大高度占窗口的 25%
  (setq which-key-side-window-max-height 0.25)
  ;; 排序方式：按键序排列
  (setq which-key-sort-order 'which-key-key-order)
  ;; 分隔符样式
  (setq which-key-separator " → ")
  ;; 不在 echo area 显示前缀键（让 which-key 接管）
  (setq which-key-show-prefix 'left)

  :config
  (which-key-mode 1)

  ;; ── 为已定义的前缀键添加描述 ──────────────────────────────
  (which-key-add-key-based-replacements
    "C-j"       "Skemacs"
    "C-j o"     "Org"
    "C-j o o"   "Open Org Dir"
    "C-j o a"   "Agenda"
    "C-j o c"   "Capture"
    "C-j o l"   "Store Link"
    "C-j o i"   "Insert Link"
    "C-j o C-p" "iimage-mode"
    "C-j C-k"   "Kill Line"
    "C-j t"     "Load Times"
    "C-x w"     "Window"
    "C-x w h"   "Move Cursor Left"
    "C-x w j"   "Move Cursor Down"
    "C-x w k"   "Move Cursor Up"
    "C-x w l"   "Move Cursor Right"
    "C-x w s"   "Split Below"
    "C-x w S"   "Split Below (focus)"
    "C-x w v"   "Split Right"
    "C-x w V"   "Split Right (focus)"
    "C-j s"     "Search"
    "C-x b"     "Buffer"
    "C-x b b"   "Switch Buffer"
    "C-x b n"   "Next Buffer"
    "C-x b p"   "Previous Buffer"
    "C-x b k"   "Kill Buffer"
    "C-x b r"   "Recent Files"
    "C-x b R"   "Recent Files (list)"
    "C-x b l"   "Buffer List (ibuffer)"))

(provide 'init-which-key)
;;; init-which-key.el ends here
