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
    "C-j"       "Skemacs"                   ;; Skemacs 主前缀
    "C-j o"     "Org"                       ;; Org 相关
    "C-j o o"   "Open Org Dir"              ;; 打开 Org 目录
    "C-j o a"   "Agenda"                    ;; 日程
    "C-j o c"   "Capture"                   ;; 快速捕获
    "C-j o l"   "Store Link"                ;; 存储链接
    "C-j o i"   "Insert Link"               ;; 插入链接
    "C-j o C-p" "iimage-mode"               ;; 内联图片模式
    "C-j C-k"   "Kill Line"                 ;; 删除行
    "C-j t"     "Load Times"                ;; 加载耗时
    "C-j n"     "Line numbers"              ;; 显示行号
    "C-j r"     "Relative Line numbers"     ;; 行号格式
    "C-x w"     "Window"                    ;; 窗口操作
    "C-x w h"   "Move Cursor Left"          ;; 光标左移
    "C-x w j"   "Move Cursor Down"          ;; 光标下移
    "C-x w k"   "Move Cursor Up"            ;; 光标上移
    "C-x w l"   "Move Cursor Right"         ;; 光标右移
    "C-x w s"   "Split Below"               ;; 水平分割
    "C-x w S"   "Split Below (focus)"       ;; 水平分割并聚焦
    "C-x w v"   "Split Right"               ;; 垂直分割
    "C-x w V"   "Split Right (focus)"       ;; 垂直分割并聚焦
    "C-x w r"   "Resize Window"             ;; 调整窗口大小
    "C-j s"     "Search"                    ;; 搜索
    "C-x b"     "Buffer"                    ;; 缓冲区操作
    "C-x b b"   "Switch Buffer"             ;; 切换缓冲区
    "C-x b n"   "Next Buffer"               ;; 下一个缓冲区
    "C-x b p"   "Previous Buffer"           ;; 上一个缓冲区
    "C-x b k"   "Kill Buffer"               ;; 关闭缓冲区
    "C-x b r"   "Recent Files"              ;; 最近文件
    "C-x b R"   "Recent Files (list)"       ;; 最近文件（列表）
    "C-x b l"   "Buffer List (ibuffer)"))   ;; 缓冲区列表

(provide 'init-which-key)
;;; init-which-key.el ends here
