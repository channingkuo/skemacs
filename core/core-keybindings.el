;;; core-keybindings.el --- 前缀键定义与全局快捷键 -*- lexical-binding: t -*-
;;; Commentary:

;; 定义所有前缀键（C-j、C-x w、C-x b、C-j o），并绑定不依赖任何
;; 第三方包的全局快捷键。各功能模块的快捷键在各自的文件中定义，
;; 挂载到这里定义的前缀键下。

;;; Code:

;; ============================================================================
;; 前缀键定义
;; ============================================================================

;; C-j 主前缀（覆盖原来的 electric-newline-and-maybe-indent）
(define-prefix-command 'skemacs-prefix)
(global-set-key (kbd "C-j") 'skemacs-prefix)

;; C-j o — Org 相关
(define-prefix-command 'skemacs-org-prefix)
(global-set-key (kbd "C-j o") 'skemacs-org-prefix)

;; C-x w — 窗口相关
(define-prefix-command 'skemacs-window-prefix)
(global-set-key (kbd "C-x w") 'skemacs-window-prefix)

;; C-x b — Buffer 相关
(define-prefix-command 'skemacs-buffer-prefix)
(global-set-key (kbd "C-x b") 'skemacs-buffer-prefix)

;; C-j p — 项目相关
(define-prefix-command 'skemacs-project-prefix)
(global-set-key (kbd "C-j p") 'skemacs-project-prefix)

;; C-j s — 搜索相关（Consult 搜索命令）
(define-prefix-command 'skemacs-search-prefix)
(global-set-key (kbd "C-j s") 'skemacs-search-prefix)

;; C-j f — 文件树（Treemacs）
(define-prefix-command 'skemacs-filetree-prefix)
(global-set-key (kbd "C-j f") 'skemacs-filetree-prefix)

;; C-j g — Git 相关（Magit）
(define-prefix-command 'skemacs-git-prefix)
(global-set-key (kbd "C-j g") 'skemacs-git-prefix)

;; C-j j — 跳转相关（Avy）
(define-prefix-command 'skemacs-jump-prefix)
(global-set-key (kbd "C-j j") 'skemacs-jump-prefix)

;; C-j v — 终端相关（Vterm）
(define-prefix-command 'skemacs-terminal-prefix)
(global-set-key (kbd "C-j v") 'skemacs-terminal-prefix)

;; C-j a — AI Agent（agent-shell）
(define-prefix-command 'skemacs-agent-prefix)
(global-set-key (kbd "C-j a") 'skemacs-agent-prefix)

;; C-j c — Claude Code IDE
(define-prefix-command 'skemacs-claude-prefix)
(global-set-key (kbd "C-j c") 'skemacs-claude-prefix)

;; ============================================================================
;; 全局快捷键（不依赖任何第三方包）
;; ============================================================================

;; --- 导航 ---
;; 向下/向上移动 10 行
(global-set-key (kbd "M-n") 'skemacs/next-ten-lines)
(global-set-key (kbd "M-p") 'skemacs/previous-ten-lines)

;; --- 剪贴板 ---
;; 复制选中内容到系统剪贴板
(global-set-key (kbd "C-c C-y") 'skemacs/copy-to-clipboard)

;; --- 编辑 ---
;; 删去光标所在行
(global-set-key (kbd "C-j C-k") 'kill-whole-line)

;; --- 跳转 ---
;; 返回上一个位置（配合 LSP 跳转定义使用）
(global-set-key (kbd "C-o") 'pop-global-mark)

;; --- 补全 ---
;; 纯文本补全
(global-set-key (kbd "M-/") 'dabbrev-expand)

;; --- 窗口 ---
;; 方向键切换窗口
(global-set-key (kbd "C-x w h") 'windmove-left)
(global-set-key (kbd "C-x w j") 'windmove-down)
(global-set-key (kbd "C-x w k") 'windmove-up)
(global-set-key (kbd "C-x w l") 'windmove-right)
;; 分割窗口
(global-set-key (kbd "C-x w s") 'split-window-below)
(global-set-key (kbd "C-x w S") 'skemacs/split-window-horizontally)
(global-set-key (kbd "C-x w v") 'split-window-right)
(global-set-key (kbd "C-x w V") 'skemacs/split-window-vertically)

;; --- Buffer ---
;; Buffer 切换（基础版，模块加载后可能被 ivy 等覆盖）
(global-set-key (kbd "C-x b b") 'switch-to-buffer)
(global-set-key (kbd "C-x b n") 'next-buffer)
(global-set-key (kbd "C-x b p") 'previous-buffer)
(global-set-key (kbd "C-x b k") 'kill-buffer)
;; 解绑原来的 C-x k（已移到 C-x b k）
(global-unset-key (kbd "C-x k"))

;; --- Org ---
;; Org 相关快捷键（使用内置 org-mode 函数）
(global-set-key (kbd "C-j o o") 'skemacs/open-org-directory)
(global-set-key (kbd "C-j o a") 'org-agenda)
(global-set-key (kbd "C-j o c") 'org-capture)

;; --- 查看加载时间 ---
(global-set-key (kbd "C-j t") 'skemacs/show-load-times)

(provide 'core-keybindings)
;;; core-keybindings.el ends here
