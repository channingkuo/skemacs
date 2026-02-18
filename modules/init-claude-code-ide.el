;;; init-claude-code-ide.el --- Claude Code IDE 集成 (MCP) -*- lexical-binding: t -*-
;;; Commentary:

;; 通过 MCP (Model Context Protocol) 将 Claude Code CLI 与 Emacs 深度集成。
;; 提供双向桥接：Claude 可以访问 Emacs 的 LSP、xref、tree-sitter 等能力，
;; 同时在 vterm 终端中运行完整的 Claude Code 会话。
;;
;; 快捷键（挂载到 C-j c 前缀下）：
;;   C-j c c — 启动/切换 Claude Code
;;   C-j c m — 打开命令菜单 (transient)
;;   C-j c p — 发送 prompt
;;   C-j c r — 恢复上次会话
;;   C-j c k — 继续最近对话
;;   C-j c s — 列出所有会话
;;   C-j c t — 切换窗口可见性
;;   C-j c @ — 引用选中文本

;;; Code:

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :defer t
  :commands (claude-code-ide
             claude-code-ide-menu
             claude-code-ide-send-prompt
             claude-code-ide-resume
             claude-code-ide-continue
             claude-code-ide-stop
             claude-code-ide-list-sessions
             claude-code-ide-toggle
             claude-code-ide-toggle-recent
             claude-code-ide-insert-at-mentioned
             claude-code-ide-check-status)
  :bind
  (("C-j c c" . claude-code-ide)
   ("C-j c m" . claude-code-ide-menu)
   ("C-j c p" . claude-code-ide-send-prompt)
   ("C-j c r" . claude-code-ide-resume)
   ("C-j c k" . claude-code-ide-continue)
   ("C-j c s" . claude-code-ide-list-sessions)
   ("C-j c t" . claude-code-ide-toggle)
   ("C-j c T" . claude-code-ide-toggle-recent)
   ("C-j c @" . claude-code-ide-insert-at-mentioned)
   ("C-j c q" . claude-code-ide-stop))
  :init
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-j c"   "Claude Code"
      "C-j c c" "Start/switch"
      "C-j c m" "Menu"
      "C-j c p" "Send prompt"
      "C-j c r" "Resume session"
      "C-j c k" "Continue"
      "C-j c s" "List sessions"
      "C-j c t" "Toggle window"
      "C-j c T" "Toggle recent"
      "C-j c @" "Mention selection"
      "C-j c q" "Stop"))
  :config
  (setq claude-code-ide-terminal-backend 'vterm)
  (setq claude-code-ide-use-side-window t)
  (setq claude-code-ide-window-side 'right)
  (setq claude-code-ide-window-width 60)
  (setq claude-code-ide-focus-on-open t)
  (setq claude-code-ide-use-ide-diff t)

  (claude-code-ide-emacs-tools-setup))

(provide 'init-claude-code-ide)
;;; init-claude-code-ide.el ends here
