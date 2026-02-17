;;; init-agent-shell.el --- AI Agent 集成 (agent-shell + ACP) -*- lexical-binding: t -*-
;;; Commentary:

;; 使用 agent-shell 通过 ACP (Agent Client Protocol) 与 AI 编码代理交互。
;; 支持 Claude Code、Cursor Agent、Gemini CLI 三种代理。
;;
;; ACP 适配器通过 setup-agents.sh 安装到项目内置的 nodejs/ 目录中，
;; 使用 agent-shell 的命令配置变量直接指定完整路径，不修改全局 exec-path / $PATH。
;;
;; 快捷键（挂载到 C-j a 前缀下）：
;;   C-j a a — 启动/复用任意 Agent
;;   C-j a c — 启动 Claude Code
;;   C-j a u — 启动 Cursor Agent
;;   C-j a g — 启动 Gemini CLI
;;   C-j a n — 新建 Agent Shell
;;   C-j a i — 插入文件作为上下文

;;; Code:

;; ============================================================================
;; 路径常量
;; ============================================================================

(defvar skemacs--node-bin
  (expand-file-name "nodejs/bin" user-emacs-directory)
  "项目内置 Node.js 的 bin 目录路径。")

(defvar skemacs--agent-path
  (concat skemacs--node-bin ":" (getenv "PATH"))
  "ACP 代理子进程使用的 PATH 环境变量。
将项目内 nodejs/bin/ 置于最前，确保 shebang 找到正确的 node。")

;; ============================================================================
;; agent-shell — ACP 代理 Shell
;; ============================================================================

(use-package agent-shell
  :ensure t
  :defer t
  :bind
  (("C-j a a" . agent-shell)                            ; 启动/复用任意 Agent
   ("C-j a c" . agent-shell-anthropic-start-claude-code) ; Claude Code
   ("C-j a u" . agent-shell-cursor-start-agent)          ; Cursor Agent
   ("C-j a g" . agent-shell-google-start-gemini)         ; Gemini CLI
   ("C-j a n" . agent-shell-new-shell)                   ; 新建 Agent Shell
   ("C-j a i" . agent-shell-insert-file))                ; 插入文件作为上下文
  :init
  ;; which-key 描述
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-j a"   "AI Agent"
      "C-j a a" "Agent shell"
      "C-j a c" "Claude Code"
      "C-j a u" "Cursor Agent"
      "C-j a g" "Gemini CLI"
      "C-j a n" "New shell"
      "C-j a i" "Insert file"))
  :config
  ;; ── 命令路径：直接指向项目内 nodejs/bin/ 下的二进制 ──────────
  (setq agent-shell-anthropic-claude-command
        (list (expand-file-name "claude-code-acp" skemacs--node-bin)))

  (setq agent-shell-cursor-command
        (list (expand-file-name "cursor-agent-acp" skemacs--node-bin)))

  (setq agent-shell-google-gemini-command
        (list (expand-file-name "gemini" skemacs--node-bin)
              "--experimental-acp"))

  ;; ── 子进程环境变量：独立 PATH 指向项目内 node ──────────────
  ;; 确保 ACP 适配器脚本的 #!/usr/bin/env node 找到项目内的 node
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables
         "PATH" skemacs--agent-path))

  (setq agent-shell-cursor-environment
        (agent-shell-make-environment-variables
         "PATH" skemacs--agent-path))

  (setq agent-shell-google-gemini-environment
        (agent-shell-make-environment-variables
         "PATH" skemacs--agent-path))

  ;; ── 认证配置 ──────────────────────────────────────────────
  ;; Claude Code: 登录式 OAuth（首次使用时打开浏览器）
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))

  ;; Gemini CLI: 登录式 OAuth
  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :login t)))

(provide 'init-agent-shell)
;;; init-agent-shell.el ends here
