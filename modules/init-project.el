;;; init-project.el --- 项目管理 (project.el) -*- lexical-binding: t -*-
;;; Commentary:

;; 配置 Emacs 内置 project.el 作为项目管理方案。
;; project.el 基于版本控制（Git 等）自动识别项目根目录，
;; 提供项目内文件查找、搜索、编译、buffer 切换等功能。
;;
;; 快捷键（挂载到 C-j p 前缀下）：
;;   C-j p p — 切换项目（project-switch-project）
;;   C-j p f — 项目内查找文件（project-find-file）
;;   C-j p g — 项目内正则搜索（project-find-regexp）
;;   C-j p d — 打开项目根目录 dired（project-dired）
;;   C-j p b — 切换项目内 buffer（project-switch-to-buffer）
;;   C-j p c — 项目编译（project-compile）
;;   C-j p e — 项目 eshell（project-eshell）
;;   C-j p k — 关闭项目所有 buffer（project-kill-buffers）
;;   C-j p ! — 在项目根目录执行 shell 命令（project-shell-command）
;;   C-j p & — 在项目根目录执行异步 shell 命令（project-async-shell-command）

;;; Code:

;; ============================================================================
;; project.el — 内置项目管理
;; ============================================================================

(use-package project
  :ensure nil                             ; Emacs 内置（28+）
  :bind
  (("C-j p p" . project-switch-project)          ; 切换项目
   ("C-j p f" . project-find-file)               ; 查找项目文件
   ("C-j p g" . project-find-regexp)             ; 项目内正则搜索
   ("C-j p d" . project-dired)                   ; 项目根目录 dired
   ("C-j p b" . project-switch-to-buffer)        ; 切换项目 buffer
   ("C-j p c" . project-compile)                 ; 项目编译
   ("C-j p e" . project-eshell)                  ; 项目 eshell
   ("C-j p k" . project-kill-buffers)            ; 关闭项目 buffer
   ("C-j p !" . project-shell-command)           ; 项目 shell 命令
   ("C-j p &" . project-async-shell-command))    ; 项目异步 shell 命令
  :init
  ;; which-key 描述
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-j p"   "Project"
      "C-j p p" "Switch project"
      "C-j p f" "Find file"
      "C-j p g" "Find regexp"
      "C-j p d" "Dired (root)"
      "C-j p b" "Switch buffer"
      "C-j p c" "Compile"
      "C-j p e" "Eshell"
      "C-j p k" "Kill buffers"
      "C-j p !" "Shell command"
      "C-j p &" "Async shell command"))
  :config
  ;; ── 项目切换时的默认操作菜单 ───────────────────────────
  ;; 切换项目后弹出操作选择（find-file、dired 等）
  (setq project-switch-commands
        '((project-find-file "Find file" ?f)
          (project-find-regexp "Find regexp" ?g)
          (project-dired "Dired" ?d)
          (project-eshell "Eshell" ?e)
          (project-switch-to-buffer "Buffer" ?b)
          (project-compile "Compile" ?c)))

  ;; ── 自定义项目发现策略 ──────────────────────────────────
  ;; 除了默认的 VC（Git 等）识别外，还可以通过标记文件识别项目
  ;; 例如包含 .project 或 Makefile 的目录也视为项目根
  (setq project-vc-extra-root-markers
        '(".project"                      ; 自定义项目标记文件
          "Makefile"                      ; Makefile 项目
          "package.json"                  ; Node.js 项目
          "Cargo.toml"                    ; Rust 项目
          "go.mod"                        ; Go 项目
          "pyproject.toml"               ; Python 项目
          "pom.xml"                       ; Java Maven 项目
          "build.gradle")))              ; Java Gradle 项目

(provide 'init-project)
;;; init-project.el ends here
