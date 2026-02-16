;;; init-completion.el --- Minibuffer 补全框架 (Vertico 全家桶) -*- lexical-binding: t -*-
;;; Commentary:

;; 使用 Vertico + Orderless + Marginalia + Consult + Embark 构建现代化的
;; minibuffer 补全体验，替代默认的 completing-read 界面。
;;
;; 包含组件：
;;   Vertico    — 垂直补全 UI，替换默认 minibuffer 补全
;;   Orderless  — 灵活的模糊匹配（空格分隔多关键词）
;;   Marginalia — 候选项旁显示注解信息（类型、文档、快捷键等）
;;   Consult    — 增强版搜索、跳转、buffer 切换等命令
;;   Embark     — 对候选项执行上下文操作（类似右键菜单）
;;
;; 快捷键：
;;   C-j s s — 当前 buffer 内搜索（consult-line）
;;   C-j s r — 项目内 ripgrep 搜索（consult-ripgrep）
;;   C-j s f — 查找文件（consult-find）
;;   C-j s g — grep 搜索（consult-grep）
;;   C-j s o — 大纲跳转（consult-outline）
;;   C-j s i — imenu 符号跳转（consult-imenu）
;;   C-.     — 候选项上下文操作（embark-act）
;;   C-;     — 候选项默认操作（embark-dwim）

;;; Code:

;; ============================================================================
;; savehist — 持久化 minibuffer 历史记录
;; ============================================================================

(use-package savehist
  :ensure nil                             ; Emacs 内置
  :init
  (savehist-mode 1))

;; ============================================================================
;; Vertico — 垂直补全 UI
;; ============================================================================

(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
  :config
  ;; 最多显示 15 个候选项
  (setq vertico-count 15)
  ;; 列表首尾循环滚动
  (setq vertico-cycle t)
  ;; 不自动调整 minibuffer 高度
  (setq vertico-resize nil))

;; vertico-directory — 目录导航增强
;; Backspace 删除整个目录路径分量，~ 跳转 HOME
(use-package vertico-directory
  :ensure nil                             ; vertico 自带扩展
  :after vertico
  :bind (:map vertico-map
         ("RET"   . vertico-directory-enter)
         ("DEL"   . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; ============================================================================
;; Orderless — 灵活匹配风格
;; ============================================================================

(use-package orderless
  :ensure t
  :config
  ;; 主要使用 orderless，basic 作为回退（某些补全表需要 basic）
  (setq completion-styles '(orderless basic))
  ;; 清除默认的 category-specific 覆盖
  (setq completion-category-defaults nil)
  ;; 文件路径使用 partial-completion 支持通配符
  (setq completion-category-overrides
        '((file (styles partial-completion)))))

;; ============================================================================
;; Marginalia — 候选项注解
;; ============================================================================

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1)
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle)))

;; ============================================================================
;; Consult — 增强版搜索与导航命令
;; ============================================================================

(use-package consult
  :ensure t
  :bind
  (;; --- C-x b 前缀下（覆盖基础 buffer 命令）---
   ("C-x b b" . consult-buffer)          ; 增强版 switch-to-buffer（含 recentf、bookmark）

   ;; --- M-g 前缀（goto-map）---
   ("M-g g"   . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o"   . consult-outline)
   ("M-g i"   . consult-imenu)
   ("M-g I"   . consult-imenu-multi)

   ;; --- M-s 前缀（search-map，Emacs 标准搜索前缀）---
   ("M-s l"   . consult-line)
   ("M-s r"   . consult-ripgrep)
   ("M-s f"   . consult-find)
   ("M-s g"   . consult-grep)

   ;; --- C-j s 前缀（Skemacs 搜索前缀，方便 which-key 发现）---
   ("C-j s s" . consult-line)            ; 当前 buffer 搜索
   ("C-j s r" . consult-ripgrep)         ; 项目内 ripgrep
   ("C-j s f" . consult-find)            ; 查找文件
   ("C-j s g" . consult-grep)            ; grep 搜索
   ("C-j s o" . consult-outline)         ; 大纲跳转
   ("C-j s i" . consult-imenu)           ; imenu 符号跳转
   ("C-j s I" . consult-imenu-multi)     ; 跨 buffer 符号跳转
   ("C-j s b" . consult-bookmark)        ; 书签

   ;; --- isearch 集成 ---
   :map isearch-mode-map
   ("M-s l"   . consult-line)            ; isearch → consult-line
   ("M-s r"   . consult-isearch-history))

  :init
  ;; which-key 描述
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-j s"   "Search"            ;; 搜索
      "C-j s s" "Line (buffer)"     ;; 按行搜索（当前缓冲区）
      "C-j s r" "Ripgrep (project)" ;; Ripgrep 搜索（项目）
      "C-j s f" "Find File"         ;; 查找文件
      "C-j s g" "Grep"              ;; Grep 搜索
      "C-j s o" "Outline"           ;; 大纲
      "C-j s i" "Imenu"             ;; Imenu 索引
      "C-j s I" "Imenu (multi)"     ;; Imenu（多缓冲区）
      "C-j s b" "Bookmark"))        ;; 书签

  ;; 优化 register 预览
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  :config
  ;; 按 M-. 预览候选项（不自动预览，避免干扰）
  (setq consult-preview-key "M-.")
  ;; 缩窄候选范围的前缀键
  (setq consult-narrow-key "<"))

;; ============================================================================
;; Embark — 候选项上下文操作
;; ============================================================================

(use-package embark
  :ensure t
  :bind
  (("C-."   . embark-act)                ; 对当前候选项弹出操作菜单
   ("C-;"   . embark-dwim)               ; 执行默认操作
   ("C-h B" . embark-bindings))          ; 查看所有 embark 绑定
  :init
  ;; 用 embark 替换默认的前缀键帮助
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; 隐藏 Embark live/completions 的 mode-line
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; embark-consult — Embark 与 Consult 的集成
(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-completion)
;;; init-completion.el ends here
