;;; init-treemacs.el --- Treemacs 文件树侧边栏 -*- lexical-binding: t -*-
;;; Commentary:

;; 使用 Treemacs 提供项目文件树侧边栏，支持文件浏览、项目管理等功能。
;; 集成 magit（Git 状态显示）和 ace-window（窗口编号兼容）。
;;
;; 快捷键：
;;   C-j f t — 开关 Treemacs 侧边栏（treemacs）
;;   C-j f s — 在 Treemacs 中定位当前文件（treemacs-find-file）
;;   C-j f d — 在 Treemacs 中定位当前项目（treemacs-select-directory）
;;   C-j f a — 向 Treemacs 工作区添加项目（treemacs-add-project-to-workspace）

;;; Code:

;; ============================================================================
;; treemacs — 文件树侧边栏
;; ============================================================================

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (("C-j f t" . treemacs)                          ; 开关侧边栏
   ("C-j f s" . treemacs-find-file)                ; 定位当前文件
   ("C-j f d" . treemacs-select-directory)         ; 选择目录
   ("C-j f a" . treemacs-add-project-to-workspace)) ; 添加项目
  :init
  ;; which-key 描述
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-j f"   "File tree"
      "C-j f t" "Toggle Treemacs"
      "C-j f s" "Find current file"
      "C-j f d" "Select directory"
      "C-j f a" "Add project"))
  :config
  ;; ── 基础配置 ──────────────────────────────────────────────
  (setq treemacs-width 30                          ; 侧边栏宽度
        treemacs-width-is-initially-locked nil     ; Allow resizing from start
        treemacs-is-never-other-window nil         ; C-x o 不跳过 treemacs 窗口
        treemacs-display-in-side-window nil        ; Use normal window instead of side window
        treemacs-show-hidden-files t               ; 显示隐藏文件
        treemacs-follow-after-init t               ; 初始化后自动定位当前文件
        treemacs-sorting 'alphabetic-asc           ; 按文件名升序排列
        treemacs-no-png-images nil                 ; 使用图标（终端下自动降级为文本）
        treemacs-recenter-after-file-follow nil    ; 定位文件后不居中
        treemacs-silent-refresh t                  ; 静默刷新（不显示消息）
        treemacs-silent-filewatch t                ; 静默文件监控
        treemacs-collapse-dirs 3                   ; 折叠连续单子目录（最多3层）
        treemacs-file-event-delay 2000             ; 文件事件延迟（ms），降低刷新频率
        treemacs-indent-guide-style 'line)         ; 缩进引导线样式

  ;; ── 文件跟随与标签跟随 ─────────────────────────────────────
  (treemacs-follow-mode t)                         ; 自动跟随当前 buffer
  (treemacs-filewatch-mode t)                      ; 监控文件系统变化自动刷新
  (treemacs-fringe-indicator-mode 'always)         ; 始终显示左侧边缘指示器
  (treemacs-hide-gitignored-files-mode nil))       ; 不隐藏 .gitignore 的文件

;; ============================================================================
;; treemacs-magit — Git 状态集成
;; ============================================================================

(use-package treemacs-magit
  :ensure t
  :defer t
  :after (treemacs magit))

;; ============================================================================
;; treemacs-icons-dired — 在 dired 中使用 treemacs 图标
;; ============================================================================

(use-package treemacs-icons-dired
  :ensure t
  :defer t
  :hook (dired-mode . (lambda ()
                        (when (display-graphic-p)
                          (treemacs-icons-dired-enable-once)))))

(provide 'init-treemacs)
;;; init-treemacs.el ends here
