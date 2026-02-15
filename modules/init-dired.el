;;; init-dired.el --- Dired 增强配置 -*- lexical-binding: t -*-
;;; Commentary:

;; 增强 Dired 文件管理器：
;; - 使用 dired-omit-mode 过滤不需要显示的文件（如 .DS_Store）
;; - 可通过 C-x M-o 在 dired buffer 中临时切换显示/隐藏

;;; Code:

;; ============================================================================
;; dired-x：过滤隐藏不需要的文件
;; ============================================================================

(use-package dired
  :ensure nil                             ; 内置包
  :hook
  ;; 进入 dired 时自动启用 omit 模式
  (dired-mode . dired-omit-mode)
  :config
  (require 'dired-x)

  ;; ── 要隐藏的文件匹配规则（正则表达式）──────────────────────
  ;; 默认值会匹配以 . 开头的隐藏文件，这里改为只匹配特定的无用文件
  (setq dired-omit-files
        (concat "\\(?:"
                "\\.DS_Store"               ; macOS 目录元数据
                "\\|Thumbs\\.db"            ; Windows 缩略图缓存
                "\\|desktop\\.ini"          ; Windows 桌面配置
                "\\|__MACOSX"              ; macOS 压缩包产生的垃圾目录
                "\\)"))

  ;; 是否同时按扩展名过滤（如 .elc .pyc 等编译产物）
  ;; 取消注释下面的行来启用
  ;; (setq dired-omit-extensions
  ;;       (append dired-omit-extensions '(".elc" ".pyc" ".pyo" ".o")))

  ;; 隐藏 omit 模式的 modeline 提示，减少视觉干扰
  (setq dired-omit-verbose nil)

  ;; ── 其他实用 dired 设置 ─────────────────────────────────────
  ;; 两个 dired 窗口时，操作（复制/移动）默认目标为另一个窗口的目录
  ;; (setq dired-dwim-target t)
  ;; 递归复制和删除目录时不反复确认
  (setq dired-recursive-copies 'always)
  ;; (setq dired-recursive-deletes 'always)
  )

(provide 'init-dired)
;;; init-dired.el ends here
