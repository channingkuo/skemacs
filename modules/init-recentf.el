;;; init-recentf.el --- 最近文件与 Buffer 列表 -*- lexical-binding: t -*-
;;; Commentary:

;; 启用 recentf-mode 记录最近打开的文件，并配置 ibuffer 作为
;; 增强版 buffer 列表。
;;
;; 快捷键（挂载到 C-x b 前缀下）：
;;   C-x b r — 最近打开的文件（completing-read 选择）
;;   C-x b R — 最近打开的文件（列表窗口）
;;   C-x b l — 所有 Buffer 列表（ibuffer）

;;; Code:

;; ============================================================================
;; recentf — 最近打开文件
;; ============================================================================

(use-package recentf
  :ensure nil                             ; Emacs 内置
  :init
  ;; 记录最多 200 个文件
  (setq recentf-max-saved-items 200)
  ;; 记录在菜单中显示的最大数量
  (setq recentf-max-menu-items 30)
  ;; 每 5 分钟自动保存一次列表（防止意外丢失）
  (setq recentf-auto-cleanup 'never)
  ;; 排除不需要记录的文件
  (setq recentf-exclude
        '("/tmp/"
          "/ssh:"
          "/sudo:"
          "\\.elc$"
          "COMMIT_EDITMSG"
          "\\.git/"
          "/elpa/"
          "/eln-cache/"
          "recentf$"
          "/node_modules$/"
          "bookmarks$"))
  :config
  (recentf-mode 1)

  ;; 每 5 分钟自动保存 recentf 列表
  (run-at-time nil (* 5 60) 'recentf-save-list)

  ;; ── 快捷键 ────────────────────────────────────────────────
  ;; C-x b r — 用 completing-read 选择最近文件（支持 icomplete / vertico 等）
  (global-set-key (kbd "C-x b r") 'recentf-open)
  ;; C-x b R — 打开最近文件列表窗口（可点击选择）
  (global-set-key (kbd "C-x b R") 'recentf-open-files))

;; ============================================================================
;; ibuffer — 增强版 Buffer 列表
;; ============================================================================

(use-package ibuffer
  :ensure nil                             ; Emacs 内置
  :init
  ;; 默认不显示空的 filter group
  (setq ibuffer-show-empty-filter-groups nil)
  ;; 打开时不询问确认
  (setq ibuffer-expert t)
  :config
  ;; C-x b l — 打开 ibuffer
  (global-set-key (kbd "C-x b l") 'ibuffer))

(provide 'init-recentf)
;;; init-recentf.el ends here
