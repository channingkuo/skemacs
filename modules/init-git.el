;;; init-git.el --- Git 集成 (Magit + diff-hl) -*- lexical-binding: t -*-
;;; Commentary:

;; 使用 Magit 提供完整的 Git 操作界面，配合 diff-hl 在侧边栏显示修改标记。
;;
;; Magit 是 Emacs 生态中最强大的 Git 客户端，支持 stage/commit/push/pull/
;; rebase/merge/stash/log/blame 等几乎所有 Git 操作，全部通过交互式界面完成。
;;
;; 快捷键（挂载到 C-j g 前缀下）：
;;   C-j g g — 打开 Magit 状态面板（magit-status）
;;   C-j g l — 查看 Git 日志（magit-log-current）
;;   C-j g b — 查看当前文件 blame（magit-blame-addition）
;;   C-j g d — 查看当前文件 diff（magit-diff-buffer-file）
;;   C-j g f — 查看文件的 Git 日志（magit-log-buffer-file）
;;   C-j g s — Stage 当前文件（magit-stage-file）
;;   C-j g u — Unstage 当前文件（magit-unstage-file）

;;; Code:

;; ============================================================================
;; magit — Git 操作界面
;; ============================================================================

(use-package magit
  :ensure t
  :defer t
  :bind
  (("C-j g g" . magit-status)                  ; Git 状态面板
   ("C-j g l" . magit-log-current)             ; 当前分支日志
   ("C-j g b" . magit-blame-addition)          ; 文件 blame
   ("C-j g d" . magit-diff-buffer-file)        ; 当前文件 diff
   ("C-j g f" . magit-log-buffer-file)         ; 当前文件日志
   ("C-j g s" . magit-stage-file)              ; Stage 当前文件
   ("C-j g u" . magit-unstage-file))           ; Unstage 当前文件
  :init
  ;; which-key 描述
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-j g"   "Git"
      "C-j g g" "Magit status"
      "C-j g l" "Log (current branch)"
      "C-j g b" "Blame"
      "C-j g d" "Diff buffer file"
      "C-j g f" "Log buffer file"
      "C-j g s" "Stage file"
      "C-j g u" "Unstage file"))
  :config
  ;; ── 基础配置 ──────────────────────────────────────────────
  ;; 打开 magit-status 时使用全窗口显示
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1)

  ;; 保存文件时自动刷新 magit buffer
  (setq magit-auto-revert-mode t)

  ;; 提交信息填写区域使用 fill-column 指示器
  (add-hook 'git-commit-setup-hook
            (lambda ()
              (setq fill-column 72)
              (display-fill-column-indicator-mode 1))))

;; ============================================================================
;; diff-hl — 侧边栏 Git 修改标记
;; ============================================================================

(use-package diff-hl
  :ensure t
  :defer t
  :hook
  ((after-init  . global-diff-hl-mode)          ; 全局启用
   (dired-mode  . diff-hl-dired-mode)           ; dired 中也显示
   (magit-pre-refresh  . diff-hl-magit-pre-refresh)   ; magit 操作前刷新
   (magit-post-refresh . diff-hl-magit-post-refresh))  ; magit 操作后刷新
  :config
  ;; 在终端（非 GUI）下使用 margin 模式替代 fringe
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1)))

(provide 'init-git)
;;; init-git.el ends here
