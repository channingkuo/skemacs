;;; 010-recentf.el --- Recent files tracking with enhanced filtering -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration sets up the built-in recentf package for tracking
;; recently opened files with enhanced filtering to exclude temporary files,
;; auto-generated content, and package directories.. Includes automatic
;; cleanup and immediate saving of new files.

;;; Code:

(use-package recentf
  :ensure nil  ; Built-in package
  :demand t    ; Load immediately, don't defer
  :init
  (recentf-mode 1)
  :config
  ;; 排除一些不需要记录的文件类型和目录
  (add-to-list 'recentf-exclude "\\.?cache")
  (add-to-list 'recentf-exclude "\\.?tmp")
  (add-to-list 'recentf-exclude "/var/folders/")
  (add-to-list 'recentf-exclude "/tmp/")
  (add-to-list 'recentf-exclude ".*-autoloads\\.el\\'")  ; Auto-generated files
  (add-to-list 'recentf-exclude ".*\\.\\(gz\\|zip\\|tar\\)\\'")
  (add-to-list 'recentf-exclude ".*elpa.*")  ; Package directory
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG")  ; Git commit messages
  (add-to-list 'recentf-exclude "bookmarks")
  (add-to-list 'recentf-exclude "\\.gitignore")

  (setq recentf-max-saved-items 1000)
  
  ;; 每30s自动保存一次
  (setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))

  ;; 确保退出时保存列表
  (add-hook 'kill-emacs-hook 'recentf-save-list)
  ;; 确保新文件立即保存到 recentf 列表
  (add-hook 'find-file-hook
            (lambda ()
              (unless (member (buffer-file-name) recentf-list)
                (recentf-add-file (buffer-file-name)))))
  ;; TODO 最近文件列表关联项目，使用emacs打开不同项目时，自动切换到对应的 recentf 列表，互相不会干扰
  )

;;; 060-recentf.el ends here
