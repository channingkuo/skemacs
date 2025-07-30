;;; 03-recentf.el --- Recent files tracking with enhanced filtering -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration sets up the built-in recentf package for tracking
;; recently opened files with enhanced filtering to exclude temporary files,
;; auto-generated content, and package directories.. Includes automatic
;; cleanup and immediate saving of new files.

;;; Code:

(use-package recentf
  :ensure nil  ; Built-in package
  :demand t    ; Load immediately, don't defer
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
  
  ;; 确保立即保存新添加的文件
  (add-hook 'find-file-hook (lambda ()
                              (recentf-save-list)))
  
  ;; 定期清理失效文件
  (run-at-time nil (* 5 60) 'recentf-cleanup))

;;; 060-recentf.el ends here
