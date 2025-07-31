;;; 020-backup.el --- auto save and backup configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file contains auto save and backup configuration for Emacs.

;;; Code:

;; 文件自动备份
(setq make-backup-files t)
(let ((backups-dir (concat user-emacs-directory "backups/")))
  ;; 确保目录存在
  (unless (file-directory-p backups-dir)
    (make-directory backups-dir t))
  (setq backup-directory-alist
      `((".*" . ,backups-dir))))
(setq backup-by-copying t)
(setq version-control t)
(setq kept-new-versions 3)
(setq delete-old-versions t)
;; 配置自动保存文件位置
(let ((auto-save-dir (concat user-emacs-directory "auto-save/")))
  ;; 确保目录存在
  (unless (file-directory-p auto-save-dir)
    (make-directory auto-save-dir t))
  ;; 设置自动保存文件转换规则
  (setq auto-save-file-name-transforms
        `((".*" ,auto-save-dir t))))

;;; 020-backup.el ends here
