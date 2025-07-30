;;; 02-project-recent-files.el --- Project-aware recent files with ivy interface -*- lexical-binding: t -*-
;;; Commentary:

;; This file provides enhanced project recent file functionality that filters
;; recent files by current project context. Integrates with Projectile and
;; recentf to provide a better recent files experience within projects.
;; Overrides the default projectile-recentf command for improved usability.

;;; Code:

(defun skemacs/project-recent-files-get ()
  "Get recent files for current project from recentf."
  (when (and (fboundp 'projectile-project-p) (projectile-project-p))
    (let ((project-root (projectile-project-root)))
      ;; recentf-mode 现在在核心配置中启用，不需要条件性启用
      (let ((filtered-files (seq-filter (lambda (file)
                                          (and (file-exists-p file)
                                               (string-prefix-p project-root (expand-file-name file))))
                                        recentf-list)))
        ;; (message "Filtered project files: %s" filtered-files)
        filtered-files))))


(defun skemacs/project-recent-files-restore ()
  "Restore and open a project recent file from saved records."
  (interactive)
  (if (and (fboundp 'projectile-project-p) (projectile-project-p))
      (let ((recent-files (skemacs/project-recent-files-get)))
        (if recent-files
            (let ((relative-files (mapcar (lambda (file)
                                            (file-relative-name file (projectile-project-root)))
                                          recent-files)))
              (let ((ivy-truncate-lines nil))  ; 允许换行显示长路径
                (ivy-read "Recent files: " relative-files
                          :action (lambda (relative-path)
                                    (let ((full-path (expand-file-name relative-path (projectile-project-root))))
                                      (if (file-exists-p full-path)
                                          (find-file full-path)
                                        (message "File no longer exists: %s" full-path))))
                          :caller 'skemacs/project-recent-files-restore)))
          (message "No recent files found for current project")))
    (message "Not in a project")))

;; Override projectile-recentf to use my implementation
(with-eval-after-load 'projectile
  (define-key projectile-command-map (kbd "e") #'skemacs/project-recent-files-restore))

;;; 002-project-recent-files.el ends here
