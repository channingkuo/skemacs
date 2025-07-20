;; Project recent files tracking using recentf
(require 'projectile)
(require 'recentf)

(defun skemacs/project-recent-files-get ()
  "Get recent files for current project from recentf."
  (when (and (fboundp 'projectile-project-p) (projectile-project-p))
    (let ((project-root (projectile-project-root)))
      ;; (message "recentf-mode enabled: %s" recentf-mode)
      ;; (message "recentf-save-file: %s" recentf-save-file)
      ;; (message "recentf file exists: %s" (file-exists-p recentf-save-file))
      (when (not recentf-mode)
        ;; (message "Enabling recentf-mode...")
        (recentf-mode 1)
        ;; (message "Loading recentf data...")
        (recentf-load-list))
      ;; (message "recentf-list length: %d" (length recentf-list))
      ;; (message "recentf-list: %s" recentf-list)
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
              (ivy-read "Recent files: " relative-files
                        :action (lambda (relative-path)
                                  (let ((full-path (expand-file-name relative-path (projectile-project-root))))
                                    (if (file-exists-p full-path)
                                        (find-file full-path)
                                      (message "File no longer exists: %s" full-path))))
                        :caller 'skemacs/project-recent-files-restore))
          (message "No recent files found for current project")))
    (message "Not in a project")))

;; Override projectile-recentf to use our implementation
(with-eval-after-load 'projectile
  (define-key projectile-command-map (kbd "e") #'skemacs/project-recent-files-restore))