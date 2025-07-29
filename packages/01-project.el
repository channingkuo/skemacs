(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-indexing-method 'alien)
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :init (counsel-projectile-mode))

;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :ensure t
  :after (projectile)
  :config
  (add-to-list 'dashboard-items '(agenda) t)
  (setq dashboard-week-agenda t)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (setq dashboard-banner-logo-title "Welcome to skemacs!")
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-startup-banner (skemacs/get-banner-path))
  (setq dashboard-buffer-name "*skemacs*")
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents   . 10)
                          (projects  . 10)
                          (bookmarks . 5)
                          (agenda    . 5)))
  ;; (dashboard-setup-startup-hook)  ; 禁用原始启动hook，使用自定义启动逻辑
)