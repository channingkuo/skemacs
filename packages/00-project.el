(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile
              ("C-c p" . projectile))
  :config
  (setq projectile-indexing-method 'alien)
  (setq projectile-switch-project-action #'projectile-dired))
;;   (setq projectile-mode-line "Projectile")
;;   (setq projectile-track-known-projects-automatically nil))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :init (counsel-projectile-mode))

;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :ensure t
  :after (projectile)
  :config
  (setq dashboard-banner-logo-title "Welcome to skemacs!") ;; 个性签名，随读者喜好设置
  (setq dashboard-projects-backend 'projectile) ;; 安装了 projectile 后再使用
  (setq dashboard-startup-banner (skemacs/get-banner-path)) ;; 也可以自定义图片
  (setq dashboard-buffer-name "*skemacs*") ;; 自定义dashboard buffer名称
  (setq dashboard-items '((recents  . 5)    ;; 显示多少个最近文件
			              (bookmarks . 5)   ;; 显示多少个最近书签
			              (projects . 10))) ;; 显示多少个最近项目
  ;; (dashboard-setup-startup-hook)  ; 禁用原始启动hook，使用自定义启动逻辑
  )