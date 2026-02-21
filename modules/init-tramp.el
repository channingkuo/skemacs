;;; init-tramp.el --- TRAMP 远程文件访问配置 -*- lexical-binding: t -*-
;;; Commentary:

;; 配置 TRAMP 以便通过 SSH 透明地访问远程服务器上的文件。
;;
;; ── 使用方法 ────────────────────────────────────────────────────
;;
;; C-x C-f 然后输入远程路径:
;;
;;   /ssh:user@host:/path/to/file        基本 SSH 访问
;;   /ssh:user@host#2222:/path           非默认端口
;;   /ssh:myalias:/path                  使用 ~/.ssh/config 中的别名
;;   /ssh:user@host|sudo:host:/etc/conf  远程 sudo 编辑
;;   /sudo::/etc/hosts                   本地 sudo
;;   /ssh:user@jump|ssh:user@target:/p   多跳（跳板机）
;;
;; Dired 同样支持远程路径:
;;   C-x d /ssh:user@host:/path/

;;; Code:

(use-package tramp
  :ensure nil
  :defer t
  :config
  (setq tramp-default-method "ssh")

  ;; 减少日志输出以提升性能（默认 3，0=静默 10=最详细）
  (setq tramp-verbose 1)

  ;; 启用文件名缓存，减少远程查询
  (setq remote-file-name-inhibit-cache nil)

  ;; 复用 SSH ControlMaster 连接（需要 ~/.ssh/config 中配置 ControlMaster）
  (setq tramp-use-ssh-controlmaster-options t)

  ;; 远程路径下禁用 vc，避免不必要的 git 查询拖慢速度
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

  ;; 持久化连接，避免频繁重连
  (setq tramp-persistency-file-name
        (expand-file-name "tramp" user-emacs-directory)))

(provide 'init-tramp)
;;; init-tramp.el ends here
