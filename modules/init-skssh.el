;;; init-skssh.el --- skssh SSH 连接管理器 -*- lexical-binding: t -*-
;;; Commentary:

;; 从 GitHub 安装并配置 skssh（https://github.com/channingkuo/skssh）。
;;
;; ── 主要命令 ────────────────────────────────────────────────────
;;
;;   M-x skssh                         打开 SSH 主机列表
;;   M-x skssh-add-host                交互式添加主机
;;   M-x skssh-sftp                    打开 SFTP 双窗格
;;   M-x skssh-import-from-ssh-config  从 ~/.ssh/config 一键导入主机
;;
;; ── 快捷键（以 C-c s 为前缀） ───────────────────────────────────
;;
;;   C-c s s   打开主机列表
;;   C-c s a   添加主机
;;   C-c s f   打开 SFTP
;;   C-c s i   从 ~/.ssh/config 导入

;;; Code:

(use-package skssh
  :vc (:url "https://github.com/channingkuo/skssh" :rev :newest)
  :commands (skssh skssh-add-host skssh-sftp skssh-import-from-ssh-config)
  :bind-keymap ("C-c s" . skssh-prefix-map)
  :bind (:map skssh-prefix-map
         ("s" . skssh)
         ("a" . skssh-add-host)
         ("f" . skssh-sftp)
         ("i" . skssh-import-from-ssh-config))
  :init
  (define-prefix-command 'skssh-prefix-map)
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-c s"   "skssh"
      "C-c s s" "Host Lists"
      "C-c s a" "Add Host"
      "C-c s f" "SFTP"
      "C-c s i" "Import ssh/config")))

(provide 'init-skssh)
;;; init-skssh.el ends here
