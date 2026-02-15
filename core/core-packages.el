;;; core-packages.el --- 包管理器配置 -*- lexical-binding: t -*-
;;; Commentary:

;; 配置 package.el 包源（清华镜像 + MELPA）、初始化 use-package。
;; package-initialize 延迟到首次安装包时执行，避免拖慢启动。

;;; Code:

;; ============================================================================
;; Emacs 版本检查
;; ============================================================================

(when (version< emacs-version "29.1")
  (message "[skemacs] 警告: Emacs 版本过旧 (%s)，建议升级到 29.1+" emacs-version))

;; ============================================================================
;; 包源配置（清华镜像 + MELPA 官方源作为备用）
;; ============================================================================

(require 'package)
(setq package-archives
      '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(add-to-list 'package-archives '("melpa-official" . "https://melpa.org/packages/") t)

;; ============================================================================
;; 延迟初始化 package.el（首次需要时自动执行）
;; ============================================================================

(defvar skemacs--package-initialized nil
  "package-initialize 是否已执行。")

(defun skemacs--ensure-package-init ()
  "确保 package.el 已初始化，仅在首次调用时执行。"
  (unless skemacs--package-initialized
    (package-initialize)
    (setq skemacs--package-initialized t)))

;; 在 package-install 前自动初始化
(advice-add 'package-install :before
            (lambda (&rest _) (skemacs--ensure-package-init)))

;; 在 use-package 的 :ensure 检查包是否已安装前自动初始化
(advice-add 'package-installed-p :before
            (lambda (&rest _) (skemacs--ensure-package-init)))

;; ============================================================================
;; use-package 初始化（Emacs 29+ 内置，无需 package-initialize）
;; ============================================================================

(require 'use-package)
(setq use-package-always-ensure t)       ; 自动安装缺失的包
(setq use-package-expand-minimally t)    ; 加速宏展开

;; ============================================================================
;; 网络代理（按需启用，默认注释）
;; ============================================================================

;; 如需代理，取消下面的注释并修改地址：
;; (setq url-proxy-services
;;       '(("http.*"     . "127.0.0.1:7890")
;;         ("https.*"    . "127.0.0.1:7890")
;;         ("socks5h://.*" . "127.0.0.1:7890")))

(provide 'core-packages)
;;; core-packages.el ends here
