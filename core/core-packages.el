;;; core-packages.el --- 包管理器配置 -*- lexical-binding: t -*-
;;; Commentary:

;; 配置 package.el 包源（MELPA 官方源）、初始化 use-package。

;;; Code:

;; ============================================================================
;; Emacs 版本检查
;; ============================================================================

(when (version< emacs-version "29.1")
  (message "[skemacs] 警告: Emacs 版本过旧 (%s)，建议升级到 29.1+" emacs-version))

;; ============================================================================
;; 包源配置（MELPA 官方源）
;; ============================================================================

(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

;; ============================================================================
;; 初始化 package.el
;; ============================================================================

;; 扫描 elpa/ 目录，填充 package-alist，激活已安装的包
;; Emacs 30 + native-comp 下此操作通常 < 50ms
(package-initialize)

;; ============================================================================
;; use-package 初始化（Emacs 29+ 内置，无需 package-initialize）
;; ============================================================================

(require 'use-package)
(setq use-package-always-ensure t)       ; 自动安装缺失的包
(setq use-package-expand-minimally t)    ; 加速宏展开

;; ============================================================================
;; 网络代理（按需启用，默认注释）
;; ============================================================================

;; 注意：url-proxy-services 的 key 必须是协议名（不是正则），不支持 SOCKS5
;; (setq url-proxy-services
;;       '(("http"  . "127.0.0.1:7890")
;;         ("https" . "127.0.0.1:7890")
;;         ("no_proxy" . "^\\(localhost\\|127\\.0\\.0\\.1\\)")))

(provide 'core-packages)
;;; core-packages.el ends here
