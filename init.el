;;; init.el --- Emacs 配置启动入口 -*- lexical-binding: t -*-
;;; Commentary:

;; 启动入口文件。按显式顺序加载 core/ 下的核心模块，
;; 然后通过模块系统加载 modules/ 下的功能模块。
;; 加载过程实时显示在启动 splash 画面中，加载完成后按 Enter 进入 dired。
;;
;; 启动流程:
;;   early-init.el  →  init.el  →  splash 画面  →  core/*.el  →  modules/*.el  →  按 Enter 进入
;;
;; 使用 M-x skemacs/show-load-times 查看每个模块的加载耗时。

;;; Code:

;; ============================================================================
;; 记录启动时间
;; ============================================================================

(defvar skemacs-start-time (current-time)
  "Emacs 启动时间戳。")

;; ============================================================================
;; 引导 load-path（先于一切 require）
;; ============================================================================

;; core/ 必须先加入 load-path，否则无法 require 其中的文件
(add-to-list 'load-path
             (expand-file-name "core" user-emacs-directory))

;; ============================================================================
;; 加载核心基础设施（不计时，因为模块系统本身还没就绪）
;; ============================================================================

;; 1. 路径定义（设置 modules/、local/ 等其余目录的 load-path）
(require 'core-load-paths)

;; 2. 模块加载系统（提供 splash 画面和 skemacs-load-module 等基础设施）
(require 'core-module)

;; ============================================================================
;; 自定义配置文件（尽早设置，防止 Custom 写入 init.el）
;; ============================================================================

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; ============================================================================
;; 初始化 Splash 画面（在加载任何模块之前）
;; ============================================================================

(skemacs--splash-init)

;; ============================================================================
;; 加载核心模块（实时显示在 splash 中）
;; ============================================================================

(skemacs-load-core-module "core-packages")
(skemacs-load-core-module "core-ui")
(skemacs-load-core-module "core-editor")
(skemacs-load-core-module "core-funcs")
(skemacs-load-core-module "core-keybindings")

;; ============================================================================
;; 加载功能模块（实时显示在 splash 中）
;; ============================================================================

;; 自动扫描 modules/ 目录下所有 .el 文件并加载
;; 如需指定加载顺序或禁用某些模块，可设置:
;;   (setq skemacs-module-list '("init-theme" "init-which-key" ...))
(setq skemacs-disabled-modules '("init-tramp"))
(skemacs-load-all-modules)

;; ============================================================================
;; 加载自定义配置（custom-file 已在上方设置）
;; ============================================================================

(when (file-exists-p custom-file)
  (load custom-file nil t))

;; ============================================================================
;; 启动完成：恢复 GC + 完成 splash 画面
;; ============================================================================

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; 恢复 GC 阈值到正常水平（16MB）
            (setq gc-cons-threshold (* 16 1024 1024))
            (setq gc-cons-percentage 0.1)
            ;; 打印加载时间报告到 *Messages*
            (skemacs--print-load-report)
            ;; 检测启动模式：emacs file → 跳过等待；emacs / emacs dir → 等待 Enter
            (let ((elapsed (float-time (time-subtract (current-time) skemacs-start-time)))
                  (file-mode (skemacs--started-with-file-p)))
              (message "[skemacs] Emacs ready, startup time: %.3fs%s"
                       elapsed (if file-mode " (file mode)" ""))
              (skemacs--splash-finalize elapsed file-mode))))

(provide 'init)
;;; init.el ends here
