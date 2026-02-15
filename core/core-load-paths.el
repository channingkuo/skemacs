;;; core-load-paths.el --- 路径常量与 load-path 设置 -*- lexical-binding: t -*-
;;; Commentary:

;; 定义项目中所有目录的路径常量，并将必要的目录加入 load-path。
;; 此文件应在所有其他 core 文件之前加载。

;;; Code:

;; ============================================================================
;; 路径常量
;; ============================================================================

(defconst skemacs-dir user-emacs-directory
  "Emacs 配置根目录。")

(defconst skemacs-core-dir (expand-file-name "core" skemacs-dir)
  "核心配置文件目录。")

(defconst skemacs-modules-dir (expand-file-name "modules" skemacs-dir)
  "功能模块目录，每个文件为一个自包含的模块。")

(defconst skemacs-local-dir (expand-file-name "local" skemacs-dir)
  "本地自定义配置目录（不纳入版本控制）。")

(defconst skemacs-cache-dir (expand-file-name ".cache" skemacs-dir)
  "缓存文件目录（自动生成的数据文件）。")

;; ============================================================================
;; 确保目录存在
;; ============================================================================

(dolist (dir (list skemacs-modules-dir skemacs-local-dir skemacs-cache-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; ============================================================================
;; 设置 load-path
;; ============================================================================

(dolist (dir (list skemacs-core-dir skemacs-modules-dir skemacs-local-dir))
  (unless (member dir load-path)
    (add-to-list 'load-path dir)))

(provide 'core-load-paths)
;;; core-load-paths.el ends here
