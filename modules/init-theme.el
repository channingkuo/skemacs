;;; init-theme.el --- 主题管理与切换 -*- lexical-binding: t -*-
;;; Commentary:

;; 主题配置模块。支持本地主题（themes/ 目录下的 modus-themes）和
;; 第三方主题包（doom-themes、ef-themes、catppuccin 等）。
;; 提供交互式主题切换功能，可通过 C-j T 手动选择主题。
;;
;; 内置主题:
;;   - modus-operandi-tinted  （浅色，暖色调背景）
;;   - modus-vivendi-tritanopia（深色，黑色背景）
;;
;; 第三方主题（通过 use-package :ensure 自动安装）:
;;   - doom-themes 系列
;;   - ef-themes 系列
;;   - catppuccin-theme

;;; Code:

;; ============================================================================
;; 主题配置变量
;; ============================================================================

(defvar skemacs-default-theme 'modus-vivendi-tritanopia
  "启动时默认加载的主题。")

(defvar skemacs-theme-list
  '(;; ---- 本地 Modus 主题 ----
    modus-operandi-tinted
    modus-vivendi-tritanopia
    ;; ---- Doom 主题 ----
    doom-one
    doom-one-light
    doom-vibrant
    doom-dracula
    doom-gruvbox
    doom-gruvbox-light
    doom-monokai-pro
    doom-nord
    doom-nord-light
    doom-palenight
    doom-solarized-dark
    doom-solarized-light
    doom-tokyo-night
    doom-tomorrow-night
    ;; ---- Ef 主题 ----
    ef-autumn
    ef-spring
    ef-summer
    ef-winter
    ef-dark
    ef-light
    ef-duo-dark
    ef-duo-light
    ;; ---- Catppuccin 主题 ----
    catppuccin)
  "可选主题列表，用于 `skemacs/switch-theme' 的补全候选。")

(defvar skemacs--current-theme nil
  "当前已加载的主题名称（symbol）。")

;; ============================================================================
;; 本地 Modus 主题（themes/ 目录，无需安装）
;; ============================================================================

(require 'modus-themes nil t)

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-mixed-fonts nil
      modus-themes-prompts '(bold)
      modus-themes-completions '((matches . (extrabold))
                                 (selection . (semibold italic)))
      modus-themes-org-blocks 'tinted-background
      modus-themes-headings
      '((1 . (variable-pitch 1.3))
        (2 . (1.15))
        (t . (1.0))))

;; ============================================================================
;; 第三方主题包声明
;; ============================================================================

(use-package doom-themes
  :ensure t
  :defer t
  :config
  ;; doom-themes 全局设置
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; 优化 treemacs 主题（如果使用 treemacs）
  (with-eval-after-load 'treemacs
    (doom-themes-treemacs-config))
  ;; 修复 org-mode 的样式
  (doom-themes-org-config))

(use-package ef-themes
  :ensure t
  :defer t)

(use-package catppuccin-theme
  :ensure t
  :defer t
  :config
  (setq catppuccin-flavor 'mocha))

;; ============================================================================
;; 主题切换函数
;; ============================================================================

(defun skemacs--disable-all-themes ()
  "禁用所有当前已启用的主题。"
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun skemacs/load-theme (theme)
  "加载指定的 THEME，先禁用所有已启用的主题。
THEME 是一个 symbol，例如 'doom-one 或 'modus-vivendi-tritanopia。"
  (skemacs--disable-all-themes)
  (condition-case err
      (progn
        (load-theme theme t)
        (setq skemacs--current-theme theme)
        (message "[skemacs] 主题已切换: %s" theme))
    (error
     (message "[skemacs] 加载主题失败 %s: %s" theme (error-message-string err)))))

(defun skemacs/switch-theme ()
  "交互式选择并切换主题。
使用 `completing-read' 从 `skemacs-theme-list' 中选择主题。"
  (interactive)
  (let* ((theme-names (mapcar #'symbol-name skemacs-theme-list))
         (current (when skemacs--current-theme
                    (symbol-name skemacs--current-theme)))
         (prompt (if current
                     (format "切换主题 (当前: %s): " current)
                   "选择主题: "))
         (selected (completing-read prompt theme-names nil t nil nil current)))
    (when (and selected (not (string-empty-p selected)))
      (skemacs/load-theme (intern selected)))))

(defun skemacs/cycle-theme ()
  "在 `skemacs-theme-list' 中循环切换到下一个主题。"
  (interactive)
  (let* ((current-pos (cl-position skemacs--current-theme skemacs-theme-list))
         (next-pos (if current-pos
                       (mod (1+ current-pos) (length skemacs-theme-list))
                     0))
         (next-theme (nth next-pos skemacs-theme-list)))
    (skemacs/load-theme next-theme)))

(defun skemacs/toggle-light-dark ()
  "在浅色和深色主题之间快速切换。
浅色默认: modus-operandi-tinted
深色默认: modus-vivendi-tritanopia"
  (interactive)
  (let ((light-theme 'modus-operandi-tinted)
        (dark-theme 'modus-vivendi-tritanopia))
    (if (eq skemacs--current-theme light-theme)
        (skemacs/load-theme dark-theme)
      (skemacs/load-theme light-theme))))

;; ============================================================================
;; which-key 描述
;; ============================================================================

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-j T"   "切换主题"
    "C-j T T" "选择主题"
    "C-j T n" "下一个主题"
    "C-j T t" "明暗切换"))

;; ============================================================================
;; 快捷键
;; ============================================================================

;; C-j T 主题前缀
(define-prefix-command 'skemacs-theme-prefix)
(global-set-key (kbd "C-j T") 'skemacs-theme-prefix)

;; C-j T T — 选择主题（completing-read）
(global-set-key (kbd "C-j T T") 'skemacs/switch-theme)

;; C-j T n — 循环切换下一个主题
(global-set-key (kbd "C-j T n") 'skemacs/cycle-theme)

;; C-j T t — 明暗快速切换
(global-set-key (kbd "C-j T t") 'skemacs/toggle-light-dark)

;; ============================================================================
;; 启动时加载默认主题
;; ============================================================================

(skemacs/load-theme skemacs-default-theme)

(provide 'init-theme)
;;; init-theme.el ends here
