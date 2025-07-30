;;; 102-treemacs.el --- Tree-style file explorer with project integration -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration sets up Treemacs, a tree layout file explorer
;; for Emacs.. Integrates with Projectile for project-aware navigation
;; and provides a familiar sidebar interface for browsing project
;; structure and files.
;; treemacs: https://github.com/Alexander-Miller/treemacs

;;; Code:

(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-width 30
        treemacs-width-is-initially-locked nil        ; Allow resizing from start
        treemacs-display-in-side-window nil           ; Use normal window instead of side window
        treemacs-is-never-other-window nil
        treemacs-show-cursor nil                      ; Hide cursor in treemacs
        treemacs-silent-refresh t)                    ; Suppress refresh messages
  ;; 禁用tag-follow-mode以避免显示函数
  (treemacs-tag-follow-mode -1)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
	("/" . treemacs-advanced-helpful-hydra)
	("C-j" . nil))) ; Unbind C-j to allow our global prefix key to work

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

;; (use-package lsp-treemacs
;;   :ensure t
;;   :after (treemacs lsp))

;;; 230-treemacs.el ends here
