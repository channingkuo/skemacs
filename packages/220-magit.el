;;; 101-magit.el --- Git porcelain for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration sets up Magit, a comprehensive Git interface
;; for Emacs.. Provides an intuitive interface for all Git operations
;; including staging, committing, branching, merging, and history
;; browsing.. Optimized for performance in large repositories.
;; Magit: https://magit.vc/

;;; Code:

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-auto-revert-mode nil)
  (setq magit-refresh-status-buffer nil))

;;; 220-magit.el ends here
