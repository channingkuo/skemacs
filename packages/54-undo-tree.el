;;; 54-undo-tree.el --- Visual undo system with Hydra interface -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration replaces the default undo system with undo-tree,
;; which provides a branching undo history that preserves all edit
;; history. Includes a Hydra interface for easy navigation and
;; visualization of the undo tree structure.

;;; Code:

(use-package undo-tree
  :ensure t
  :after hydra
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil)
  :hydra (hydra-undo-tree (:hint nil)
			  "
  _p_: Undo  _n_: Redo   "
			  ("p"   undo-tree-undo)
			  ("n"   undo-tree-redo)
			  ;; ("s"   undo-tree-save-history)
			  ;; ("l"   undo-tree-load-history)
			  ("v"   undo-tree-visualize "Visualize" :color blue)
			  ("q"   nil "Quit hydra" :color blue)))

;;; 54-undo-tree.el ends here
