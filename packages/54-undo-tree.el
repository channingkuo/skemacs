;; 替换C-x u
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
