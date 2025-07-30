;; Window management hydra
(use-package hydra
  :ensure t
  :config
  (defhydra hydra-window-resize (:hint nil)
	    "
  _{_: shrink vertical  _}_: enlarge vertical  _[_: shrink horizontal  _]_ : enlarge horizontal  _q_: quit"
	    ("{"   skemacs/shrink-window)
	    ("}"   skemacs/enlarge-window)
	    ("["   skemacs/shrink-window-horizontally)
	    ("]"   skemacs/enlarge-window-horizontally)
	    ("q"   nil :exit t :color blue)))
