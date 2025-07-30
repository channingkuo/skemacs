;;; 99-window-hydra.el --- Window management with Hydra interface -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration provides a Hydra interface for window management
;; operations including resizing windows vertically and horizontally.
;; Offers a convenient modal interface for common window operations
;; without remembering multiple key bindings.

;;; Code:

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

;;; 200-window-hydra.el ends here
