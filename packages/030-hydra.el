;;; 02-hydra.el --- Transient key binding system setup -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration sets up Hydra, a package for creating transient
;; key binding menus.. Hydras provide a convenient way to group related
;; commands and create modal interfaces for complex operations.
;; hydra: https://github.com/abo-abo/hydra

;;; Code:

(use-package hydra
  :ensure t)

(use-package use-package-hydra
  :ensure t
  :after hydra)

;;; 030-hydra.el ends here
