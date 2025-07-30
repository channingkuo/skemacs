;;; 53-smart-mode-line.el --- Enhanced mode line with path abbreviation -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration sets up smart-mode-line for a cleaner, more
;; informative mode line display.. Includes path abbreviations for
;; common directories and mode name filtering to reduce clutter
;; while maintaining essential information.
;; ;; smart-mode-line: https://github.com/Malabarba/smart-mode-line

;;; Code:

(use-package smart-mode-line
  :ensure t
  :defer 2
  :init
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  ;; (setq sml/mode-width 0)
  ;; (setq sml/name-width '(16 . 48))
  :config
  (setq rm-blacklist
	(format "^ \\(%s\\)$"
		(mapconcat #'identity
			   '("Projectile.*" "company.*" "AcePY" "ws" "hs" "Wrap" "Abbrev" "ElDoc" "Ind" "Flymake.*" "ELDOC-BOX"
			     "Undo-Tree" "counsel" "ivy" "yas" "WK" "GG" "Hi" "Apheleia" "Lens" "AD")
			   "\\|")))
  :custom
  (sml/replacer-regexp-list
   '(("^~/\\.emacs\\.d/"           ":Emacs:")
     ("^~/Documents/YSW/Source/*"  ":YSW:")
     ("^~/Documents/ChanningKuo/*" ":Kuo:")))
  )

;;; 180-smart-mode-line.el ends here)
