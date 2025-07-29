;; https://github.com/Malabarba/smart-mode-line
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
     ("^~/Documents/ChanningKuo/*" ":Kuo:"))))
