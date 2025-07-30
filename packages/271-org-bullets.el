;;; 201-org-bullets.el --- Beautiful bullet points for Org-mode headlines -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration enhances Org-mode by replacing the default
;; asterisk-based headline markers with attractive Unicode symbols.
;; Includes multiple theme options and customizable bullet styles
;; to improve readability and visual appeal of Org documents.

;;; Code:

(use-package org-bullets
  :ensure t
  :defer t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list
        '("◉"    ; 1级 - 实心圆，突出重要性
          "○"    ; 2级 - 空心圆，清晰易读
          "✸"    ; 3级 - 星花，醒目
          "✿"    ; 4级 - 花朵，优雅
          "◎"    ; 5级 - 同心圆
          "⦿"    ; 6级 - 同心圆，内实心
          "◦"))  ; 7级+ - 小圆
  )

;;; 271-org-bullets.el ends here
