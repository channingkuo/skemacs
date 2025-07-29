;; org-bullets - 美化org mode标题项目符号
(use-package org-bullets
  :ensure t
  :defer t
  :hook (org-mode . org-bullets-mode)
  :config
  ;; 设置自定义的项目符号，从漂亮的Unicode符号中选择
  ;; org-bullets会按照层级顺序使用这些符号 (1级, 2级, 3级...)
  (setq org-bullets-bullet-list
        '("◉"    ; 1级 - 实心圆，突出重要性
          "○"    ; 2级 - 空心圆，清晰易读  
          "✸"    ; 3级 - 星花，醒目
          "✿"    ; 4级 - 花朵，优雅
          "◎"    ; 5级 - 同心圆
          "⦿"    ; 6级 - 同心圆，内实心
          "◦"))  ; 7级+ - 小圆

  ;; 备选方案1: 极简风格
  ;; (setq org-bullets-bullet-list '("●" "○" "▸" "▹" "▪" "▫" "▴"))

  ;; 备选方案2: 星型主题
  ;; (setq org-bullets-bullet-list '("★" "☆" "✦" "✧" "✩" "✪" "✫"))

  ;; 备选方案3: 几何图形
  ;; (setq org-bullets-bullet-list '("◆" "◇" "◈" "◎" "●" "○" "◦"))

  ;; 备选方案4: 箭头风格
  ;; (setq org-bullets-bullet-list '("➤" "▶" "▸" "▹" "►" "▻" "⦿"))

  ;; 备选方案5: 经典符号
  ;; (setq org-bullets-bullet-list '("§" "¶" "※" "❖" "♦" "◆" "▲"))
  )