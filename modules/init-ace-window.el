;;; init-ace-window.el --- Ace-window 窗口快速跳转 -*- lexical-binding: t -*-
;;; Commentary:

;; 使用 ace-window 替代内置 other-window，快速切换窗口：
;; - 两个窗口时直接跳转，多于两个时显示编号提示
;; - 绑定到 C-x o 替代默认的 other-window

;;; Code:

;; ============================================================================
;; ace-window：窗口快速跳转
;; ============================================================================

(use-package ace-window
  :ensure t
  :defer t
  :bind
  (("C-x o" . ace-window))             ; 替代内置 other-window
  :config
  ;; 当窗口数 >= 3 时才显示编号提示；2 个窗口直接跳转
  (setq aw-dispatch-always nil)

  ;; 使用 home row 键作为窗口编号，方便快速按
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  ;; 提示字符使用大号字体，更醒目
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))

(provide 'init-ace-window)
;;; init-ace-window.el ends here
