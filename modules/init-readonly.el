;;; init-readonly.el --- 默认以只读模式打开文件 -*- lexical-binding: t -*-
;;; Commentary:

;; 所有通过 find-file 打开的文件默认进入 view-mode（只读浏览模式）。
;; 需要编辑时用 C-x C-q 切换回可写模式。
;;
;; view-mode 常用快捷键：
;;
;;   导航
;;     SPC / DEL       向下/向上翻页
;;     d / u           向下/向上半页
;;     n / p           下一行/上一行
;;     < / >           跳到 buffer 开头/末尾
;;
;;   搜索
;;     s               增量搜索 (isearch-forward)
;;     r               反向增量搜索 (isearch-backward)
;;     / 或 \           正则搜索
;;
;;   退出
;;     q               退出 view-mode 并关闭 buffer
;;     e               退出 view-mode 保留 buffer
;;     C-x C-q         退出只读模式，进入编辑

;;; Code:

;; ============================================================================
;; view-mode：默认只读浏览
;; ============================================================================

(add-hook 'find-file-hook #'view-mode)

(with-eval-after-load 'view
  (define-key view-mode-map (kbd "C-x C-q")
              (lambda ()
                (interactive)
                (view-mode -1)
                (read-only-mode -1))))

(provide 'init-readonly)
;;; init-readonly.el ends here
