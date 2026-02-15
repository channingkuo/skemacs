;;; core-funcs.el --- 自定义工具函数 -*- lexical-binding: t -*-
;;; Commentary:

;; 自定义工具函数：导航、窗口管理、剪贴板操作、配置重载等。
;; 所有函数使用 skemacs/ 命名空间前缀。

;;; Code:

;; ============================================================================
;; 导航
;; ============================================================================

(defun skemacs/next-ten-lines ()
  "向下移动 10 行。"
  (interactive)
  (next-line 10))

(defun skemacs/previous-ten-lines ()
  "向上移动 10 行。"
  (interactive)
  (previous-line 10))

;; ============================================================================
;; 窗口管理
;; ============================================================================

(defun skemacs/split-window-vertically ()
  "垂直分割窗口并切换到新窗口。"
  (interactive)
  (split-window-right)
  (windmove-right))

(defun skemacs/split-window-horizontally ()
  "水平分割窗口并切换到新窗口。"
  (interactive)
  (split-window-below)
  (windmove-down))

(defun skemacs/shrink-window-horizontally (delta)
  "水平缩小窗口 DELTA 列。"
  (interactive "p")
  (shrink-window delta t))

(defun skemacs/enlarge-window-horizontally (delta)
  "水平放大窗口 DELTA 列。"
  (interactive "p")
  (enlarge-window delta t))

(defun skemacs/shrink-window (delta)
  "垂直缩小窗口 DELTA 行。"
  (interactive "p")
  (shrink-window delta))

(defun skemacs/enlarge-window (delta)
  "垂直放大窗口 DELTA 行。"
  (interactive "p")
  (enlarge-window delta))

;; ============================================================================
;; 剪贴板
;; ============================================================================

(defun skemacs/copy-to-clipboard ()
  "复制选中内容到系统剪贴板（跨平台）。"
  (interactive)
  (cond
   ((eq system-type 'gnu/linux)
    (if (display-graphic-p)
        (progn
          (require 'xclip nil t)
          (if (featurep 'xclip)
              (progn
                (xclip-mode 1)
                (if (use-region-p)
                    (progn
                      (xclip-copy-region (region-beginning) (region-end))
                      (message "已复制到剪贴板!")
                      (deactivate-mark))
                  (message "没有选中区域")))
            (message "xclip 包不可用")))
      (message "非图形界面")))
   ((eq system-type 'darwin)
    (if (use-region-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "pbcopy")
          (message "已复制到剪贴板!")
          (deactivate-mark))
      (message "没有选中区域")))
   ((eq system-type 'windows-nt)
    (if (use-region-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "clip.exe")
          (message "已复制到剪贴板!")
          (deactivate-mark))
      (message "没有选中区域")))
   (t (message "不支持的系统类型"))))

;; ============================================================================
;; 配置管理
;; ============================================================================

(defun skemacs/reload-config ()
  "重新加载 Emacs 配置。"
  (interactive)
  (load-file (concat user-emacs-directory "init.el"))
  (message "[skemacs] 配置重载完成"))

;; ============================================================================
;; Org 目录
;; ============================================================================

(defun skemacs/open-org-directory ()
  "打开 ~/org 目录。"
  (interactive)
  (let ((org-dir "~/org/"))
    (if (file-directory-p org-dir)
        (dired org-dir)
      (message "目录 %s 不存在" org-dir))))

(provide 'core-funcs)
;;; core-funcs.el ends here
