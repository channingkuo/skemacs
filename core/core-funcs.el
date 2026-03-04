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
  "Copy selected region to system clipboard (cross-platform)."
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
                      (message "Copied to clipboard!")
                      (deactivate-mark))
                  (message "No region selected")))
            (message "xclip package not available")))
      (message "Not in graphical mode")))
   ((eq system-type 'darwin)
    (if (use-region-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "pbcopy")
          (message "Copied to clipboard!")
          (deactivate-mark))
      (message "No region selected")))
   ((eq system-type 'windows-nt)
    (if (use-region-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "clip.exe")
          (message "Copied to clipboard!")
          (deactivate-mark))
      (message "No region selected")))
   (t (message "Unsupported system type"))))

;; ============================================================================
;; Org 目录
;; ============================================================================

(defun skemacs/open-org-directory ()
  "打开 ~/org 目录。"
  (interactive)
  (let ((org-dir "~/org/"))
    (if (file-directory-p org-dir)
        (dired org-dir)
      (message "Directory %s does not exist" org-dir))))

;; ============================================================================
;; 显示
;; ============================================================================

(setq display-line-numbers-type 'relative)

(defun skemacs/toggle-line-numbers-type ()
  "在相对行号和绝对行号之间切换。"
  (interactive)
  (if (eq display-line-numbers-type 'relative)
      (progn (setq display-line-numbers-type t)
             (message "Line numbers: absolute"))
    (setq display-line-numbers-type 'relative)
    (message "Line numbers: relative"))
  (when display-line-numbers-mode
    (display-line-numbers-mode 1)))

(provide 'core-funcs)
;;; core-funcs.el ends here
