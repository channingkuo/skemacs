;;; init-vterm.el --- 终端模拟器 (vterm) -*- lexical-binding: t -*-
;;; Commentary:

;; 使用 vterm 提供高性能终端模拟器，基于 libvterm C 库实现，
;; 支持完整的终端功能（颜色、光标移动、滚动等），性能远超 ansi-term。
;;
;; 依赖: 首次加载时自动编译 C 模块，系统需要安装 cmake 和 GNU libtool
;;   macOS:  brew install cmake libtool  (安装为 glibtool，系统自带的 Apple libtool 不兼容)
;;   Ubuntu: sudo apt install cmake libtool-bin
;;
;; 快捷键：
;;   C-j v v — 打开 vterm（新建终端）
;;   C-j v t — 打开 vterm（切换到已有终端或新建）
;;   C-j v o — 在另一个窗口打开 vterm

;;; Code:

;; ============================================================================
;; vterm — 高性能终端模拟器
;; ============================================================================

(use-package vterm
  :ensure t
  :defer t
  :bind
  (("C-j v v" . vterm)                            ; 新建终端
   ("C-j v t" . skemacs/vterm-toggle)             ; 切换/新建终端
   ("C-j v o" . vterm-other-window))              ; 在另一窗口打开
  :init
  ;; which-key 描述
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-j v"   "Terminal"
      "C-j v v" "New vterm"
      "C-j v t" "Toggle vterm"
      "C-j v o" "Vterm other window"))

  ;; 快速切换 vterm buffer 的辅助函数
  (defun skemacs/vterm-toggle ()
    "切换到已有的 vterm buffer，若不存在则新建。"
    (interactive)
    (if (eq major-mode 'vterm-mode)
        ;; 已在 vterm 中，回到上一个 buffer
        (switch-to-prev-buffer)
      ;; 查找已有的 vterm buffer
      (let ((vterm-buf (seq-find (lambda (buf)
                                   (with-current-buffer buf
                                     (eq major-mode 'vterm-mode)))
                                 (buffer-list))))
        (if vterm-buf
            (switch-to-buffer vterm-buf)
          (vterm)))))

  :config
  ;; ── 基础配置 ──────────────────────────────────────────────
  ;; 终端最大回滚行数
  (setq vterm-max-scrollback 10000)

  ;; 关闭终端进程退出后自动关闭 buffer
  (setq vterm-kill-buffer-on-exit t)

  ;; 使用系统默认 shell
  (setq vterm-shell (or (getenv "SHELL") "/bin/zsh"))

  ;; buffer 命名规则
  (setq vterm-buffer-name-string "vterm: %s"))

(provide 'init-vterm)
;;; init-vterm.el ends here
