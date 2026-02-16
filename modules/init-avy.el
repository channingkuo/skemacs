;;; init-avy.el --- Avy 快速跳转 -*- lexical-binding: t -*-
;;; Commentary:

;; 使用 avy 实现可视范围内的快速跳转，通过输入少量字符即可将光标移动到
;; 屏幕上的任意位置。avy 会在候选位置显示提示字符，按下对应字符即可跳转。
;;
;; 快捷键：
;;   C-;     — 跳转到任意单词开头（avy-goto-word-1）
;;   C-'     — 跳转到任意字符（avy-goto-char-2）
;;   M-g g   — 跳转到任意行（avy-goto-line）
;;   M-g w   — 跳转到任意单词（avy-goto-word-1）
;;   C-j j c — 跳转到任意字符（avy-goto-char）
;;   C-j j w — 跳转到任意单词开头（avy-goto-word-1）
;;   C-j j l — 跳转到任意行（avy-goto-line）
;;   C-j j t — 跳转到任意字符定时搜索（avy-goto-char-timer）

;;; Code:

;; ============================================================================
;; avy：可视范围快速跳转
;; ============================================================================

(use-package avy
  :ensure t
  :defer t
  :bind
  (("C-;"     . avy-goto-word-1)          ; 输入 1 个字符跳转到单词开头
   ("C-'"     . avy-goto-char-2)          ; 输入 2 个字符跳转
   ("M-g g"   . avy-goto-line)            ; 跳转到任意行
   ("M-g w"   . avy-goto-word-1)          ; 跳转到单词开头
   ("C-j j c" . avy-goto-char)            ; 跳转到任意字符
   ("C-j j w" . avy-goto-word-1)          ; 跳转到单词开头
   ("C-j j l" . avy-goto-line)            ; 跳转到任意行
   ("C-j j t" . avy-goto-char-timer))     ; 定时字符搜索跳转
  :init
  ;; which-key 描述
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-j j"   "Jump (Avy)"
      "C-j j c" "Goto char"
      "C-j j w" "Goto word"
      "C-j j l" "Goto line"
      "C-j j t" "Goto char timer"))
  :config
  ;; 使用 home row 键作为提示字符，方便快速按下
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  ;; 搜索范围：所有可见窗口
  (setq avy-all-windows t)

  ;; 提示样式：在候选位置覆盖原字符
  (setq avy-style 'at-full)

  ;; 背景变暗以突出提示字符
  (setq avy-background t)

  ;; avy-goto-char-timer 的等待时间（秒），超时后自动触发跳转
  (setq avy-timeout-seconds 0.8))

(provide 'init-avy)
;;; init-avy.el ends here
