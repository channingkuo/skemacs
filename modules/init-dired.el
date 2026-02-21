;;; init-dired.el --- Dired 增强配置 -*- lexical-binding: t -*-
;;; Commentary:

;; 增强 Dired 文件管理器：
;; - 使用 dired-omit-mode 过滤不需要显示的文件（如 .DS_Store）
;; - 可通过 C-x M-o 在 dired buffer 中临时切换显示/隐藏
;;
;; ── Dired 常用操作快捷键速查 ─────────────────────────────────
;;
;; 导航：
;;   n / p             下/上移动一行
;;   j                 跳转到指定文件（输入文件名）
;;   ^                 返回上级目录
;;   RET / o / C-o     打开文件 / 在另一窗口打开 / 在另一窗口打开但不切换
;;   i                 在当前 buffer 中展开子目录
;;   $                 折叠/展开子目录
;;
;; 标记：
;;   m / u             标记 / 取消标记当前文件
;;   U                 取消所有标记
;;   t                 反转所有标记
;;   * /               标记所有目录
;;   * s               标记所有文件（不含 . 和 ..）
;;   % m               按正则表达式标记文件名
;;
;; 文件操作（大写字母，作用于标记的文件或光标所在文件）：
;;   C                 复制
;;   R                 重命名 / 移动
;;   D                 删除
;;   S                 创建符号链接
;;   H                 创建硬链接
;;   Z                 压缩 / 解压
;;   M                 修改权限 (chmod)
;;   O                 修改所有者 (chown)
;;   G                 修改所属组 (chgrp)
;;   T                 修改时间戳 (touch)
;;
;; 创建与编辑：
;;   +                 创建新目录
;;   C-x C-f           在当前目录创建/打开文件
;;   w                 复制文件名到 kill-ring
;;   0 w               复制文件完整路径到 kill-ring
;;
;; 显示与排序：
;;   (                 切换显示详细信息 / 简洁模式
;;   s                 切换按名称 / 日期排序
;;   C-u s             自定义 ls 参数排序
;;   g                 刷新当前目录
;;   l                 刷新指定文件行
;;   k                 从显示中隐藏行（不删除文件）
;;
;; 标记后的批量操作：
;;   ! / &             对标记文件执行 shell 命令 (同步 / 异步)
;;   % R / % C         按正则批量重命名 / 复制
;;   A                 在标记文件中搜索 (regexp)
;;   Q                 在标记文件中查找替换 (regexp)
;;
;; Wdired（可编辑模式）：
;;   C-x C-q           进入 wdired 模式（直接编辑文件名）
;;   C-c C-c           确认修改
;;   C-c C-k           放弃修改

;;; Code:

;; ============================================================================
;; dired-x：过滤隐藏不需要的文件
;; ============================================================================

(use-package dired
  :ensure nil                             ; 内置包
  :hook
  ;; 进入 dired 时自动启用 omit 模式
  (dired-mode . dired-omit-mode)
  :config
  (require 'dired-x)

  ;; ── 要隐藏的文件匹配规则（正则表达式）──────────────────────
  ;; 默认值会匹配以 . 开头的隐藏文件，这里改为只匹配特定的无用文件
  (setq dired-omit-files
        (concat "\\(?:"
                "\\.DS_Store"               ; macOS 目录元数据
                "\\|Thumbs\\.db"            ; Windows 缩略图缓存
                "\\|desktop\\.ini"          ; Windows 桌面配置
                "\\|__MACOSX"              ; macOS 压缩包产生的垃圾目录
                "\\)"))

  ;; 是否同时按扩展名过滤（如 .elc .pyc 等编译产物）
  ;; 取消注释下面的行来启用
  ;; (setq dired-omit-extensions
  ;;       (append dired-omit-extensions '(".elc" ".pyc" ".pyo" ".o")))

  ;; 隐藏 omit 模式的 modeline 提示，减少视觉干扰
  (setq dired-omit-verbose nil)

  ;; macOS 的 BSD ls 不支持 --dired 和 -N，禁用以避免警告
  (when (eq system-type 'darwin)
    (setq dired-use-ls-dired nil))

  ;; ── 其他实用 dired 设置 ─────────────────────────────────────
  ;; 两个 dired 窗口时，操作（复制/移动）默认目标为另一个窗口的目录
  ;; (setq dired-dwim-target t)
  ;; 递归复制和删除目录时不反复确认
  (setq dired-recursive-copies 'always)
  ;; (setq dired-recursive-deletes 'always)
  )

(provide 'init-dired)
;;; init-dired.el ends here
