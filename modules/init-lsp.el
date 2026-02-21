;;; init-lsp.el --- LSP configuration with lsp-bridge -*- lexical-binding: t -*-
;;; Commentary:

;; lsp-bridge for LSP support with embedded Node.js and Python.
;; Node.js v22 LTS at ~/.emacs.d/nodejs/ (LSP servers installed there).
;; Python venv at ~/.emacs.d/.venv/ (lsp-bridge async backend).
;; Uses Volar + vtsls multiserver for Vue 3 + TypeScript.
;;
;; 首次使用前请运行: bash setup-lsp.sh

;;; Code:

;; ============================================================================
;; 内置 Node.js: 将 nodejs/bin 加入 exec-path 和 PATH
;; ============================================================================

(let ((node-bin (expand-file-name "nodejs/bin" user-emacs-directory)))
  (when (file-directory-p node-bin)
    (add-to-list 'exec-path node-bin)
    (setenv "PATH" (concat node-bin ":" (getenv "PATH"))))
  (message "[init-lsp] Node.js 路径: %s (存在: %s)" node-bin (file-directory-p node-bin)))

;; ============================================================================
;; lsp-bridge 依赖
;; ============================================================================
;;
;; ── yasnippet 快捷键 ────────────────────────────────────────
;;
;;   TAB / C-i         展开 snippet 或跳到下一个占位符
;;   S-TAB             跳到上一个占位符
;;   C-c & C-s         yas-insert-snippet     交互式插入 snippet
;;   C-c & C-n         yas-new-snippet        创建新 snippet
;;   C-c & C-v         yas-visit-snippet-file 编辑已有 snippet
;;

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  (message "[init-lsp] >>> yasnippet :config 执行中 (yas-reload-all)...")
  (yas-reload-all)
  (message "[init-lsp] >>> yasnippet :config 完成"))
(message "[init-lsp] >>> yasnippet use-package 完成")

;; ── markdown-mode 快捷键 ────────────────────────────────────
;;
;;   C-c C-s b         粗体 (**bold**)
;;   C-c C-s i         斜体 (*italic*)
;;   C-c C-s c         行内代码 (`code`)
;;   C-c C-s k         添加 <kbd> 标签
;;   C-c C-s q         引用块 (blockquote)
;;   C-c C-s p         预格式化文本 (pre)
;;   C-c C-s C         代码块 (code block)
;;   C-c C-t 1~6       插入 1~6 级标题
;;   C-c C-t h         自动插入标题 (按层级递增)
;;   C-c C-a l         插入链接 [text](url)
;;   C-c C-a u         插入 URL 链接
;;   C-c C-a f         插入脚注
;;   C-c C-i i         插入图片 ![alt](url)
;;   C-c C-l           插入/编辑链接
;;   C-c C-x C-l       切换隐藏 URL / 显示标记语法 (toggle-markup-hiding)
;;   C-c C-c l         markdown-live-preview-mode
;;   C-c C-c p         预览 (markdown-preview)
;;   C-c C-c e         导出 (markdown-export)
;;   TAB / S-TAB       折叠/展开标题下内容
;;   M-RET             新建同级列表项
;;   C-c C-n / C-c C-p 在标题间向前/向后跳转
;;   C-c <up/down>     移动列表项上/下

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package posframe
  :ensure t)

;; ============================================================================
;; lsp-bridge (via git clone at ~/.emacs.d/lsp-bridge/)
;; ============================================================================

;; ============================================================================
;; 按键
;; 按键	命令	备注
;; Alt + n	acm-select-next	选择下一个候选词
;; Down	acm-select-next	选择下一个候选词
;; Alt + p	acm-select-prev	选择上一个候选词
;; Up	acm-select-prev	选择上一个候选词
;; Alt + ,	acm-select-last	选择最后一个候选词
;; Alt + .	acm-select-first	选择第一个候选词
;; Ctrl + v	acm-select-next-page	向下滚动候选菜单
;; Alt + v	acm-select-prev-page	向上滚动候选菜单
;; Ctrl + m	acm-complete	完成补全
;; Return	acm-complete	完成补全
;; Tab	acm-complete	完成补全
;; Alt + h	acm-complete	完成补全
;; Alt + H	acm-insert-common	插入候选词共有部分
;; Alt + u	acm-filter	对候选词做二次过滤， 类似其他补全前端的模糊搜索
;; Alt + d	acm-doc-toggle	开启或关闭候选词文档
;; Alt + j	acm-doc-scroll-up	向下滚动候选词文档
;; Alt + k	acm-doc-scroll-down	向上滚动候选词文档
;; Alt + l	acm-hide	隐藏补全窗口
;; Ctrl + g	acm-hide	隐藏补全窗口
;; Alt + 数字键	acm-complete-quick-access	快速选择候选词， 需要开启 acm-enable-quick-access 选项
;; 数字键	acm-complete-quick-access	(更加)快速选择候选词， 需要同时开启 acm-enable-quick-access 和 acm-quick-access-use-number-select
;; 命令
;; lsp-bridge-find-def: 跳转到定义位置
;; lsp-bridge-find-def-other-window: 在其他窗口跳转到定义位置
;; lsp-bridge-find-def-return: 返回跳转之前的位置
;; lsp-bridge-find-impl: 跳转到接口实现位置
;; lsp-bridge-find-impl-other-window: 在其他窗口跳转到接口实现位置
;; lsp-bridge-find-type-def: 跳转到类型定义位置
;; lsp-bridge-find-type-def-other-window: 在其他窗口跳转到类型定义位置
;; lsp-bridge-find-references: 查看代码引用
;; lsp-bridge-popup-documentation: 查看光标处的文档
;; lsp-bridge-popup-documentation-scroll-up: 文档窗口向上滚动
;; lsp-bridge-popup-documentation-scroll-down: 文档窗口向下滚动
;; lsp-bridge-show-documentation: 查看光标处的文档, 但是是用 Buffer 来显示
;; lsp-bridge-rename: 重命名
;; lsp-bridge-diagnostic-jump-next: 跳转到下一个诊断位置
;; lsp-bridge-diagnostic-jump-prev: 跳转到上一个诊断位置
;; lsp-bridge-diagnostic-list: 列出所有诊断信息
;; lsp-bridge-diagnostic-copy: 拷贝当前诊断信息到剪切板
;; lsp-bridge-code-action: 弹出代码修复菜单, 也可以指需要修复的代码动作类型: "quickfix", "refactor", "refactor.extract", "refactor.inline", "refactor.rewrite", "source", "source.organizeImports", "source.fixAll"
;; lsp-bridge-workspace-list-symbol-at-point: 查找光标下符号的定义
;; lsp-bridge-workspace-list-symbols: 列出工作区所有符号， 并跳转到符号定义
;; lsp-bridge-signature-help-fetch: 在 minibuffer 显示参数信息
;; lsp-bridge-popup-complete-menu: 手动弹出补全菜单， 只有当打开 lsp-bridge-complete-manually 选项才需要使用这个命令
;; lsp-bridge-restart-process: 重启 lsp-bridge 进程 (一般只有开发者才需要这个功能)
;; lsp-bridge-toggle-sdcv-helper: 切换字典助手补全
;; lsp-bridge-peek: 在 peek window 中展示光标处的定义和引用
;; lsp-bridge-peek-abort: 关闭 peek window (默认绑定到 C-g)
;; lsp-bridge-peek-list-next-line: 选择下一个定义或引用 (默认绑定到 M-S-n )
;; lsp-bridge-peek-list-prev-line: 选择上一个定义或引用 (默认绑定到 M-S-p )
;; lsp-bridge-peek-file-content-next-line: 将 peek window 中的文件内容向下滚动一行 (默认绑定到 M-n )
;; lsp-bridge-peek-file-content-prev-line: 将 peek window 中的文件内容向上滚动一行 (默认绑定到 M-p )
;; lsp-bridge-peek-jump: 跳转到定义或引用所在处 (默认绑定到 M-l j )
;; lsp-bridge-peek-jump-back: 跳转到原来的位置 (默认绑定到 M-l b )
;; lsp-bridge-peek-through: 选择 peek window 中的一个符号进行查看
;; lsp-bridge-peek-tree-previous-branch: 选择上一个浏览历史上同级的分支 (默认绑定到 <up> )
;; lsp-bridge-peek-tree-next-branch: 选择下一个浏览历史上同级的分支 (默认绑定到 <down> )
;; lsp-bridge-peek-tree-previous-node: 选择浏览历史上一级节点 (默认绑定到 <left> )
;; lsp-bridge-peek-tree-next-node: 选择浏览历史上下一级节点 (默认绑定到 <right> )
;; lsp-bridge-indent-left: 根据 lsp-bridge-formatting-indent-alist 定义的缩进值, 向左缩进刚刚粘贴的文本
;; lsp-bridge-indent-right: 根据 lsp-bridge-formatting-indent-alist 定义的缩进值, 向右缩进刚刚粘贴的文本
;; lsp-bridge-semantic-tokens-mode: 开启或者关闭语义符号高亮， 自定义请参考 Semantic Tokens Wiki
;; lsp-bridge-breadcrumb-mode: 开启顶部 breadcrumb 栏
;; ============================================================================

(message "[init-lsp] >>> 加载 lsp-bridge... (目录存在: %s)"
         (file-directory-p (expand-file-name "lsp-bridge" user-emacs-directory)))
(use-package lsp-bridge
  :load-path "lsp-bridge"
  :hook
  ((web-mode . lsp-bridge-mode)
   (typescript-mode . lsp-bridge-mode)
   (js-mode . lsp-bridge-mode))
  :bind
  (("M-." . lsp-bridge-find-def)
   ("M-," . lsp-bridge-find-def-return)
   ("M-?" . lsp-bridge-find-references)
   :map lsp-bridge-mode-map
   ;; --- 跳转导航 ---
   ("C-j l f" . lsp-bridge-find-def)
   ("C-j l F" . lsp-bridge-find-def-other-window)
   ("C-j l i" . lsp-bridge-find-impl)
   ("C-j l I" . lsp-bridge-find-impl-other-window)
   ("C-j l t" . lsp-bridge-find-type-def)
   ("C-j l T" . lsp-bridge-find-type-def-other-window)
   ;; --- 重构 ---
   ("C-j l r" . lsp-bridge-rename)
   ("C-j l a" . lsp-bridge-code-action)
   ;; --- 文档 ---
   ("C-j l d" . lsp-bridge-show-documentation)
   ("C-j l h" . lsp-bridge-popup-documentation)
   ("C-j l s" . lsp-bridge-signature-help-fetch)
   ;; --- 诊断 ---
   ("C-j l e" . lsp-bridge-diagnostic-list)
   ("C-j l n" . lsp-bridge-diagnostic-jump-next)
   ("C-j l p" . lsp-bridge-diagnostic-jump-prev)
   ("C-j l c" . lsp-bridge-diagnostic-copy)
   ;; --- 符号 ---
   ("C-j l S" . lsp-bridge-workspace-list-symbols)
   ("C-j l o" . lsp-bridge-workspace-list-symbol-at-point)
   ;; --- Peek ---
   ("C-j l k" . lsp-bridge-peek)
   ;; --- 其他 ---
   ("C-j l R" . lsp-bridge-restart-process))
  :init
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-j l"   "LSP"                ;; 语言服务协议
      "C-j l f" "Find Def"           ;; 跳转到定义
      "C-j l F" "Find Def ↗"         ;; 跳转到定义（新窗口）
      "C-j l i" "Find Impl"          ;; 跳转到实现
      "C-j l I" "Find Impl ↗"        ;; 跳转到实现（新窗口）
      "C-j l t" "Find Type Def"      ;; 跳转到类型定义
      "C-j l T" "Find Type Def ↗"    ;; 跳转到类型定义（新窗口）
      "C-j l r" "Rename"             ;; 重命名
      "C-j l a" "Code Action"        ;; 代码操作
      "C-j l d" "Doc (buffer)"       ;; 文档（缓冲区）
      "C-j l h" "Doc (popup)"        ;; 文档（弹窗）
      "C-j l s" "Signature Help"     ;; 签名帮助
      "C-j l e" "Diagnostic List"    ;; 诊断列表
      "C-j l n" "Diag Next"          ;; 下一个诊断
      "C-j l p" "Diag Prev"          ;; 上一个诊断
      "C-j l c" "Diag Copy"          ;; 复制诊断信息
      "C-j l S" "List Symbols"       ;; 列出符号
      "C-j l o" "Symbol at Point"    ;; 光标处符号
      "C-j l k" "Peek"               ;; 预览
      "C-j l R" "Restart LSP"))      ;; 重启 LSP
  :config
  ;; Python: 使用内置 venv
  (setq lsp-bridge-python-command
        (expand-file-name ".venv/bin/python" user-emacs-directory))
  ;; 用户自定义 langserver 目录（vtsls.json 路径覆盖）
  (setq lsp-bridge-user-langserver-dir
        (expand-file-name "lsp-bridge-langserver" user-emacs-directory))
  ;; Vue 文件: 使用 Volar + vtsls 多服务器融合
  (setq lsp-bridge-multi-lang-server-extension-list
        '((("vue") . "volar_vtsls")))
  ;; 启用诊断提示
  (setq lsp-bridge-enable-diagnostics t)
  (setq lsp-bridge-enable-hover-diagnostic t)  ; 启用悬浮诊断
  (setq acm-enable-doc t)                      ; 启用补全文档
  (setq acm-enable-doc-markdown-render t)      ; 渲染 Markdown

  ;; 自定义 posframe 样式
  (setq acm-frame-background-dark-color "#191a1b")
  (setq acm-frame-background-light-color "#f0f0f0")

  ;; 文档窗口位置
  ;; (setq acm-doc-frame-pos 'top)  ; 或 'bottom

  ;; 边框样式(需要 Emacs 29+)
  ;; (set-face-attribute 'child-frame-border nil 
  ;;                   :background "#5B6268")
  )

(provide 'init-lsp)
;;; init-lsp.el ends here
