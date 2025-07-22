;; https://emacs-lsp.github.io/lsp-mode/
;; https://zhuanlan.zhihu.com/p/467681146
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l" lsp-file-watch-threshold 500)
  :hook 
  ;; Frontend modes
  (js-mode . lsp-deferred)
  (js2-mode . lsp-deferred)
  (typescript-mode . lsp-deferred)
  (typescript-ts-mode . lsp-deferred)
  (js-ts-mode . lsp-deferred)
  (tsx-ts-mode . lsp-deferred)
  (web-mode . lsp-deferred)
  (css-mode . lsp-deferred)
  (scss-mode . lsp-deferred)
  (less-css-mode . lsp-deferred)
  ;; which-key integration
  (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred)
  :config
  ;; 保持 LSP 补全启用，同时确保 yasnippet 集成
  ;; LSP 补全 + yasnippet, 参考 yasnippet.el 与 company.el 配置
  (setq lsp-completion-provider :none)
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; 防止递归调用，但保持核心功能
  (setq lsp-enable-file-watchers nil)      ; 禁用文件监视（防止递归）
  (setq lsp-enable-folding t)              ; 保持代码折叠
  (setq lsp-enable-links t)                ; 保持代码链接
  (setq lsp-enable-snippet t)              ; 保持代码片段
  (setq lsp-diagnostic-package :flycheck)  ; 使用 flycheck 做错误检查
  (setq lsp-eldoc-enable-hover nil)        ; 禁用悬停文档（减少递归风险）
  (setq lsp-signature-render-documentation nil) ; 禁用签名文档
  (setq lsp-java-java-path "/usr/local/Cellar/openjdk@17/17.0.14/libexec/openjdk.jdk/Contents/Home/bin/java")
  (setq lombok-jar-path (expand-file-name "/Users/kuo/lombok.jar"))
  (setq lsp-java-vmargs `("-Xmx2G"  ; 增加到2GB堆内存
                          "-Xms512m"  ; 设置初始堆内存
                          "-XX:+UseG1GC"  ; 使用G1垃圾回收器
                          "-XX:MaxGCPauseMillis=200"  ; 最大GC停顿时间
                          "-Dsun.zip.disableMemoryMapping=true"
                          "-Dfile.encoding=UTF-8"
                          ,(concat "-javaagent:" lombok-jar-path)
                          ))
  ;; Frontend LSP servers configuration
  (with-eval-after-load 'lsp-mode
    ;; CSS LSP server
    (setq lsp-css-validate t)
    (setq lsp-scss-validate t)
    (setq lsp-less-validate t)
    ;; Configure LSP to use Prettier for formatting
    (setq lsp-javascript-format-enable nil)  ; 禁用内置格式化
    (setq lsp-typescript-format-enable nil)  ; 禁用内置格式化
    ;; 设置 Prettier 作为格式化器
    (setq lsp-javascript-preferences-use-prettier t)
    (setq lsp-typescript-preferences-use-prettier t))
  :bind
  ;; 可快速搜索工作区内的符号（类名、函数名、变量名等）
  ("C-c l s" . lsp-ivy-workspace-symbol))

;; 图形化的支持
(use-package lsp-ui
  :ensure t
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-position 'top))

;; 利用 ivy 辅助 lsp
(use-package lsp-ivy
  :ensure t
  :after (lsp-mode))