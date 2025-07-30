;;; 110-java.el --- Java development environment with LSP -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration provides comprehensive Java development support
;; through lsp-java. Includes optimized JVM settings, code formatting
;; with IntelliJ IDEA style, Lombok support, and project integration
;; for both Maven and Gradle projects.

;;; Code:

(use-package lsp-java
  :ensure t
  :mode ("\\.java\\'")
  ;; :hook (java-mode . lsp)
  :config
	;; 禁用缩进制表符模式
  (indent-tabs-mode -1)
  ;; 确保正确的项目检测
  (setq lsp-java-workspace-dir (expand-file-name ".cache/lsp-java-workspace/" user-emacs-directory))
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
  ;; 启用项目导入
  (setq lsp-java-import-gradle-enabled t)
  (setq lsp-java-import-maven-enabled t)
  ;; 自动猜测项目根目录
  (setq lsp-auto-guess-root t)
  ;; 减少文件处理以防止递归，但保持必要功能
  (setq lsp-java-max-concurrent-builds 1)
  (setq lsp-java-references-code-lens-enabled t)      ; 重新启用引用计数
  (setq lsp-java-implementations-code-lens-enabled t) ; 重新启用实现计数
  ;; 添加大项目优化
  (setq lsp-java-format-enabled t)                    ; 启用代码格式化
  (setq lsp-java-format-settings-url "/Users/kuo/intellij-idea-java-formatter.xml")  ; 格式化规则文件
  (setq lsp-java-format-settings-profile "IntelliJ IDEA Java Code Style")            ; 格式化配置文件名
  (setq lsp-java-save-actions-organize-imports nil)   ; 保存时整理imports
  (setq lsp-java-completion-max-results 100)          ; 限制补全结果数量
  (setq lsp-java-progress-reports-enabled nil))       ; 禁用进度报告（减少噪音）

;;; 110-java.el ends here
