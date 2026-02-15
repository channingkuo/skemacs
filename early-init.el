;;; early-init.el --- 早期初始化配置 -*- lexical-binding: t -*-
;;; Commentary:

;; Emacs 27+ 在 init.el 之前、GUI 初始化之前加载此文件。
;; 用于 GC 优化、原生编译设置和禁用 package.el 自动加载。

;;; Code:

;; GC 优化：启动期间提高阈值，减少 GC 暂停
(setq gc-cons-threshold (* 800 1024 1024))
(setq gc-cons-percentage 0.6)

;; 禁止 package.el 在 init.el 前自动加载已安装的包
;; core-packages.el 通过 advice 延迟初始化，首次安装包时才执行 package-initialize
(setq package-enable-at-startup nil)

;; 原生编译优化（Emacs 28+）
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-deferred-compilation t))

;; 禁止在启动时调整 frame 大小（加速启动）
(setq frame-inhibit-implied-resize t)

;; 避免在早期初始化阶段加载不必要的 UI 元素
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; 终端模式下也需要显式禁用菜单栏
(menu-bar-mode -1)

(provide 'early-init)
;;; early-init.el ends here
