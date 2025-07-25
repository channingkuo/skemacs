;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:
;; 记录启动时间
(defvar skemacs-start-time (current-time)
  "Time when Emacs started loading the configuration.")

(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 400 1024 1024))
      (init-gc-cons-threshold (* 800 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; 定义自定义配置目录
;; 这个目录用于存放个人的 Emacs Lisp 文件
(defconst skemacs-config-dir (expand-file-name "skemacs" user-emacs-directory)
  "Custom configuration entry directory for personal Emacs Lisp files.")
;; 库的配置文件目录
(defconst packages-config-dir (expand-file-name "packages" user-emacs-directory)
  "Custom configuration directory for Emacs packages.")
;; 自定义配置文件目录
(defconst local-packages-config-dir (expand-file-name "local" user-emacs-directory)
  "Custom local configuration directory for Emacs packages.")
(defconst banners-config-dir (expand-file-name "banners" user-emacs-directory)
  "Custom local configuration directory for Emacs banners.")

;; 将自定义配置目录添加到 load-path
(unless (member skemacs-config-dir load-path)
  (add-to-list 'load-path skemacs-config-dir))

;; Banner配置
(defconst skemacs-banner-file 112 "Default banner file.")

(defun skemacs/get-banner-path ()
  "Get the complete path to the banner file."
  (expand-file-name (format "%d-banner.txt" skemacs-banner-file) banners-config-dir))

;; 立即显示启动信息和banner
(defun skemacs/show-startup-banner ()
  "Display startup banner immediately."
  (let ((banner-file (skemacs/get-banner-path)))
    (when (file-exists-p banner-file)
      (with-current-buffer (get-buffer-create "*skemacs-startup*")
        (erase-buffer)
        ;; 读取banner内容并居中显示
        (let ((banner-content (with-temp-buffer
                                (insert-file-contents banner-file)
                                (buffer-string)))
              (window-width (window-width)))
          ;; 分割banner内容为行
          (let ((lines (split-string banner-content "\n")))
            (dolist (line lines)
              (let* ((line-length (length line))
                     (padding (max 0 (/ (- window-width line-length) 2))))
                (insert (make-string padding ? ) line "\n")))))
        (goto-char (point-max))
        (insert "Indexing layers...\n")
        (switch-to-buffer (current-buffer))
        (goto-char (point-min))
        (read-only-mode 1)))))

;; 在加载配置前显示banner
(skemacs/show-startup-banner)

(defvar skemacs-startup-type nil "Emacs启动类型")
;; 获取文件参数（排除选项）
(defun skemacs-get-file-args ()
  "获取非选项的文件/目录参数"
  (let ((args (cdr command-line-args))
        (file-args '())
        (skip-next nil))
    (while args
      (let ((arg (car args)))
        (cond
         (skip-next (setq skip-next nil))
         ((string-prefix-p "--" arg)
          (when (member arg '("--load" "--funcall" "--eval" "--user" 
                             "--daemon" "--bg-daemon" "--chdir"))
            (setq skip-next t)))
         ((string-prefix-p "-" arg)
          (when (string-match-p "[lfeut]" (substring arg 1))
            (setq skip-next t)))
         (t (push arg file-args))))
      (setq args (cdr args)))
    (reverse file-args)))

;; 检测启动方式
(defun skemacs-detect-startup-type ()
  "检测Emacs启动方式"
  (let ((file-args (skemacs-get-file-args)))
    (cond
     (file-args
      (let ((arg (car file-args)))
        (cond
         ((string= arg ".")
          (setq skemacs-startup-type 'with-dot))
         ((and (file-exists-p arg) (file-directory-p arg))
          (setq skemacs-startup-type 'with-folder))
         ((and (file-exists-p arg) (file-regular-p arg))
          (setq skemacs-startup-type 'with-file))
         (t
          (setq skemacs-startup-type 'with-new-file)))))
     (t
      (setq skemacs-startup-type 'plain)))))
;; 检测启动类型
(skemacs-detect-startup-type)

;; 更新banner显示的函数
(defun skemacs/update-banner-status (message &optional close)
  "Update the banner buffer with loading status."
  (when (get-buffer "*skemacs-startup*")
    (with-current-buffer "*skemacs-startup*"
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert message "\n")
        ;; 检查是否需要滚动
        (let ((window (get-buffer-window (current-buffer))))
          (when window
            (with-selected-window window
              (let ((total-lines (count-lines (point-min) (point-max)))
                    (window-height (window-height)))
                (if (> total-lines window-height)
                    ;; 如果内容超出屏幕高度，滚动到底部
                    (progn
                      (goto-char (point-max))
                      (recenter -1))
                  ;; 否则保持在顶部
                  (goto-char (point-min)))))))
        (redisplay t))))
  (when close
    ;; 延迟关闭启动buffer并显示dashboard
    (run-with-timer 1 nil (lambda ()
                            (when (get-buffer "*skemacs-startup*")
                              (kill-buffer "*skemacs-startup*"))
                            (pcase skemacs-startup-type
                              ('with-dot ;; do nothing
                              )
                              ('with-folder ;; do nothing
                              )
                              ('with-file ;; do nothing
                              )
                              ('with-new-file ;; do nothing
                              )
                              (_ ;; plain or other types
                              ;; 确保显示dashboard
                              (let ((dashboard-buffer (get-buffer "*skemacs*")))
                              (if dashboard-buffer
                                  ;; 如果dashboard buffer存在，切换到它
                                  (switch-to-buffer dashboard-buffer)
                                ;; 否则创建并显示dashboard
                                (when (fboundp 'dashboard-open)
                                  (dashboard-open))
                                ;; 如果dashboard-open不存在，尝试手动创建
                                (unless (get-buffer "*skemacs*")
                                  (with-current-buffer (get-buffer-create "*skemacs*")
                                    (dashboard-mode)
                                    (dashboard-insert-startupify-lists)
                                    (switch-to-buffer (current-buffer))))))))))))


;; 如果skemacs-startup-type不是with-file和with-new-file，则加载所有的配置文件
(unless (memq skemacs-startup-type '(with-file with-new-file))
  ;; 加载skemacs-config-dir该目录下所有以 .el 结尾的文件
  (when (file-directory-p skemacs-config-dir)
    (let* ((config-files (directory-files skemacs-config-dir t "\\.el$" t)))
      (let* ((sorted-config-files (sort config-files 'string<)))
      (dolist (file sorted-config-files)
        (let ((time1 (current-time))
              (file-name (file-name-nondirectory file)))
          (skemacs/update-banner-status (format "Loading  %s..." file-name))
          (load file nil t)
          (let ((time2 (current-time)))
            (skemacs/update-banner-status (format "Loaded   %s in %.6f seconds" 
                                                  file-name
                                                  (float-time (time-subtract time2 time1))))))))))
  )

;; 设置自定义配置文件位置
(setq custom-file (expand-file-name ".skemacs" user-emacs-directory))
(when (file-exists-p custom-file)
  (let ((time1 (current-time)))
    (load custom-file nil t)
    (let ((time2 (current-time)))
      (skemacs/update-banner-status (format "Loaded   .skemacs in %.6f seconds"
                                            (float-time (time-subtract time2 time1)))))))

;; 如果skemacs-startup-type是with-file或with-new-file，单独加载002-config.el
(when (memq skemacs-startup-type '(with-file with-new-file))
  (let ((config-file (expand-file-name "002-config.el" skemacs-config-dir)))
    (when (file-exists-p config-file)
      (let ((time1 (current-time)))
        (skemacs/update-banner-status "Loading  002-config.el...")
        (load config-file nil t)
        (let ((time2 (current-time)))
          (skemacs/update-banner-status (format "Loaded   002-config.el in %.6f seconds"
                                                (float-time (time-subtract time2 time1)))))))))

;; 启动完成hook
(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((elapsed-time (float-time (time-subtract (current-time) skemacs-start-time))))
              (skemacs/update-banner-status (format "Emacs configuration loaded in %.6f seconds" elapsed-time))
              (skemacs/update-banner-status "Ready!" t)
              ;; 显示启动时间
              (message "Emacs configuration loaded in %.6f seconds" elapsed-time))))
;; C-x a i R重新加载配置后hook
(defadvice load-file (after skemacs/reload-config activate)
    "Hook that runs after load-file completes"
    (when (string-match-p "\\init.el$" (ad-get-arg 0))
                (let ((elapsed-time (float-time (time-subtract (current-time) skemacs-start-time))))
                  (skemacs/update-banner-status (format "Emacs configuration loaded in %.6f seconds" elapsed-time))
                  (skemacs/update-banner-status "Ready!" t))))
(provide 'init)