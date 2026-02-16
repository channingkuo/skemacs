;;; core-module.el --- 模块加载系统 -*- lexical-binding: t -*-
;;; Commentary:

;; 提供模块加载基础设施：
;; - 自动扫描 modules/ 目录或按指定列表加载
;; - 每个模块加载时自动计时，实时显示在启动 splash 画面中
;; - condition-case 错误隔离，单个模块出错不影响其他模块
;; - 可通过 skemacs-disabled-modules 禁用特定模块
;; - M-x skemacs/show-load-times 查看格式化的计时报告

;;; Code:

;; ============================================================================
;; 用户可配置变量
;; ============================================================================

(defvar skemacs-module-list nil
  "要加载的模块列表（不含 .el 后缀）。
设为 nil 时自动扫描 `skemacs-modules-dir' 下所有 .el 文件。
示例: \\='(\"init-theme\" \"init-which-key\" \"init-completion\")")

(defvar skemacs-disabled-modules nil
  "要禁用的模块列表（不含 .el 后缀）。
优先级高于 `skemacs-module-list' 和自动扫描。
示例: \\='(\"init-vterm\" \"init-treemacs\")")

(defvar skemacs-banner-file 112
  "启动 banner 编号。设为 nil 禁用 banner。")

;; ============================================================================
;; 内部状态
;; ============================================================================

(defvar skemacs-load-times nil
  "模块加载计时数据。
格式: ((\"模块名\" . 耗时秒数) ...) ，按加载顺序排列。")

(defvar skemacs-module-errors nil
  "模块加载错误记录。
格式: ((\"模块名\" . \"错误信息\") ...)")

(defvar skemacs-modules-loaded 0
  "成功加载的模块数量。")

(defvar skemacs-modules-disabled 0
  "被禁用的模块数量。")

(defvar skemacs--splash-buffer nil
  "启动 splash buffer 对象。加载期间用于实时更新。")

;; ============================================================================
;; 启动 Splash 画面（动态更新）
;; ============================================================================

(defun skemacs--center-string (str width)
  "将字符串 STR 居中到 WIDTH 宽度。"
  (let* ((len (length str))
         (padding (max 0 (/ (- width len) 2))))
    (concat (make-string padding ?\s) str)))

(defun skemacs--splash-init ()
  "创建启动 splash buffer，显示 banner 和表头。
在任何模块加载之前调用。"
  (let ((buf (get-buffer-create "*skemacs*"))
        (win-width (window-width)))
    (setq skemacs--splash-buffer buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)

        ;; ── Banner ──
        (let* ((banner-path (when skemacs-banner-file
                              (expand-file-name
                               (format "%d-banner.txt" skemacs-banner-file)
                               (expand-file-name "banners" user-emacs-directory)))))
          (when (and banner-path (file-exists-p banner-path))
            (insert "\n")
            (let ((banner-lines (split-string
                                 (with-temp-buffer
                                   (insert-file-contents banner-path)
                                   (buffer-string))
                                 "\n")))
              (dolist (line banner-lines)
                (insert (skemacs--center-string line win-width) "\n")))
            (insert "\n")))

        (insert (skemacs--center-string "[ Press Enter to continue ]" win-width) "\n\n")

        ;; ── 欢迎信息 ──
        (insert (skemacs--center-string
                 (format "Emacs %s  |  Skemacs Configuration" emacs-version)
                 win-width) "\n\n")

        ;; ── 表头 ──
        (let* ((table-width 56)
               (left-pad (max 0 (/ (- win-width table-width) 2)))
               (pad (make-string left-pad ?\s)))
          (insert pad (make-string table-width ?─) "\n")
          (insert pad (format "  %-32s %10s  %s\n" "Module" "Time" "Status"))
          (insert pad (make-string table-width ?─) "\n"))))

    ;; 切换到 splash buffer 并全屏
    (switch-to-buffer buf)
    (delete-other-windows)
    (redisplay t)))

(defun skemacs--splash-append-line (name time-str status)
  "在 splash buffer 末尾追加一行模块加载信息，并刷新显示。
NAME 为模块名，TIME-STR 为格式化的耗时，STATUS 为状态字符串。"
  (when (and skemacs--splash-buffer (buffer-live-p skemacs--splash-buffer))
    (with-current-buffer skemacs--splash-buffer
      (let* ((inhibit-read-only t)
             (win-width (or (window-width (get-buffer-window skemacs--splash-buffer))
                            (window-width)))
             (table-width 56)
             (left-pad (max 0 (/ (- win-width table-width) 2)))
             (pad (make-string left-pad ?\s)))
        (goto-char (point-max))
        (insert pad (format "  %-32s %10s  %s\n" name time-str status))
        ;; 自动滚动到底部
        (let ((win (get-buffer-window skemacs--splash-buffer)))
          (when win
            (set-window-point win (point-max))))
        (redisplay t)))))

(defun skemacs--splash-append-skip (name)
  "在 splash buffer 中显示跳过的模块。"
  (skemacs--splash-append-line name "--" "SKIP"))

(defun skemacs--splash-finalize (total-elapsed)
  "在 splash buffer 末尾追加汇总行和提示，进入 recursive-edit 等待 Enter。
TOTAL-ELAPSED 为总启动耗时（秒）。
用户可自由移动光标和滚动，按 Enter 后关闭 splash 并进入 dired。"
  (when (and skemacs--splash-buffer (buffer-live-p skemacs--splash-buffer))
    (with-current-buffer skemacs--splash-buffer
      (let* ((inhibit-read-only t)
             (win-width (or (window-width (get-buffer-window skemacs--splash-buffer))
                            (window-width)))
             (table-width 56)
             (left-pad (max 0 (/ (- win-width table-width) 2)))
             (pad (make-string left-pad ?\s))
             (total-time (apply #'+ (or (mapcar #'cdr skemacs-load-times) '(0))))
             (errors (length skemacs-module-errors))
             (loaded skemacs-modules-loaded)
             (disabled skemacs-modules-disabled))
        (goto-char (point-max))

        ;; ── 汇总行 ──
        (insert pad (make-string table-width ?─) "\n")
        (insert pad (format "  Total: %-8s | %d loaded | %d disabled | %d errors\n"
                            (skemacs--format-time total-time) loaded disabled errors))

        ;; ── 错误详情 ──
        (when skemacs-module-errors
          (insert "\n")
          (insert (skemacs--center-string "── Error Details ──" win-width) "\n")
          (dolist (err (reverse skemacs-module-errors))
            (insert (skemacs--center-string
                     (format "%s: %s" (car err) (cdr err))
                     win-width) "\n")))

        ;; ── 将启动时间写入欢迎信息行（居中显示）──
        (goto-char (point-min))
        (when (search-forward "Skemacs Configuration" nil t)
          (beginning-of-line)
          (let ((beg (point)))
            (end-of-line)
            (delete-region beg (point))
            (insert (skemacs--center-string
                     (format "Emacs %s  |  Skemacs Configuration  |  Starting time: %.3fs"
                             emacs-version total-elapsed)
                     win-width))))

        ;; 滚到行首
        (goto-char (point-min))
        (read-only-mode 1)

        ;; 设置 keymap：允许移动光标和滚动，Enter 退出
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map special-mode-map)
          (define-key map (kbd "RET")
                      (lambda () (interactive) (exit-recursive-edit)))
          (define-key map (kbd "<return>")
                      (lambda () (interactive) (exit-recursive-edit)))
          (use-local-map map))))

    ;; 确保 splash buffer 在最前面，光标定位到左上角
    (switch-to-buffer skemacs--splash-buffer)
    (delete-other-windows)
    (goto-char (point-min))
    (set-window-start (selected-window) (point-min))
    (redisplay t)

    ;; 进入 recursive-edit：允许完整的光标移动和滚动
    ;; 只有按 Enter 才会退出循环
    (recursive-edit)

    ;; Enter 按下，进入 dired
    (skemacs--dismiss-startup-splash)))

(defun skemacs--dismiss-startup-splash ()
  "关闭启动 splash 画面，进入 dired。"
  (interactive)
  (when (get-buffer "*skemacs*")
    (kill-buffer "*skemacs*"))
  ;; 打开当前目录的 dired
  (dired default-directory))

;; ============================================================================
;; 核心加载函数
;; ============================================================================

(defun skemacs--format-time (seconds)
  "将 SECONDS 格式化为可读字符串。"
  (cond
   ((>= seconds 1.0) (format "%.3fs" seconds))
   ((>= seconds 0.001) (format "%.0fms" (* seconds 1000)))
   (t (format "%.0fus" (* seconds 1000000)))))

(defun skemacs-load-module (name &optional dir)
  "加载名为 NAME 的模块，记录耗时并隔离错误。
NAME 为模块名（不含 .el 后缀）。
DIR 为模块所在目录，默认为 `skemacs-modules-dir'。
加载结果会实时更新到启动 splash 画面中。
返回 t 表示加载成功，nil 表示失败。"
  (let* ((module-dir (or dir skemacs-modules-dir))
         (file (expand-file-name (concat name ".el") module-dir))
         (time-start (current-time)))
    (if (not (file-exists-p file))
        (progn
          (push (cons name "file not found") skemacs-module-errors)
          (skemacs--splash-append-line name "--" "MISS")
          (message "[skemacs] Warning: module %s not found (%s)" name file)
          nil)
      (condition-case err
          (progn
            (load file nil t)
            (let* ((elapsed (float-time (time-subtract (current-time) time-start)))
                   (time-str (skemacs--format-time elapsed))
                   (status (if (>= elapsed 0.5) "SLOW" "OK")))
              (push (cons name elapsed) skemacs-load-times)
              (setq skemacs-modules-loaded (1+ skemacs-modules-loaded))
              (skemacs--splash-append-line name time-str status)
              (message "[skemacs] Loaded %-30s  %6.3fs" name elapsed)
              t))
        (error
         (let* ((elapsed (float-time (time-subtract (current-time) time-start)))
                (time-str (skemacs--format-time elapsed))
                (err-msg (error-message-string err)))
           (push (cons name elapsed) skemacs-load-times)
           (push (cons name err-msg) skemacs-module-errors)
           (skemacs--splash-append-line name time-str "ERROR")
           (message "[skemacs] Error: failed to load %s — %s" name err-msg)
           nil))))))

(defun skemacs-load-core-module (name)
  "加载 core/ 目录下名为 NAME 的核心模块，记录耗时。
与 `skemacs-load-module' 类似，但从 `skemacs-core-dir' 加载。"
  (skemacs-load-module name skemacs-core-dir))

(defun skemacs--get-module-files ()
  "获取 modules/ 目录下所有 .el 文件名（不含后缀），按字母排序。"
  (when (file-directory-p skemacs-modules-dir)
    (let ((files (directory-files skemacs-modules-dir nil "\\.el$" t)))
      (mapcar #'file-name-sans-extension (sort files #'string<)))))

(defun skemacs-load-all-modules ()
  "加载所有启用的功能模块。
如果 `skemacs-module-list' 非空，按列表顺序加载。
否则自动扫描 `skemacs-modules-dir' 目录。
被 `skemacs-disabled-modules' 列出的模块会被跳过。"
  (let* ((modules (or skemacs-module-list (skemacs--get-module-files)))
         (total (length modules)))
    (message "[skemacs] Loading %d modules..." total)
    (dolist (mod modules)
      (if (member mod skemacs-disabled-modules)
          (progn
            (setq skemacs-modules-disabled (1+ skemacs-modules-disabled))
            (skemacs--splash-append-skip mod)
            (message "[skemacs] Skipped disabled module: %s" mod))
        (skemacs-load-module mod)))
    ;; 同时加载 local/ 目录（如果有文件）
    (when (file-directory-p skemacs-local-dir)
      (let ((local-files (directory-files skemacs-local-dir nil "\\.el$" t)))
        (when local-files
          (message "[skemacs] Loading %d local configs..." (length local-files))
          (dolist (f (sort local-files #'string<))
            (let ((name (file-name-sans-extension f)))
              (skemacs-load-module name skemacs-local-dir))))))))

;; ============================================================================
;; 计时报告（*Messages* 输出 + 交互式 buffer）
;; ============================================================================

(defun skemacs--print-load-report ()
  "在 *Messages* 中打印加载计时摘要。"
  (let* ((times (reverse skemacs-load-times))
         (total-time (apply #'+ (or (mapcar #'cdr times) '(0))))
         (errors (length skemacs-module-errors))
         (loaded skemacs-modules-loaded)
         (disabled skemacs-modules-disabled))
    (message "")
    (message "══════════════════════════════════════════════════════════")
    (message "              Emacs Startup Timing Report")
    (message "══════════════════════════════════════════════════════════")
    (message " %-32s %10s  %s" "Module" "Time" "Status")
    (message "──────────────────────────────────────────────────────────")
    (dolist (entry times)
      (let* ((name (car entry))
             (time (cdr entry))
             (err (assoc name skemacs-module-errors))
             (status (if err "ERROR" (if (>= time 0.5) "SLOW" "OK")))
             (time-str (skemacs--format-time time)))
        (message " %-32s %10s  %s" name time-str status)))
    (message "──────────────────────────────────────────────────────────")
    (message " Total: %s | %d loaded | %d disabled | %d errors"
             (skemacs--format-time total-time) loaded disabled errors)
    (message "══════════════════════════════════════════════════════════")
    (message "")))

(defun skemacs/show-load-times ()
  "在专用 buffer 中展示模块加载计时报告。"
  (interactive)
  (let ((buf (get-buffer-create "*skemacs-load-times*"))
        (times (reverse skemacs-load-times))
        (total-time (apply #'+ (or (mapcar #'cdr skemacs-load-times) '(0))))
        (errors (length skemacs-module-errors))
        (loaded skemacs-modules-loaded)
        (disabled skemacs-modules-disabled))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Startup Timing Report\n")
        (insert (make-string 56 ?─) "\n")
        (insert (format "  %-32s %10s  %s\n" "Module" "Time" "Status"))
        (insert (make-string 56 ?─) "\n")
        (dolist (entry times)
          (let* ((name (car entry))
                 (time (cdr entry))
                 (err (assoc name skemacs-module-errors))
                 (status (if err "ERROR" (if (>= time 0.5) "SLOW" "OK")))
                 (time-str (skemacs--format-time time)))
            (insert (format "  %-32s %10s  %s\n" name time-str status))))
        (insert (make-string 56 ?─) "\n")
        (insert (format "  Total: %-8s | %d loaded | %d disabled | %d errors\n"
                        (skemacs--format-time total-time) loaded disabled errors))
        (when skemacs-module-errors
          (insert "\nError Details:\n")
          (insert (make-string 56 ?─) "\n")
          (dolist (err (reverse skemacs-module-errors))
            (insert (format "  %s: %s\n" (car err) (cdr err)))))
        (goto-char (point-min))
        (read-only-mode 1)
        (special-mode)))
    (switch-to-buffer-other-window buf)))

(provide 'core-module)
;;; core-module.el ends here
