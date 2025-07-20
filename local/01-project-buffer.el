;; Project-aware buffer management
(require 'projectile)
(require 'ivy)

;; Define face for recent files separator
(defface skemacs/recent-files-separator
  '((t :background "#d946ef" :foreground "#000000" :weight bold :extend t))
  "Face for recent files separator line."
  :group 'skemacs)

(defun skemacs/should-filter-buffer-p (buffer)
  "Check if buffer should be filtered out from display."
  (let ((name (buffer-name buffer)))
    (or 
     ;; Filter out minibuffer and echo area
     (string-match "\\*Minibuf-[0-9]+\\*" name)
     (string-match "\\*Echo Area [0-9]+\\*" name)
     ;; Filter out other internal buffers
     (string-prefix-p " " name))))  ; Buffers starting with space are usually internal

(defun skemacs/get-project-buffers ()
  "Get all buffers that belong to the current project, including system buffers."
  (let ((all-buffers (if (and (fboundp 'projectile-project-p) (projectile-project-p))
                         (let ((project-root (projectile-project-root)))
                           (seq-filter (lambda (buf)
                                         (or 
                                          ;; Include system buffers (starting with *)
                                          (string-prefix-p "*" (buffer-name buf))
                                          ;; Include project files
                                          (when-let ((file-name (buffer-file-name buf)))
                                            (string-prefix-p project-root file-name))
                                          ;; Include buffers with default-directory in project
                                          (with-current-buffer buf
                                            (and default-directory
                                                 (string-prefix-p project-root (expand-file-name default-directory))))))
                                       (buffer-list)))
                       ;; If not in project, return all buffers
                       (buffer-list))))
    ;; Filter out internal buffers
    (seq-filter (lambda (buf) (not (skemacs/should-filter-buffer-p buf))) all-buffers)))

(defun skemacs/format-buffer-size (size)
  "Format buffer size in human readable format."
  (cond
   ((> size 1048576) (format "%.1fM" (/ size 1048576.0)))
   ((> size 1024) (format "%.1fk" (/ size 1024.0)))
   (t (format "%d" size))))

(defun skemacs/format-buffer-for-display (buffer)
  "Format buffer for display similar to Helm buffer list."
  (let* ((name (buffer-name buffer))
         (file (buffer-file-name buffer))
         (size (with-current-buffer buffer (buffer-size)))
         (formatted-size (skemacs/format-buffer-size size))
         (mode (with-current-buffer buffer 
                 (cond
                  ((string-prefix-p "*" name) 
                   (cond
                    ((string= name "*scratch*") "Text")
                    ((string= name "*Messages*") "Messages")
                    ((string-match "\\*\\(.+\\)\\*" name) 
                     (capitalize (match-string 1 name)))
                    (t "Fundamental")))
                  ((eq major-mode 'dired-mode) "Dired by name")
                  (t (format-mode-line mode-name)))))
         (dir (cond
               (file (file-name-directory file))
               ((and (fboundp 'projectile-project-p) (projectile-project-p))
                (projectile-project-root))
               (t (with-current-buffer buffer default-directory))))
         (colored-dir (when dir 
                        (let ((home-dir (expand-file-name "~"))
                              (shortened-dir (if (string-prefix-p (expand-file-name "~") dir)
                                               (concat "~" (substring dir (length (expand-file-name "~"))))
                                             dir)))
                          (propertize shortened-dir 'face '(:foreground "blue"))))))
    (format "%-25s %8s %15s %s"
            name formatted-size mode (or colored-dir ""))))

(defun skemacs/get-project-buffer-list ()
  "Get formatted buffer list for current project."
  (mapcar #'skemacs/format-buffer-for-display (skemacs/get-project-buffers)))

(defun skemacs/switch-buffer ()
  "Switch buffer with project-aware filtering and recent files display."
  (interactive)
  (if (and (fboundp 'projectile-project-p) (projectile-project-p))
      (let* ((project-buffers (skemacs/get-project-buffer-list))
             (recent-files (when (fboundp 'skemacs/project-recent-files-get)
                            (skemacs/project-recent-files-get)))
             (recent-file-names (mapcar (lambda (file) 
                                         (if (string-prefix-p (expand-file-name "~") file)
                                             (concat "~" (substring file (length (expand-file-name "~"))))
                                           file))
                                       recent-files))
             (all-candidates (append project-buffers
                                   (when recent-file-names
                                     (append (list "" 
                                                   (let* ((text "--- Recent Files ---")
                                                          (width (- (window-width) 1))
                                                          (padding (max 0 (- width (length text)))))
                                                     (propertize (concat text (make-string padding ? ))
                                                                 'face 'skemacs/recent-files-separator)))
                                             recent-file-names)))))
        (if all-candidates
            (let ((ivy-truncate-lines t)
                  (ivy-wrap nil)  ; 禁用默认的循环
                  (buffer-count (length project-buffers))
                  (separator-line "--- Recent Files ---"))
              (ivy-read (format "[%s] Switch to buffer: " (file-name-nondirectory (directory-file-name (projectile-project-root)))) all-candidates
                        :keymap (let ((map (make-sparse-keymap)))
                                  (set-keymap-parent map ivy-minibuffer-map)
                                  ;; 自定义C-n行为
                                  (define-key map (kbd "C-n") 
                                    (lambda ()
                                      (interactive)
                                      (let ((current-idx ivy--index)
                                            (total-candidates (length ivy--all-candidates)))
                                        (cond
                                         ;; 在buffer list最后一项，跳过分隔符到recent files第一项
                                         ((and (= current-idx (1- buffer-count))
                                               recent-file-names)
                                          (setq ivy--index (+ buffer-count 1))) ; 跳过空行和分隔符
                                         ;; 在recent files最后一项，不移动
                                         ((= current-idx (1- total-candidates))
                                          nil)
                                         ;; 其他情况正常向下
                                         (t (ivy-next-line)))
                                        (ivy--exhibit))))
                                  ;; 自定义C-p行为  
                                  (define-key map (kbd "C-p")
                                    (lambda ()
                                      (interactive)
                                      (let ((current-idx ivy--index)
                                            (total-candidates (length ivy--all-candidates)))
                                        (cond
                                         ;; 在buffer list第一项，不移动
                                         ((= current-idx 0)
                                          nil)
                                         ;; 在recent files第一项，跳回buffer list最后一项
                                         ((and (= current-idx (+ buffer-count 1))
                                               recent-file-names)
                                          (setq ivy--index (1- buffer-count)))
                                         ;; 其他情况正常向上
                                         (t (ivy-previous-line)))
                                        (ivy--exhibit))))
                                  map)
                        :action (lambda (candidate)
                                (cond
                                 ((string= candidate "")
                                  (message "Empty separator line"))
                                 ((string-prefix-p "--- Recent Files ---" candidate)
                                  (message "Select a recent file below"))
                                 ((member candidate recent-file-names)
                                  (let ((index (seq-position recent-file-names candidate)))
                                    (when index
                                      (let ((original-file (nth index recent-files)))
                                        (if (file-exists-p original-file)
                                            (find-file original-file)
                                          (message "File not found: %s" original-file))))))
                                 (t 
                                  ;; Extract buffer name from formatted string
                                  (let ((buffer-name (if (string-match "^\\([^ ]+\\)" candidate)
                                                         (match-string 1 candidate)
                                                       candidate)))
                                    (switch-to-buffer buffer-name)))))
                        :caller 'skemacs/switch-buffer))
          (message "No project buffers found")))
    ;; Fall back to normal buffer switching if not in a project
    (ivy-switch-buffer)))

;; C-x b binding is handled in 02-ivy.el