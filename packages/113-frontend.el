;; Frontend development configuration

(with-eval-after-load 'warnings
  (add-to-list 'warning-suppress-types '(native-compiler)))
;; TypeScript mode
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :config
  (setq typescript-indent-level 2)
  ;; 确保分号结尾
  (setq typescript-auto-indent-flag t))

;; Enhanced JavaScript mode
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" "\\.jsx\\'")
  :config
  (setq js2-basic-offset 2)
  (setq js2-bounce-indent-p nil)
  ;; 确保分号结尾
  (setq js2-missing-semi-one-line-override nil))

;; Web mode for HTML, CSS, and mixed content
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.htm\\'" "\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.vue\\'" "\\.svelte\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t))

;; SCSS mode
(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :config
  (setq scss-compile-at-save nil))

;; LESS mode  
(use-package less-css-mode
  :ensure t
  :mode "\\.less\\'")

;; JSON mode
(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

;; YAML mode
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

;; Prettier for JavaScript/TypeScript formatting
(use-package prettier-js
  :ensure t
  ;; 注释掉自动启用 hook，改为手动控制
  ;; :hook ((js-mode js2-mode typescript-mode web-mode) . prettier-js-mode)
  :config
  (setq prettier-js-args '(
                           "--single-quote" "true"
                           "--print-width" "100"
                           "--trailing-comma" "none"
                           "--html-whitespace-sensitivity" "ignore"
                           "--semi"))
  
  
  ;; 绑定到 LSP 格式化快捷键
  (with-eval-after-load 'lsp-mode
    ;; 重写 LSP 格式化函数使用 Prettier
    (defun skemacs/prettier-format-buffer ()
      "Format buffer with prettier."
      (interactive)
      (when (and buffer-file-name
                (or (derived-mode-p 'js-mode)
                    (derived-mode-p 'js2-mode) 
                    (derived-mode-p 'typescript-mode)
                    (derived-mode-p 'web-mode)
                    (string-match-p "\\.vue\\'" buffer-file-name)))
        (prettier-js)))
    
    ;; 重写 lsp-format-buffer 在前端文件中使用 Prettier
    (defun skemacs/lsp-format-buffer-with-prettier (orig-fun &rest args)
      "Use prettier for frontend files, otherwise use original LSP formatting."
      (if (and buffer-file-name
               (or (derived-mode-p 'js-mode)
                   (derived-mode-p 'js2-mode)
                   (derived-mode-p 'typescript-mode) 
                   (derived-mode-p 'web-mode)
                   (string-match-p "\\.vue\\'" buffer-file-name)))
          (prettier-js)
        (apply orig-fun args)))
    
    (advice-add 'lsp-format-buffer :around #'skemacs/lsp-format-buffer-with-prettier)
    
    ;; 添加切换保存时自动格式化的函数
    (defun skemacs/toggle-prettier-on-save ()
      "Toggle prettier-js-mode for automatic formatting on save."
      (interactive)
      (if prettier-js-mode
          (progn
            (prettier-js-mode -1)
            (message "Prettier auto-format on save: DISABLED"))
        (progn
          (prettier-js-mode 1)
          (message "Prettier auto-format on save: ENABLED"))))
    
    ;; 添加格式化选中区域的函数
    (defun skemacs/prettier-format-region (beg end)
      "Format selected region with prettier."
      (interactive "r")
      (when (and buffer-file-name
                 (or (derived-mode-p 'js-mode)
                     (derived-mode-p 'js2-mode)
                     (derived-mode-p 'typescript-mode)
                     (derived-mode-p 'web-mode)
                     (string-match-p "\\.vue\\'" buffer-file-name)))
        (let ((temp-file (make-temp-file "prettier" nil 
                                         (file-name-extension buffer-file-name t)))
              (selected-text (buffer-substring-no-properties beg end)))
          (unwind-protect
              (progn
                ;; 将选中内容写入临时文件
                (with-temp-file temp-file
                  (insert selected-text))
                ;; 用 prettier 格式化临时文件 (--write 直接修改文件)
                (let ((result (shell-command (format "prettier --write %s %s" 
                                                     (mapconcat 'identity prettier-js-args " ")
                                                     (shell-quote-argument temp-file)))))
                  (when (= result 0)  ; 格式化成功
                    ;; 读取格式化后的内容并替换选中区域
                    (let ((formatted-content (with-temp-buffer
                                               (insert-file-contents temp-file)
                                               (buffer-string))))
                      (delete-region beg end)
                      (goto-char beg)
                      (insert formatted-content)))))
            ;; 清理临时文件
            (when (file-exists-p temp-file)
              (delete-file temp-file))))))
    
    ;; 绑定快捷键
    (define-key lsp-mode-map (kbd "C-c l = -") #'skemacs/prettier-format-buffer)
    (define-key lsp-mode-map (kbd "C-c l = t") #'skemacs/toggle-prettier-on-save)
    (define-key lsp-mode-map (kbd "C-c l = R") #'skemacs/prettier-format-region)
    (which-key-add-key-based-replacements "C-c l = -" "format buffer with prettier")
    (which-key-add-key-based-replacements "C-c l = t" "toggle prettier on save")
    (which-key-add-key-based-replacements "C-c l = R" "format region with prettier")))