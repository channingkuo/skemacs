;; 向下移动10行
(global-set-key (kbd "M-n") 'skemacs/next-ten-lines)
;; 向上移动10行
(global-set-key (kbd "M-p") 'skemacs/previous-ten-lines)
;; 复制选中内容到剪贴板
(global-set-key (kbd "C-c C-y") 'skemacs/copy-to-clipboard)
;; 重新加载配置文件
(global-set-key (kbd "C-x a i R") 'skemacs/reload-config)

;; 将 C-j 设为前缀键，覆盖原来的 electric-newline-and-maybe-indent
(define-prefix-command 'skemacs)
(global-set-key (kbd "C-j") 'skemacs)
;; avy 快速跳转
;; M-g w 绑定到 avy-goto-word-1
(global-set-key (kbd "C-j C-SPC") 'avy-goto-char-timer)
;; 删去光标所在行（在图形界面时可以用 "C-S-<DEL>"，终端常会拦截这个按法)
(global-set-key (kbd "C-j C-k") 'kill-whole-line)
;; 54-undo-tree.el
(global-set-key (kbd "C-j u") 'hydra-undo-tree/body)
;; 08-multiple-cursors.el
(global-set-key (kbd "C-j m") 'hydra-multiple-cursors/body)

;; window相关快捷键设置
(define-prefix-command 'window)
(global-set-key (kbd "C-x w") 'window)
(global-set-key (kbd "C-x w h") 'windmove-left)
(global-set-key (kbd "C-x w j") 'windmove-down)
(global-set-key (kbd "C-x w k") 'windmove-up)
(global-set-key (kbd "C-x w l") 'windmove-right)
;; 覆盖window-toggle-side-windows, 水平分隔window
(global-set-key (kbd "C-x w s") 'split-window-below)
(global-set-key (kbd "C-x w S") 'skemacs/split-window-horizontally)
(global-set-key (kbd "C-x w v") 'split-window-right)
(global-set-key (kbd "C-x w V") 'skemacs/split-window-vertically)
;; 99-window-hydra.el
(global-set-key (kbd "C-x w {") 'hydra-window-resize/body)

;; buffer 相关快捷键统一到C-x b前缀下
(define-prefix-command 'buffer)
(global-set-key (kbd "C-x b") 'buffer)
;; 强化buffer切换
(global-set-key (kbd "C-x b b") 'skemacs/switch-buffer)
;; next buffer
(global-set-key (kbd "C-x b n") 'next-buffer)
;; previous buffer
(global-set-key (kbd "C-x b p") 'previous-buffer)
;; kill current buffer
(global-set-key (kbd "C-x b k") 'kill-buffer)
;; 解绑C-x k快捷键 原来都功能是 kill-buffer，现已移至C-x b k
(global-unset-key (kbd "C-x k"))

;; pop-global-mark lsp查找定义后用于快速返回
(global-set-key (kbd "C-o") 'pop-global-mark)
;; 纯文本补全
(global-set-key (kbd "M-/") 'dabbrev-expand)
;; (global-set-key (kbd "M-/") 'hippie-expand)

;; C-x r m （bookmark-set） ：设置书签。可以为书签起个标记名称，默认为文件名。也可以为目录打书签。
;; C-x r b（bookmark-jump，counsel-bookmark） ：跳转到书签。
;; C-x r l （bookmark-bmenu-list）：列出已有书签。
;; M-x bookmark-delete ：删除书签。