(use-package org
  :config
  (setq org-ellipsis " ⤵")
  ;; 自动换行
  (setq truncate-lines nil)
  ;; 代码块语法高亮
  (setq org-src-fontify-natively t)
  (setq org-log-done 'time)
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d@/!)" "ABORT(a@/!)")))
  (setq org-todo-keyword-faces '(("TODO"  . (:foreground "red" :weight bold))
                                 ("DOING" . (:foreground "orange" :weight bold))
                                 ("DONE"  . (:foreground "green" :weight bold))
                                 ("ABORT" . (:foreground "gray" :weight bold :strike-through t))))
  ;; 为不同 TODO 状态的 headline 设置颜色的 hook
  (defun skemacs/org-set-todo-headline-colors ()
    "为不同 TODO 状态设置 headline 颜色"
    (font-lock-add-keywords nil
      '(("^\\*+ TODO \\(.*\\)$" 1 '(:foreground "#ff6c6b" :weight bold) t)
        ("^\\*+ DOING \\(.*\\)$" 1 '(:foreground "#ECBE7B" :weight bold) t)
        ("^\\*+ DONE \\(.*\\)$" 1 '(:foreground "#98be65" :strike-through t) t)
        ("^\\*+ ABORT \\(.*\\)$" 1 '(:foreground "#5B6268" :strike-through t) t))
      'append))
  (add-hook 'org-mode-hook 'skemacs/org-set-todo-headline-colors)

  (setq org-enforce-todo-dependencies t)
  (setq org-directory "~/org")
  (setq org-directory-publish "~/org/www")
  (setq org-default-notes-file "~/org/capture.org")
  (setq font-lock-ensure t)
  (setq org-log-into-drawer "LOGBOOK")
  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))
  (setq org-startup-indented t)
  (setq org-return-follows-link t)
  (setq org-pretty-entities t)
  (setq org-pretty-entities-include-sub-superscripts t)
  (setq org-hide-emphasis-markers t)
  ;; (setq org-agenda-block-separator "")
  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-done-headline t)
  (setq org-fontify-quote-and-verse-blocks t)
  ;; 加载的时候不显示图片
  (setq org-startup-with-inline-images nil)
  (setq org-image-actual-width '(300))
  (setq org-startup-folded 'overview)
  ;; 自动显示到2级标题的 hook
  (defun skemacs/org-show-two-levels ()
    "打开文件时只显示1级和2级标题"
    (org-content 2))
  (add-hook 'org-mode-hook 'skemacs/org-show-two-levels)

  (setq org-edit-src-content-indentation 0)
  (setq org-html-link-org-files-as-html t)
  (setq org-confirm-babel-evaluate t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)
  
  (setq org-capture-templates
        '(("t" "Capture a new Task" entry (file+headline "~/org/capture.org" "Todos Lists")
           "* TODO %?\12%u\12%a\12")
          ("n" "Note" entry (file+headline "~/org/capture.org" "Notes")
           "* %?\12%u\12%a\12")
          ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
           "* %?\12Entered on %U\12%a\12"))))
  ;; (add-to-list 'org-capture-templates
  ;;            '("r" "Book Reading Task" entry
  ;;              (file+olp "~/org/capture.org" "Reading" "Book")
  ;;              "* TODO %^{书名}\n%u\n%a\n" :clock-in t :clock-resume t)))

;; 已配置
;; M-<up>	将当前 headline 及其内容作为整体向上移动	 
;; M-<down>	将当前 headline 及其内容作为整体向下移动
;; M-<return>	在当前 headline 前建立一个同级 headline

;; 未配置
;; C-<return>	在当前 headline 所属的内容后建立一个同级 headline	无 headline 时创建一个一级 headline

;; M-<right>	降低当前 headline 的层级	 
;; M-<left>	提高当前 headline 的层级	 