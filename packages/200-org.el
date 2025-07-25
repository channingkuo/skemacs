(use-package org
  :config
  ;; 自动换行
  (setq truncate-lines nil)
  ;; 代码块语法高亮
  (setq org-src-fontify-natively t)
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d@/!)" "ABORT(a@/!)")))
  (setq org-todo-keyword-faces '(("TODO"  . "red")
                                 ("DOING" . "yellow")
                                 ("DONE"  . "green")
                                 ("ABORT" . "gray")))
  (setq org-enforce-todo-dependencies t)
  (setq org-default-notes-file "~/org/capture.org")
  
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