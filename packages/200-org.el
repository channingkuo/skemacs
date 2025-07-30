;;; 200-org.el --- Comprehensive Org-mode configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration provides a comprehensive Org-mode setup for
;; task management, note-taking, and project organization. Includes
;; custom TODO workflows, agenda views, capture templates, and
;; super-agenda for advanced task organization and prioritization.

;;; Code:

(use-package org
  :ensure t
  ;; Associate all org files with org mode
  ;; dashboard中显示agenda异常 cache must be activated
  ;; :mode "\\.org\\'"
  :hook (
	 ;; Make the indentation look nicer
	 (org-mode . org-indent-mode)
	 ;; Wrap the lines in org mode so that things are easier to read
	 (org-mode . visual-line-mode)
	 ;; 恢复全局的C-j prefix设置，将org原有功能移到C-RET
	 (org-mode . (lambda () 
                       (local-set-key (kbd "C-j") 'skemacs)
                       (local-set-key (kbd "C-<return>") 'org-return-and-maybe-indent))))
  :config
  (highlight-symbol-mode -1)
  ;; Must do this so the agenda knows where to look for my files
  (setq org-agenda-files '("~/org"))
  (setq org-directory "~/org")
  (setq org-default-notes-file "~/org/emacs.org")
  ;; When a TODO is set to a done state, record a timestamp
  (setq org-log-done 'time)
  ;; Follow the links
  (setq org-return-follows-link  t)
  ;; Hide the markers so you just see bold text as BOLD-TEXT and not *BOLD-TEXT*
  (setq org-hide-emphasis-markers t)
  (setq org-log-into-drawer "LOGBOOK")
  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))
  ;; 子TODO全部完成了，父TODO才能完成
  (setq org-enforce-todo-dependencies t)
  ;; 加载的时候不显示图片
  (setq org-startup-with-inline-images nil)
  (setq org-image-actual-width '(300))
  (setq org-ellipsis " ⤵")
  ;; 自动换行
  (setq truncate-lines nil)
  ;; 代码块语法高亮
  (setq org-src-fontify-natively t)

  (setq font-lock-ensure t)
  (setq org-pretty-entities t)
  (setq org-pretty-entities-include-sub-superscripts t)
  ;; (setq org-agenda-block-separator "")
  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-done-headline t)
  (setq org-fontify-quote-and-verse-blocks t)

  (setq org-edit-src-content-indentation 0)
  (setq org-html-link-org-files-as-html t)
  (setq org-confirm-babel-evaluate t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)
  
  (setq org-startup-folded 'overview)
  ;; 自动显示到2级标题的 hook
  (defun skemacs/org-show-two-levels ()
    "打开文件时只显示1级和2级标题"
    (org-content 2))
  (add-hook 'org-mode-hook 'skemacs/org-show-two-levels)

  (setq org-capture-templates
	'(    
          ("a" "App Codes"
           entry (file+headline "~/org/notes.org" "App Codes")
           "** %?"
           :empty-lines 0)

           ("n" "Note"
           entry (file+headline "~/org/notes.org" "Random Notes")
           "** %?"
           :empty-lines 0)

          ("j" "Work Log Entry"
           entry (file+datetree "~/org/work-log.org")
           "* %?"
           :empty-lines 0)

          ("c" "Code To-Do"
           entry (file+headline "~/org/todos.org" "Code Related Tasks")
           "* TODO [#B] %? %^g\n:Created: %T\n%i\n%a\nProposed Solution: "
           :empty-lines 0)

          ("g" "General To-Do"
           entry (file+headline "~/org/todos.org" "General Tasks")
           "* TODO [#B] %?\n:Created: %T\n "
           :empty-lines 0)

          ("e" "Emacs Config To-Do"
           entry (file+headline "~/org/emacs.org" "Emacs Config Tasks")
           "* TODO [#B] %? %^g\n:Created: %T\n "
           :empty-lines 0)

          ("m" "Meeting"
           entry (file+datetree "~/org/meetings.org")
           "* %? :meeting:%^g \n:Created: %T\n** Attendees\n*** \n** Notes\n** Action Items\n*** TODO [#A] "
           :tree-type week
           :clock-in t
           :clock-resume t
           :empty-lines 0)

	  ))

  (setq org-todo-keywords
	'((sequence "TODO(t)" "PLANNING(p)" "IN-PROGRESS(i@/!)" 
                    "VERIFYING(v!)" "BLOCKED(b@)"  "|" "DONE(d!)" 
                    "OBE(o@!)" "WONT-DO(w@/!)" )))

  (setq org-todo-keyword-faces
	'(
          ("TODO" . (:foreground "GoldenRod" :weight bold))
          ("PLANNING" . (:foreground "DeepPink" :weight bold))
          ("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
          ("VERIFYING" . (:foreground "DarkOrange" :weight bold))
          ("BLOCKED" . (:foreground "Red" :weight bold))
          ("DONE" . (:foreground "LimeGreen" :weight bold))
          ("OBE" . (:foreground "LimeGreen" :weight bold))
          ("WONT-DO" . (:foreground "LimeGreen" :weight bold))
	  ))

  ;; 设置之后会覆盖tag标签的颜色， 只设置TODO关键字的颜色也很OK
  ;; 为不同 TODO 状态的 headline 设置颜色的 hook
  ;; (defun skemacs/org-set-todo-headline-colors ()
  ;;   "为不同 TODO 状态设置 headline 颜色"
  ;;   (font-lock-add-keywords nil
  ;;     '(("^\\*+ TODO \\(.*\\)$" 1 '(:foreground "GoldenRod" :weight bold) t)
  ;;       ("^\\*+ PLANNING \\(.*\\)$" 1 '(:foreground "DeepPink" :weight bold) t)
  ;;       ("^\\*+ IN-PROGRESS \\(.*\\)$" 1 '(:foreground "Cyan" :weight bold) t)
  ;;       ("^\\*+ VERIFYING \\(.*\\)$" 1 '(:foreground "DarkOrange" :weight bold) t)
  ;;       ("^\\*+ BLOCKED \\(.*\\)$" 1 '(:foreground "Red" :weight bold) t)
  ;;       ("^\\*+ DONE \\(.*\\)$" 1 '(:foreground "LimeGreen") t)
  ;;       ("^\\*+ OBE \\(.*\\)$" 1 '(:foreground "LimeGreen") t)
  ;;       ("^\\*+ WONT-DO \\(.*\\)$" 1 '(:foreground "LimeGreen") t))
  ;;     'append))
  ;; (add-hook 'org-mode-hook 'skemacs/org-set-todo-headline-colors)

  ;; Tags
  (setq org-tag-alist '(
                        ("@setup" . ?s)
                        ("@test" . ?t)

                        ("@feature" . ?u)
                        ("@spike" . ?j)
                        ("@emergency" . ?e)
                        ("@research" . ?h)
                        ("meeting" . ?g)

                        ("backend" . ?k)
                        ("broken_code" . ?c)
                        ("frontend" . ?f)
                        ("bug" . ?b)
                        ("refactor" . ?r)
                        ;; 杂项
                        ("misc" . ?m)
                        ("new_work" . ?n)
                        ;; 成就
                        ("accomplishment" . ?a)
                        ;; Special tags
                        ;; 影响重大, 最优先级
                        ("CRITICAL" . ?x)
                        ;; 阻碍 阻力
                        ("OBSTACLE" . ?o)
			))

  ;; Tag colors organized by urgency hierarchy
  (setq org-tag-faces
	'(
          ;; HIGHEST URGENCY - Critical/Emergency items (bright reds, maximum visibility)
          ("CRITICAL"       . (:foreground "red1"       :weight bold :box t))
          ("@emergency"     . (:foreground "red"        :weight bold))
          ("bug"            . (:foreground "red2"       :weight bold))
          ("broken_code"    . (:foreground "DarkRed"    :weight bold))
          ("OBSTACLE"       . (:foreground "DarkOrange" :weight bold))

          ;; HIGH PRIORITY - Very visible (oranges, bright yellows)
          ("@spike"         . (:foreground "DarkOrange" :weight bold))
          ("@feature"       . (:foreground "OrangeRed"  :weight bold))
          ("new_work"       . (:foreground "gold"       :weight bold))

          ;; MEDIUM PRIORITY - Moderately visible (blues, purples, magentas)
          ("backend"        . (:foreground "MediumBlue"   :weight bold))
          ("frontend"       . (:foreground "purple"       :weight bold))
          ("refactor"       . (:foreground "MediumOrchid" :weight bold))
          ("@research"      . (:foreground "SteelBlue"    :weight bold))

          ;; LOW PRIORITY - Subtle colors (greens, cyans)
          ("@setup"         . (:foreground "ForestGreen" :weight bold))
          ("accomplishment" . (:foreground "LimeGreen"   :weight bold))

          ;; NEUTRAL/INFO - Muted colors (grays)
          ("@test"          . (:foreground "DimGray"))
          ("misc"           . (:foreground "gray60"))
	  ))

  ;; Agenda View "d"
  (defun air-org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.
    PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
        nil)))

  (setq org-agenda-skip-deadline-if-done t)

  (setq org-agenda-custom-commands
        '(
          ;; Daily Agenda & TODOs
          ("d" "Daily agenda and all TODOs"

           ;; Display items with priority A
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))

            ;; View 7 days in the calendar view
            (agenda "" ((org-agenda-span 7)))

            ;; Display items with priority B (really it is view all items minus A & C)
            (alltodo ""
                     ((org-agenda-skip-function '(or (air-org-skip-subtree-if-priority ?A)
                                                     (air-org-skip-subtree-if-priority ?C)
                                                     (org-agenda-skip-if nil '(scheduled deadline))))
                      (org-agenda-overriding-header "ALL normal priority tasks:")))

            ;; Display items with priority C
            (tags "PRIORITY=\"C\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "Low-priority Unfinished tasks:")))
            )

           ;; Don't compress things (change to suite your tastes)
           ((org-agenda-compact-blocks nil)))
          
          ("j" "TODO and Tags View"
           (
            (agenda ""
                    (
                     (org-agenda-remove-tags t)                                       
                     (org-agenda-span 7)
                     )
                    )

            (alltodo ""
                     (
                      ;; Remove tags to make the view cleaner
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format "  %t  %s")                    
                      (org-agenda-overriding-header "CURRENT STATUS")

                      ;; Define the super agenda groups (sorts by order)
                      (org-super-agenda-groups
                       '(
                         ;; Filter where tag is CRITICAL emergency broken_code bug
                         (:name "Emergency Tasks"
                                :tag ("CRITICAL" "@emergency" "broken_code" "bug")
                                :order 0
                                )
                         ;; Filter where TODO state is IN-PROGRESS
                         (:name "Currently Working"
                                :todo "IN-PROGRESS"
                                :order 10
                                )
                         ;; Filter where TODO state is PLANNING
                         (:name "Planning Next Steps"
                                :todo "PLANNING"
                                :order 20
                                )
                         ;; Filter where TODO state is BLOCKED or where the tag is OBSTACLE
                         (:name "Problems & Blockers"
                                :todo "BLOCKED"
                                :tag "OBSTACLE"
                                :order 30
                                )
                         ;; Filter where tag is @spike @feature
                         (:name "Features & Spikes"
                                :tag ("@spike" "@feature")
                                :order 40
                                )
                         ;; Filter where tag is @research
                         (:name "Research Required"
                                :tag "@research"
                                :order 70
                                )
                         ;; Filter where state is TODO and the priority is A
                         (:name "Other Important Items"
                                :and (:todo "TODO" :priority "A")
                                :order 90
                                )
                         ;; Filter where state is TODO and priority is B
                         (:name "General Backlog"
                                :and (:todo "TODO" :priority "B")
                                :order 100
                                )
                         ;; Filter where the priority is C or less (supports future lower priorities)
                         (:name "Non Critical"
                                :priority<= "C"
                                :order 110
                                )
                         ;; Filter where TODO state is VERIFYING
                         (:name "Currently Being Verified"
                                :todo "VERIFYING"
                                :order 200
                                )
			 ))))))))
  )

(use-package org-super-agenda
  :ensure t
  :after org
  :config
  (org-super-agenda-mode 1))

;; 已配置
;; M-<up>	将当前 headline 及其内容作为整体向上移动	 
;; M-<down>	将当前 headline 及其内容作为整体向下移动
;; M-<return>	在当前 headline 前建立一个同级 headline

;; 未配置
;; C-<return>	在当前 headline 所属的内容后建立一个同级 headline	无 headline 时创建一个一级 headline

;; M-<right>	降低当前 headline 的层级	 
;; M-<left>	提高当前 headline 的层级	 

;; 自定义TODO状态排序函数，支持优先级二级排序
(defun skemacs/org-sort-todo-keywords ()
  "按照预定义的TODO关键词顺序排序org条目, 同一TODO状态内按优先级排序"
  (interactive)
  (let ((todo-order '("IN-PROGRESS" "PLANNING" "TODO" "VERIFYING" "BLOCKED" "DONE" "OBE" "WONT-DO")))
    (org-sort-entries nil ?f
                      (lambda ()
                        (save-excursion
                          (org-back-to-heading t)
                          (let* ((todo-state (org-get-todo-state))
                                 (todo-index (if todo-state
                                                 (or (cl-position todo-state todo-order :test 'string=) 999)
                                               1000))
                                 ;; 直接获取当前行的优先级
                                 (priority-char (save-excursion
                                                  (beginning-of-line)
                                                  (if (re-search-forward "\\[#\\([ABCD]\\)\\]" (line-end-position) t)
                                                      (string-to-char (match-string 1))
                                                    nil)))
                                 ;; 将优先级字符转换为数字
                                 (priority-value (cond
                                                  ((eq priority-char ?A) 1)
                                                  ((eq priority-char ?B) 2)
                                                  ((eq priority-char ?C) 3)
                                                  ((eq priority-char ?D) 4)
                                                  (t 5))))
                            ;; 组合排序键：TODO状态索引*10 + 优先级值
                            (+ (* todo-index 10) priority-value))))
                      '<)))

;;; 200-org.el ends here
