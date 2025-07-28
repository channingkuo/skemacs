;; 检查下org版本问题
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
    ;; (org-mode-hook . org-bullets-mode))
  :config
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

  ;; (custom-theme-set-faces
  ;;   'user
  ;;   `(org-level-8 ((t (, :inherit default :weight bold))))
  ;;   `(org-level-7 ((t (, :inherit default :weight bold))))
  ;;   `(org-level-6 ((t (, :inherit default :weight bold))))
  ;;   `(org-level-5 ((t (, :inherit default :weight bold))))
  ;;   `(org-level-4 ((t (, :inherit default :weight bold :height 1.1))))
  ;;   `(org-level-3 ((t (, :inherit default :weight bold :height 1.2))))
  ;;   `(org-level-2 ((t (, :inherit default :weight bold :height 1.3))))
  ;;   `(org-level-1 ((t (, :inherit default :weight bold :height 1.5))))
  ;;   `(org-document-title ((t (, :inherit default :weight bold :height 1.6 :underline nil)))))

  (setq org-capture-templates
      '(    
        ("j" "Work Log Entry"
         entry (file+datetree "~/org/work-log.org")
         "* %?"
         :empty-lines 0)

        ("n" "Note"
         entry (file+headline "~/org/notes.org" "Random Notes")
         "** %?"
         :empty-lines 0)

        ("d" "Door Codes"
         entry (file+headline "~/org/notes.org" "Door Codes")
         "** %?"
         :empty-lines 0)

        ("g" "General To-Do"
         entry (file+headline "~/org/todos.org" "General Tasks")
         "* TODO [#B] %?\n:Created: %T\n "
         :empty-lines 0)

        ("e" "Emacs Config To-Do"
         entry (file+headline "~/org/emacs.org" "Emacs Config Tasks")
         "* TODO [#B] %?\n:Created: %T\n "
         :empty-lines 0)

        ("m" "Meeting"
         entry (file+datetree "~/org/meetings.org")
         "* %? :meeting:%^g \n:Created: %T\n** Attendees\n*** \n** Notes\n** Action Items\n*** TODO [#A] "
         :tree-type week
         :clock-in t
         :clock-resume t
         :empty-lines 0)

        ("c" "Code To-Do"
         entry (file+headline "~/org/todos.org" "Code Related Tasks")
         "* TODO [#B] %?\n:Created: %T\n%i\n%a\nProposed Solution: "
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

  ;; 为不同 TODO 状态的 headline 设置颜色的 hook
  (defun skemacs/org-set-todo-headline-colors ()
    "为不同 TODO 状态设置 headline 颜色"
    (font-lock-add-keywords nil
      '(("^\\*+ TODO \\(.*\\)$" 1 '(:foreground "GoldenRod" :weight bold) t)
        ("^\\*+ PLANNING \\(.*\\)$" 1 '(:foreground "DeepPink" :weight bold) t)
        ("^\\*+ IN-PROGRESS \\(.*\\)$" 1 '(:foreground "Cyan" :weight bold) t)
        ("^\\*+ VERIFYING \\(.*\\)$" 1 '(:foreground "DarkOrange" :weight bold) t)
        ("^\\*+ BLOCKED \\(.*\\)$" 1 '(:foreground "Red" :weight bold) t)
        ("^\\*+ DONE \\(.*\\)$" 1 '(:foreground "LimeGreen") t)
        ("^\\*+ OBE \\(.*\\)$" 1 '(:foreground "LimeGreen") t)
        ("^\\*+ WONT-DO \\(.*\\)$" 1 '(:foreground "LimeGreen") t))
      'append))
  (add-hook 'org-mode-hook 'skemacs/org-set-todo-headline-colors)

  ;; Tags
  (setq org-tag-alist '(
                        ;; Ticket types
                        (:startgroup . nil)
                        ("@bug" . ?b)
                        ("@feature" . ?u)
                        ("@spike" . ?j)                      
                        (:endgroup . nil)

                        ;; Ticket flags
                        ("@write_future_ticket" . ?w)
                        ("@emergency" . ?e)
                        ("@research" . ?r)

                        ;; Meeting types
                        (:startgroup . nil)
                        ("big_sprint_review" . ?i)
                        ("cents_sprint_retro" . ?n)
                        ("dsu" . ?d)
                        ("grooming" . ?g)
                        ("sprint_retro" . ?s)
                        (:endgroup . nil)

                        ;; Code TODOs tags
                        ("QA" . ?q)
                        ("backend" . ?k)
                        ("broken_code" . ?c)
                        ("frontend" . ?f)

                        ;; Special tags
                        ("CRITICAL" . ?x)
                        ("obstacle" . ?o)

                        ;; Meeting tags
                        ("HR" . ?h)
                        ("general" . ?l)
                        ("meeting" . ?m)
                        ("misc" . ?z)
                        ("planning" . ?p)

                        ;; Work Log Tags
                        ("accomplishment" . ?a)
                      ))


  ;; Tag colors
  (setq org-tag-faces
      '(
        ("planning"  . (:foreground "mediumPurple1" :weight bold))
        ("backend"   . (:foreground "royalblue1"    :weight bold))
        ("frontend"  . (:foreground "forest green"  :weight bold))
        ("QA"        . (:foreground "sienna"        :weight bold))
        ("meeting"   . (:foreground "yellow1"       :weight bold))
        ("CRITICAL"  . (:foreground "red1"          :weight bold))
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
          
          ;; James's Super View
          ("j" "James's Super View"
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
                        ;; Filter where tag is CRITICAL
                        (:name "Critical Tasks"
                                :tag "CRITICAL"
                                :order 0
                                )
                        ;; Filter where TODO state is IN-PROGRESS
                        (:name "Currently Working"
                                :todo "IN-PROGRESS"
                                :order 1
                                )
                        ;; Filter where TODO state is PLANNING
                        (:name "Planning Next Steps"
                                :todo "PLANNING"
                                :order 2
                                )
                        ;; Filter where TODO state is BLOCKED or where the tag is obstacle
                        (:name "Problems & Blockers"
                                :todo "BLOCKED"
                                :tag "obstacle"                              
                                :order 3
                                )
                        ;; Filter where tag is @write_future_ticket
                        (:name "Tickets to Create"
                                :tag "@write_future_ticket"
                                :order 4
                                )
                        ;; Filter where tag is @research
                        (:name "Research Required"
                                :tag "@research"
                                :order 7
                                )
                        ;; Filter where tag is meeting and priority is A (only want TODOs from meetings)
                        (:name "Meeting Action Items"
                                :and (:tag "meeting" :priority "A")
                                :order 8
                                )
                        ;; Filter where state is TODO and the priority is A and the tag is not meeting
                        (:name "Other Important Items"
                                :and (:todo "TODO" :priority "A" :not (:tag "meeting"))
                                :order 9
                                )
                        ;; Filter where state is TODO and priority is B
                        (:name "General Backlog"
                                :and (:todo "TODO" :priority "B")
                                :order 10
                                )
                        ;; Filter where the priority is C or less (supports future lower priorities)
                        (:name "Non Critical"
                                :priority<= "C"
                                :order 11
                                )
                        ;; Filter where TODO state is VERIFYING
                        (:name "Currently Being Verified"
                                :todo "VERIFYING"
                                :order 20
                                )
                      ))))))))
  )

;; 已配置
;; M-<up>	将当前 headline 及其内容作为整体向上移动	 
;; M-<down>	将当前 headline 及其内容作为整体向下移动
;; M-<return>	在当前 headline 前建立一个同级 headline

;; 未配置
;; C-<return>	在当前 headline 所属的内容后建立一个同级 headline	无 headline 时创建一个一级 headline

;; M-<right>	降低当前 headline 的层级	 
;; M-<left>	提高当前 headline 的层级	 