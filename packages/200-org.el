(use-package org
  :config
  (setq font-lock-ensure t)
  (setq org-log-done 'time)
  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))
  (setq org-return-follows-link t)
  (setq calendar-week-start-day 1)
  
  (setq org-src-window-setup 'split-window-right)
  (setq org-directory "~/org")
  
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))
;; (use-package org
;;   :config
;;   (setq font-lock-ensure t)
;;   (setq org-log-done 'time)
;;   (setq org-log-into-drawer "LOGBOOK")
;;   (setq-default org-display-custom-times t)
;;   (setq org-time-stamp-custom-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))
;;   (setq org-startup-indented t)
;;   (setq org-return-follows-link t)
;;   (setq org-pretty-entities t)
;;   (setq org-pretty-entities-include-sub-superscripts t)
;;   (setq org-hide-emphasis-markers t)
;;   (setq org-agenda-block-separator "")
;;   (setq org-fontify-whole-heading-line t)
;;   (setq org-fontify-done-headline t)
;;   (setq org-fontify-quote-and-verse-blocks t)
;;   (setq org-startup-with-inline-images t)
;;   ;; (setq org-image-actual-width '(500))
;;   (setq org-startup-folded 'showall)
;;   (setq org-edit-src-content-indentation 0)
;;   (setq org-html-link-org-files-as-html t)
;;   (setq org-src-fontify-natively t)
;;   (setq calendar-week-start-day 1)

;;   (setq org-src-window-setup 'current-window) ;; or 'split-window-right
;;   (setq org-confirm-babel-evaluate nil)

;;   (setq org-directory "~/org")
;;   (setq org-directory-publish "~/org/www")
;;   (setq my-work-notes-directory "~/org/work/notes")
;;   (setq my-work-notes-publish-directory "~/org/work/publish")

;;   (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

(use-package org-bullets
  :defer 20
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "⁑" "⁂" "❖" "✮" "✱" "✸"))
  (setq org-ascii-bullets '((ascii ?* ?+ ?-) (latin1 ?* ?+ ?-) (utf-8 ?* ?+ ?-))))

(use-package org-appear
  :config
  (setq org-appear-autolinks t)
  :hook (org-mode . org-appear-mode))

(use-package org-reverse-datetree)