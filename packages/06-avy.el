;;; 06-avy.el --- Quick character-based navigation -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration sets up Avy for fast cursor movement and text
;; navigation. Avy allows you to quickly jump to any visible character,
;; word, or line by showing overlay hints. Includes integration with
;; Embark for context-sensitive actions.
;; avy: https://github.com/abo-abo/avy

;;; Code:

(use-package avy
  :ensure t
  :config
  (defun avy-action-embark (pt)
    (unwind-protect
	(save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?e avy-dispatch-alist) 'avy-action-embark)
  :bind
  (("M-g w" . avy-goto-word-1)))

;;; 06-avy.el ends here
