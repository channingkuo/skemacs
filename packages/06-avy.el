;; https://github.com/abo-abo/avy
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
