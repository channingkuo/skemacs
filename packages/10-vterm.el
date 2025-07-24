(use-package vterm
  :ensure t
  :defer t
  :config
  (defun skemacs/vterm-in-project-root ()
    "Open vterm in project root if in a project, otherwise in current directory."
    (interactive)
    (let ((default-directory
           (or (when-let ((project (project-current)))
                 (project-root project))
               default-directory)))
      (call-interactively 'vterm)))
  
  ;; Use around advice to modify the default directory
  (defun skemacs/vterm-project-advice (orig-fun &rest args)
    "Advice to make vterm open in project root."
    (let ((default-directory
           (or (when-let ((project (project-current)))
                 (project-root project))
               default-directory)))
      (apply orig-fun args)))
  
  (advice-add 'vterm :around #'skemacs/vterm-project-advice))