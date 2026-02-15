;;; init-web.el --- Web development modes -*- lexical-binding: t -*-
;;; Commentary:

;; Web-mode for .vue/.html files, TypeScript mode for .ts/.tsx files.
;; Indentation set to 2 spaces (frontend standard).

;;; Code:

(use-package web-mode
  :ensure t
  :mode
  (("\\.vue\\'" . web-mode)
   ("\\.html\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0))

(use-package typescript-mode
  :ensure t
  :mode "\\.tsx?\\'"
  :config
  (setq typescript-indent-level 2))

(provide 'init-web)
;;; init-web.el ends here
