;;; 07-ace-window.el --- Numbered window navigation -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration sets up ace-window for efficient window switching.
;; It replaces the default C-x o behavior with numbered window selection,
;; allowing you to quickly jump to any visible window by pressing its
;; assigned number.

;;; Code:

(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))

;;; 100-ace-window.el ends here
