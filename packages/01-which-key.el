;;; 01-which-key.el --- Interactive key binding discovery and display -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration enables which-key mode, which displays available
;; key bindings in a popup when you start typing a key sequence.
;; Essential for discovering Emacs functionality and learning key bindings.

;;; Code:

(use-package which-key
  :ensure t
  :init (which-key-mode))

;;; 01-which-key.el ends here
