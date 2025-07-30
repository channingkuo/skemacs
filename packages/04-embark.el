;;; 04-embark.el --- Context-sensitive action dispatcher -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration sets up Embark, which provides context-sensitive
;; actions on completion candidates. It allows you to act on targets
;; at point or in the minibuffer with appropriate commands based on
;; the type and context of the target.
;; embark: https://github.com/oantolin/embark

;;; Code:

(use-package embark
  :ensure t
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings))) ;; alternative for `describe-bindings'

;;; 04-embark.el ends here
