;; 覆盖 C-a 为 mwim-beginning-of-code-or-line，这样按一次 C-a 时移动到代码文字开头，再按一次则是移动到整行的行首
;; 覆盖 C-e 为 mwim-end-of-code-or-line，当本行代码结尾有注释时，第一次按 C-e 将光标移动到代码尾部、注释之前。再按一次则是移动到整行的行尾
(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))