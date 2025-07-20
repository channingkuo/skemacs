;; ace-window 对 C-x o 重新绑定，使用时可以为每个 window 编个号，用编号进行跳转
(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))