;; https://magit.vc/
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-auto-revert-mode nil)
  (setq magit-refresh-status-buffer nil))
;; C-x M-g 调用 magit-dispatch 。
;; d 选择 Diff 。
;; -- <tab> <enter> <enter> 自动填充当前文件名，表示我们只关注这个文件的异同。
;; r 选择 Diff range，因为我们想对比另一个 commit。
;; 输入 HEAD^ （表示同一分支的上一个 commit）/ 想要对比的 commit ID。可以按 <tab> 进行提示。