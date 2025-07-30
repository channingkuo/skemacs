;;; 08-multiple-cursors.el --- Multiple cursor editing with Hydra interface -*- lexical-binding: t -*-
;;; Commentary:

;; This configuration enables multiple-cursors for editing multiple
;; locations simultaneously.. Includes a comprehensive Hydra interface
;; that provides easy access to all multiple cursor operations like
;; marking next/previous, editing lines, and cursor management.
;; multiple-cursors: https://github.com/magnars/multiple-cursors.el

;;; Code:

(use-package multiple-cursors
  :ensure t
  :after hydra
  :bind
  ("C-S-<mouse-1>" . mc/toggle-cursor-on-click)
  :hydra (hydra-multiple-cursors
	  (:hint nil)
	  "
Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Prev     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
 [_|_] Align with input CHAR       [Click] Cursor at point"
	  ("l" mc/edit-lines :exit t)
	  ("a" mc/mark-all-like-this :exit t)
	  ("n" mc/mark-next-like-this)
	  ("N" mc/skip-to-next-like-this)
	  ("M-n" mc/unmark-next-like-this)
	  ("p" mc/mark-previous-like-this)
	  ("P" mc/skip-to-previous-like-this)
	  ("M-p" mc/unmark-previous-like-this)
	  ("|" mc/vertical-align)
	  ("s" mc/mark-all-in-region-regexp :exit t)
	  ("0" mc/insert-numbers :exit t)
	  ("A" mc/insert-letters :exit t)
	  ("<mouse-1>" mc/add-cursor-on-click)
	  ;; Help with click recognition in this hydra
	  ("<down-mouse-1>" ignore)
	  ("<drag-mouse-1>" ignore)
	  ("q" nil)))

;;; 110-multiple-cursors.el ends here
