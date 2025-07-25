(defun skemacs/next-ten-lines()
  "Move cursor to next 10 lines."
  (interactive)
  (next-line 10))

(defun skemacs/previous-ten-lines()
  "Move cursor to previous 10 lines."
  (interactive)
  (previous-line 10))

(defun skemacs/split-window-vertically()
  "Split window vertically."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun skemacs/split-window-horizontally()
  "Split window horizontally."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun skemacs/reload-config()
  "Reload the Emacs configuration."
  (interactive)
  (load-file (concat user-emacs-directory "init.el"))
  (message "Emacs configuration reload Done."))

;; 复制选中内容到剪贴板
(defun skemacs/copy-to-clipboard ()
  (interactive)
  (cond
    ((eq system-type 'gnu/linux)
     (if (display-graphic-p)
         (progn
           (require 'xclip nil t)
           (if (featurep 'xclip)
               (progn
                 (xclip-mode 1)
                 (if (use-region-p)
                     (progn
                       (xclip-copy-region (region-beginning) (region-end))
                       (message "Yanked region to clipboard!")
                       (deactivate-mark))
                   (message "No region active; can't yank to clipboard!")))
             (message "xclip package not available")))
       (message "Not in graphical display")))
    
    ((eq system-type 'darwin)
     (if (use-region-p)
         (progn
           (shell-command-on-region (region-beginning) (region-end) "pbcopy")
           (message "Yanked region to clipboard!")
           (deactivate-mark))
       (message "No region active; can't yank to clipboard!")))
    
    ((eq system-type 'windows-nt)
     (if (use-region-p)
         (progn
           (shell-command-on-region (region-beginning) (region-end) "clip.exe")
           (message "Yanked region to clipboard!")
           (deactivate-mark))
       (message "No region active; can't yank to clipboard!")))
    
    (t (message "Unsupported system type"))))

;; Linux xclip package setup
(when (eq system-type 'gnu/linux)
  (use-package xclip
    :if (display-graphic-p)
    :config
    (xclip-mode 1)))

;; Window resize functions
(defun skemacs/shrink-window-horizontally (delta)
  "Shrink window horizontally by DELTA columns."
  (interactive "p")
  (shrink-window delta t))

(defun skemacs/enlarge-window-horizontally (delta)
  "Enlarge window horizontally by DELTA columns."
  (interactive "p")
  (enlarge-window delta t))

(defun skemacs/shrink-window (delta)
  "Shrink window vertically by DELTA lines."
  (interactive "p")
  (shrink-window delta))

(defun skemacs/enlarge-window (delta)
  "Enlarge window vertically by DELTA lines."
  (interactive "p")
  (enlarge-window delta))

;; 打开~/org目录
(defun skemacs/open-org-directory ()
  "Open the org directory."
  (interactive)
  (let ((org-dir "~/org/"))
    (if (file-directory-p org-dir)
        (dired org-dir)
      (message "Directory %s does not exist." org-dir))))