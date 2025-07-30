(require 'package)
(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
;; 设置 use-package 全部延迟加载，开启后无法控制加载顺序
;; (setq use-package-always-defer t)
(setq use-package-expand-minimally t)
;; 不加这一句可能有问题，建议读者尝试一下
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; 设置 HTTP/HTTPS/SOCKS5 代理
(setq url-proxy-services
      '(("http.*" . "127.0.0.1:7892")
        ("https.*" . "127.0.0.1:7892")
        ("socks5h://.*" . "127.0.0.1:7892")))

;; 加载 packages 对应的配置文件
(when (file-directory-p packages-config-dir)
  (let* ((config-files (directory-files packages-config-dir t "\\.el$" t)))
    (let* ((sorted-config-files (sort config-files 'string<)))
      (dolist (file sorted-config-files)
	(let ((time1 (current-time))
              (file-name (file-name-nondirectory file)))
          (skemacs/update-banner-status (format "Loading  %s..." file-name))
          (load file nil t)
          (let ((time2 (current-time))
		(time-delta (float-time (time-subtract (current-time) time1))))
            (when (> time-delta 0.5)
              (message "Loaded %s in %.6f seconds" file-name time-delta))
            (skemacs/update-banner-status (format "Loaded   %s in %.6f seconds" 
                                                  file-name
                                                  time-delta))))))))

;; 加载 local 对应的配置文件
(when (file-directory-p local-packages-config-dir)
  (let* ((config-files (directory-files local-packages-config-dir t "\\.el$" t)))
    (let* ((sorted-config-files (sort config-files 'string<)))
      (dolist (file sorted-config-files)
	(let ((time1 (current-time))
              (file-name (file-name-nondirectory file)))
          (skemacs/update-banner-status (format "Loading  %s..." file-name))
          (load file nil t)
          (let ((time2 (current-time))
		(time-delta (float-time (time-subtract (current-time) time1))))
            (when (> time-delta 0.5)
              (message "Loaded %s in %.6f seconds" file-name time-delta))
            (skemacs/update-banner-status (format "Loaded   %s in %.6f seconds" 
                                                  file-name
                                                  time-delta))))))))
