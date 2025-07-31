;;; 00-hello.el --- Simple hello world demonstration function -*- lexical-binding: t -*-
;;; Commentary:

;; This file provides a simple hello world function as a basic example
;; of local custom functions. Demonstrates the pattern for adding
;; custom interactive commands to the Emacs configuration.

;;; Code:

(defun hello-world ()
  (interactive)
  (message "Hello, world!"))

(provide 'hello)

;;; 00-hello.el ends here
