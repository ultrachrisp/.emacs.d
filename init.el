;;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;;; Increase garbage collection threshold
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

;;; This is all kinds of necessary 
(setq package-enable-at-startup nil)

;;; Move emacs customizations to separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file) 

(require 'org)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
