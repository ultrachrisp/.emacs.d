;;; init.el --- Bootstrapper for config.org -*- lexical-binding: t; -*-

;; Ensure Emacs loads the most recent byte-compiled files
(setq load-prefer-newer t)

;; Fast IPC payload buffer size for LSP
(setq read-process-output-max (* 3 1024 1024))

;; Temporarily disable GC during startup for max speed
(defvar default-gc-cons-threshold (* 50 1024 1024))
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold default-gc-cons-threshold)))

;; Redirect auto-generated customizations to custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

;; Load Literate Config
(require 'org)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
