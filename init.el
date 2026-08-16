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

;; Use straight to load packages
(setq straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package straight
  :custom
  (straight-use-package-by-default t))

;; Use latest version of org before the default can be used for the config file
(straight-use-package 'org)

;; Redirect auto-generated customizations to custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

;; Load Literate Config
(require 'org)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
