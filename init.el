;;; init.el --- Bootstrapper for config.org -*- lexical-binding: t; -*-

;; Ensure Emacs loads the most recent byte-compiled files
(setq load-prefer-newer t)

;; Fast IPC payload buffer size for LSP
(setq read-process-output-max (* 3 1024 1024))

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

;; use-package is built into Emacs 29+, so it doesn't need installing via straight
(use-package straight
  :custom
  (straight-use-package-by-default t))

;; Redirect auto-generated customizations to custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

;; Load Literate Config (using Emacs 31's built-in Org)
(require 'org)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
