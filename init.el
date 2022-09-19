;;;  -*- lexical-binding: t; -*-
;;(add-to-list 'load-path "~/.emacs.d")
  (defvar bootstrap-version)
  (unless (boundp 'bootstrapping-init)
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 5))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage)))

  (straight-use-package 'use-package)
(setq straight-use-package-by-default t)

  (use-package org
    :init (setq org-startup-indented t)
    :ensure org-plus-contrib
    :commands (org-element-map)
    :mode (("\\.org\\'" . org-mode)))

    ;; Annoying that this is necessary...
    ;; (require 'org)
    ;; (require 'org-refile)
    ;; (require 'org-protocol)

(org-babel-load-file "README.org")

;;(when (not (file-exists-p
;;            (expand-file-name "~/.emacs.d/chrisp-init.el")))
;;(require 'org)
;;  (org-babel-tangle-file
;;   (expand-file-name "~/.emacs.d/README.org")
;;   (expand-file-name "~/.emacs.d/setup.el"))
;;  )

;; (load-file (expand-file-name "~/.emacs.d/setup.el"))
