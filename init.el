;; Load straight.el to manage package installation:
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

;; use-package is a macro that simplifies installing and loading packages.
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setenv "GPG_AGENT_INFO" nil)

;; Load org-mode early to avoid a version clash.
(use-package org
  :init (setq org-startup-indented t)
  :ensure org-plus-contrib
  :commands (org-element-map)
  :mode (("\\.org\\'" . org-mode)))

(org-babel-load-file "~/.emacs.d/README.org")

