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
;;(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)

;; Load org-mode early to avoid a version clash.
;; (use-package org
;;   :init (setq org-startup-indented t)
;;   :ensure org-plus-contrib
;;   :commands (org-element-map)
;;   :mode (("\\.org\\'" . org-mode)))

;; (org-babel-load-file "~/.emacs.d/README.org")

(require 'org)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-types '((lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
