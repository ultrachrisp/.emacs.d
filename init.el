;;;  -*- lexical-binding: t; -*-

;; If ~/.emacs.d/chrisp-init.el, just load it.  Otherwise, first
;; bootstrap Straight and load Org to make sure we end up with the
;; right Org version, then tangle ~/.emacs.d/init.org to
;; ~/.emacs.d/chrisp-init.el

(when (not (file-exists-p
            (expand-file-name "~/.emacs.d/chrisp-init.el")))
  (message "Bootstrapping init file...")
  (defvar bootstrapping-init t)
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el"
                           user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (straight-use-package 'org-plus-contrib)
  (require 'org)
  (org-babel-tangle-file
   (expand-file-name "~/.emacs.d/init.org")
   (expand-file-name "~/.emacs.d/chrisp-init.el")))

(load-file (expand-file-name "~/.emacs.d/chrisp-init.el"))
