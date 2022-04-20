;;;  -*- lexical-binding: t; -*-

;;(when (not (file-exists-p
;;            (expand-file-name "~/.emacs.d/chrisp-init.el")))
(require 'org)
  (org-babel-tangle-file
   (expand-file-name "~/.emacs.d/README.org")
   (expand-file-name "~/.emacs.d/setup.el"))
;;  )

(load-file (expand-file-name "~/.emacs.d/setup.el"))
