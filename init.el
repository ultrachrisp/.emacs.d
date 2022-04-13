;;;  -*- lexical-binding: t; -*-

;; If ~/.emacs.d/chrisp-init.el, just load it.  Otherwise, first
;; bootstrap Straight and load Org to make sure we end up with the
;; right Org version, then tangle ~/.emacs.d/init.org to
;; ~/.emacs.d/chrisp-init.el

(when (not (file-exists-p
            (expand-file-name "~/.emacs.d/chrisp-init.el")))
  (require 'org)
  (org-babel-tangle-file
   (expand-file-name "~/.emacs.d/init.org")
   (expand-file-name "~/.emacs.d/chrisp-init.el")))

(load-file (expand-file-name "~/.emacs.d/chrisp-init.el"))
