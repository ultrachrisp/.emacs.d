;; This a sample emacs init file ~/.emacs or ~/.emacs.d/init.el

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ces-autojoin-mode 1)
 '(ces-env-variables
   (quote
    (("JAVA_HOME" . "/Library/Java/JavaVirtualMachines/jdk1.8.0_92.jdk/Contents/Home")
     ("GOPATH"    . "/Users/Chrisp/Development/gocode")
     ("PATH"      . "/Users/Chrisp/.nvm/versions/node/v7.4.0/bin/node")
     ("EDITOR" . "emacsclient")
     ("LC_ALL" . "C")
     ("LANG" . "en"))))
 '(ces-erc-channel-list (quote ((".*" "##java" "#freebsd" "#grails" "#emacs"))))
 '(ces-powerline-enabled t)
 '(ces-tab-size 4)
 '(ces-ui-font "Monospace-12")
 '(ces-ui-theme (quote wombat))
 '(ces-ui-theme-console (quote wombat))
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (sass-mode popwin smart-mode-line cl-lib-highlight smartparens multiple-cursors yasnippet yaml-mode yagist wrap-region web-mode use-package undo-tree rainbow-delimiters projectile markdown-mode magit json-mode js3-mode htmlize helm git-timemachine expand-region exec-path-from-shell emmet-mode duplicate-thing dsvn drag-stuff dired-details company-tern clojure-mode buffer-stack batch-mode anzu ag ace-window ace-jump-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; require few libs
(mapc #'require '(org ob-tangle cl-lib))

;; Just for testing, for the typical setup see the README file, use the full path to startup.org..
(org-babel-load-file (expand-file-name
                      (concat
                       (file-name-as-directory
                        (concat (file-name-as-directory (file-name-directory load-file-name)) "bootstrap"))
                       "startup.org")))
