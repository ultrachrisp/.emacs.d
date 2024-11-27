(setq vc-follow-symlinks t
      frame-resize-pixelwise t
      tab-always-indent 'complete
      enable-recursive-minibuffers t
      read-process-output-max (* 1024 1024)
      bookmark-save-flag 1)

(add-to-list 'image-types 'svg)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "SHELL")
        exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(cd "~")
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
'(safe-local-variable-values '((eval add-hook 'after-save-hook 'org-babel-tangle 0 t)))

(use-package which-key
  :config
  (which-key-mode))

(make-directory (expand-file-name "~/.emacs.d/autosaves") t)
(setq auto-save-default nil)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t)))
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(defun reload-init-file ()
  (interactive)
  (load-file "~/.emacs.d/README.el")
  (funcall major-mode))

(defun find-init-file ()
  (interactive)
  (find-file "~/.emacs.d/README.org"))

(require 'epg)
(setq epg-pinentry-mode 'loopback)

(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

(setq display-line-numbers-type t)

(defun toggle-line-number-mode ()
  (interactive)
  (when display-line-numbers
    (if (eq display-line-numbers 'visual)
        (progn
          (setq display-line-numbers t)
          (setq display-line-numbers-type t))
      (progn
        (setq display-line-numbers 'visual)
        (setq display-line-numbers-type 'visual)))))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook #'display-line-numbers-mode)

(use-package ace-window
  :bind      ("C-x o" . ace-window))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-city-lights t)
  (doom-themes-org-config))

(tool-bar-mode -1)
(menu-bar-mode -1)
(set-scroll-bar-mode nil)
(setq inhibit-startup-screen t)
(setq default-frame-alist '((font . "Monaco-15")))

(defconst indent-size 2)
(setq-default tab-width indent-size)
(setq-default indent-tabs-mode nil)
(setq-default indent-line-function 'insert-tab)

(setq org-element-use-cache nil)

(setq org-confirm-babel-evaluate nil)

(setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)" "|" "WAITING(w)")
                          (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
                          (sequence "|" "CANCELED(c)")))

(setq org-tag-alist '(("@orientation" . ?a)
                      ("@coding" . ?b)
                      ("@help" . ?c)
                      ("@phone" . ?d)
                      ("@documentation" . ?e)
                      ("@meeting" . ?f)
                      ("@email" . ?g)
                      ("@break" . ?h)
                      ("@study" . ?i)
                      ("@slack" . ?j)
                      ("@chat" . ?k)
                      ))

(setq org-columns-default-format '"%40ITEM(Task) %10TAGS %17Effort(Estimated Effort){:} %CLOCKSUM %CLOCKSUM_T")

(setq org-global-properties '(("Effort_ALL". "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00 16:00 24:00 32:00 40:00")))

(setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(use-package magit
  :bind ("C-x g" . magit-status)
  :commands (magit-status
             magit-blame
             magit-find-file
             magit-name-local-branch))

(use-package helm
  :ensure t
  :config    
  (setq helm-ff-transformer-show-only-basename nil
        helm-adaptative-history-file           "~/.emacs.d/data/helm-adaptative-history-file"
        helm-boring-file-regexp-list           '("\\.git$" "\\.svn$" "\\.elc$")
        helm-yank-symbol-first                 t
        helm-buffers-fuzzy-matching            t
        helm-ff-auto-update-initial-value      t
        helm-input-idle-delay                  0.1
        helm-idle-delay                        0.1)

  :bind (("C-x r l" . helm-bookmarks)
         ("C-x C-m" . helm-M-x)
         ("C-h i"   . helm-google-suggest)
         ("M-y"     . helm-show-kill-ring)
         ("C-h a"   . helm-apropos)
         ("C-x C-f" . helm-find-files)
         ("C-x p"   . helm-top)
         ("C-x C-b" . helm-buffers-list)))

(use-package projectile
  :commands (projectile-find-file
             projectile-grep
             projectile-switch-project
             projectile-project-root)
  :config
  (projectile-mode))

(use-package helm-projectile
  :bind      ("C-c h" . helm-projectile))

(use-package company
  :defer 0.1
  :config
  (global-company-mode t)
  (setq-default company-idle-delay 0.5
                company-require-match nil
                company-minimum-prefix-length 0
                company-show-numbers t

                company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend)
                ))

(use-package flycheck
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
  (global-flycheck-mode))

(use-package treemacs
  :ensure t
  :defer t
  :init  (with-eval-after-load 'winum
           (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package aggressive-indent
  :hook ((clojure-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode)
         (scheme-mode . aggressive-indent-mode)))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (before-save . lsp-format-buffer)
   (before-save . lsp-organize-imports))
  :commands lsp-mode lsp)

(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package mhtml-mode
  :mode (("\\.htm\\'" . mhtml-mode)
         ("\\.html\\'" . mhtml-mode))
  :hook ((mhtml-mode . emmet-mode)
         (mhtml-mode . lsp-deferred)))

(use-package emmet-mode
  :hook (emmet-mode . lsp-deferred))

(use-package css-mode
  :mode (("\\.css\\'" . css-mode)
         ("\\.scss\\'" . css-mode)
         ("\\.sass\\'" . css-mode))
  :hook (css-mode . lsp-deferred))

(use-package typescript-mode
  :mode (("\\.js\\'" . typescript-mode)
         ("\\.jsx\\'" . typescript-mode)
         ("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :custom (typescript-indent-level indent-size)
  :hook ((typescript-mode . emmet-mode)
         (typescript-mode . lsp-deferred)))

(use-package json-mode
  :mode (("\\.json\\'" . json-mode))
  :custom (js-indent-level indent-size)
  :hook (json-mode . lsp-deferred))

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package php-mode
  :mode (("\\.php\\'" . php-mode))
  :hook (php-mode . lsp-deferred))

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :hook (go-mode . lsp-deferred))

(use-package ccls
  :ensure
  :config
  '(ccls-initialization-options (quote (compilationDatabaseDirectory :build)))
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package ispell
  :init      (defun ispell-line()
               (interactive)
               (ispell-region (line-beginning-position) (line-end-position)))
  :bind      (("C-c sr" . ispell-region)
              ("C-c sb" . ispell-buffer)
              ("C-c sw" . ispell-word)
              ("C-c sl" . ispell-line)))

(setq ispell-program-name "/usr/bin/aspell")
;; (setq ispell-program-name "/opt/homebrew/bin/aspell")

(use-package writegood-mode)

(use-package olivetti
  :config
  (setq-default olivetti-body-width 100)
  (setq olivetti-body-width 100)
  :commands olivetti-mode)
