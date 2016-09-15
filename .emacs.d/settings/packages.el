;(require 'package)
;;; Standard package repositories
;(when (< emacs-major-version 24)
  ;; Mainly for ruby-mode
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")))

;; We include the org repository for completeness, but don't normally
;; use it.
;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

;(when (< emacs-major-version 24)
;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;;; Also use Melpa for most packages
;(add-to-list 'package-archives `("melpa" . ,(if (< emacs-major-version 24)
;                                                "http://melpa.org/packages/"
;                                              "https://melpa.org/packages/")))

;; Trying different package manager

;(setq package-enable-at-startup nil)
;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
;(package-initialize)

;(unless (package-installed-p 'use-package)
;  (package-refresh-contents)
;  (package-install 'use-package))
;(require 'use-package)
 
;(provide 'packages)

;; the package manager
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                   ; ("melpa-stable" . "http://stable.melpa.org/packages/")
		    ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
		    ))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Ensime
(use-package ensime
  :pin melpa-stable)
   
;; Markdown-mode
(use-package markdown-mode
  :defer t
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
  :config)
   
(use-package auto-complete
  :diminish auto-complete-mode
  :config
  (progn
    (use-package go-autocomplete)
    (add-to-list 'ac-dictionary-directories (emacs-d "elpa/auto-complete-20160416.604emacs/dict"))
    (setq ac-use-fuzzy t
	  ac-disable-inline t
          ac-use-menu-map t
          ac-auto-show-menu t
          ac-auto-start t
	  ac-ignore-case t
          ac-candidate-menu-min 0)
;    (add-to-list 'ac-modes 'enh-ruby-mode)
    (add-to-list 'ac-modes 'web-mode)
    (add-to-list 'ac-modes 'go-mode)
;    (add-to-list 'ac-modes 'clojure-mode)
    ))

; Popup menus
(use-package popup
  ;; We don't ensure this package, because we definitely don't want to have this
  ;; mess, but unfortunately it's a dependency of Ensime :(
  :ensure nil
  :defer t
  :config
  ;; Bring Popup bindings in line with Company bindings, by getting rid of C-n/p
  ;; for navigation and introducing M-n/p
  (define-key popup-menu-keymap "\C-n" nil)
  (define-key popup-menu-keymap [down] nil)
  (define-key popup-menu-keymap "\C-p" nil)
  (define-key popup-menu-keymap [up] nil)
  (define-key popup-menu-keymap (kbd "M-n") #'popup-next)
  (define-key popup-menu-keymap (kbd "M-p") #'popup-previous))

;(package-initialize)
