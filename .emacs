;; path where settings files are kept
(add-to-list 'load-path "~/.emacs.d/settings")

;; path to where plugins are kept
(setq plugin-path "~/.emacs.d/el-get/")

;; define various custom functions
(require 'custom-functions)

;; configure general settings
(require 'general-settings)

;; install dependencies with el-get
(require 'el-get-settings)

;---------------;
;;; Utilities ;;;
;---------------;

;; Git
;(include-plugin "magit")
;(require 'magit)

;; Popup
(include-elget-plugin "popup")
(require 'popup)

;; Auto complete
(require 'auto-complete-settings)

;; Websocket
(include-plugin "websocket")
(require 'websocket)

;; Request
(include-plugin "request")
(require 'request)

;-----------;
;;; Modes ;;;
;-----------;

;; Markdown mode
(require 'markdown-settings)

(require 'package)

;;; Standard package repositories
(when (< emacs-major-version 24)
  ;; Mainly for ruby-mode
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")))

;; We include the org repository for completeness, but don't normally
;; use it.
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;;; Also use Melpa for most packages
(add-to-list 'package-archives `("melpa" . ,(if (< emacs-major-version 24)
                                                "http://melpa.org/packages/"
                                              "https://melpa.org/packages/")))
											  
;; Go auto-complete
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

(with-eval-after-load 'go-mode
   (require 'go-autocomplete))

;; sets paths   
(package-initialize)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Python mode
;;(package-initialize)
;;(elpy-enable)
;(require 'python-settings)

;---------------------------------------------------------------------
;; Put auto 'custom' changes in a separate file (this is stuff like
;; custom-set-faces and custom-set-variables)
(load 
 (setq custom-file (expand-file-name "settings/custom.el" user-emacs-directory))
 'noerror)
