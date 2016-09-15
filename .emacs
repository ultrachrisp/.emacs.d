;; path where settings files are kept
(add-to-list 'load-path "~/.emacs.d/settings")

;; manually setting the exec-path
;(add-to-list 'exec-path "/usr/local/bin")
(setq exec-path (append exec-path '("/usr/local/bin")))
;(setq exec-path (append exec-path '("/usr/local/sbin")))
(setenv "PATH" (shell-command-to-string "/bin/bash -c 'echo -n $PATH'"))

;; path to where plugins are kept
(setq plugin-path "~/.emacs.d/el-get/")

;; define various custom functions
(require 'custom-functions)

;; configure general settings
(require 'general-settings)

;; install dependencies with el-get
(require 'el-get-settings)

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

;; external packages
(load (emacs-d "settings/packages"))

;---------------;
;;; Utilities ;;;
;---------------;

;; Git
;(include-plugin "magit")
;(require 'magit)

;; Popup
;(include-elget-plugin "popup")
;(require 'popup)

;; Auto complete
;(require 'auto-complete-settings)

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
;(require 'markdown-settings)

;(when (not package-archive-contents)
;  (package-refresh-contents)
;  (package-install 'use-package))
;(require 'use-package)

;; Go auto-complete
;(defun auto-complete-for-go ()
;  (auto-complete-mode 1))
;(add-hook 'go-mode-hook 'auto-complete-for-go)

;(with-eval-after-load 'go-mode
;   (require 'go-autocomplete))

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
