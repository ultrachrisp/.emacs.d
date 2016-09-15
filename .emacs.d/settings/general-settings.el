;--------------------------------;
;;; General or Global Settings ;;;
;--------------------------------;

; set PATH, because we don't load .bashrc
; function from https://gist.github.com/jakemcc/3887459
;(defun set-exec-path-from-shell-PATH ()
;  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
;  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo -n $PATH'")))
;    (setenv "PATH" path-from-shell)
;    (setq exec-path (split-string path-from-shell path-separator))))

(setq exec-path (append exec-path '("/usr/local/bin")))

; language
(setq current-language-environment "English")

; don't show the startup screen
(setq inhibit-startup-screen 1)
; don't show the menu bar
;(menu-bar-mode 0)
; don't show the tool bar
;(require 'tool-bar)
;(tool-bar-mode 0)
; don't show the scroll bar
(if window-system (scroll-bar-mode 0))

;;---- below is almost a straight copy

; show the current line and column numbers in the stats bar as well
(line-number-mode 1)
(column-number-mode 1)

; don't blink the cursor
(blink-cursor-mode 0)

; make sure transient mark mode is enabled (it should be by default,
; but just in case)
(transient-mark-mode 1)

; highlight parentheses when the cursor is next to them
(require 'paren)
(show-paren-mode 1)

; text decoration
(require 'font-lock)
;(setq font-lock-maximum-decoration 1)
(global-font-lock-mode 1)
(global-hi-lock-mode nil)
(setq jit-lock-contextually 1)
(setq jit-lock-stealth-verbose 1)

; if there is size information associated with text, change the text
; size to reflect it
(size-indication-mode 1)

; disable backup
(setq backup-inhibited t)
; disable auto save
(setq auto-save-default nil)

;----------------------------------;
;;; Some added functions from me ;;;
;----------------------------------;

;; PGP
;(require 'org-crypt)
;(org-crypt-use-before-save-magic)
;(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;(setq org-crypt-key nil)

(require 'epa-file)
(epa-file-enable)
;(setq epg-gpg-program "gpg2")

;; format string used when creating CLOCKSUM lines and when generating a
;; time duration (avoid showing days)
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
	  
;; Setup for go
(setenv "GOPATH" "/Users/Chrisp/Development/gocode")

(setq exec-path (cons "/usr/local/opt/go/libexec" exec-path))
(add-to-list 'exec-path "/Users/Chrisp/Development/gocode/bin")

(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go generate && go build -v && go test -v && go vet"))
  ; Go guru
  ;(load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
  (load-file "$GOPATH/src/golang.org/x/tools/cmd/guru/go-guru.el")
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(custom-set-variables
 '(guru-command "/usr/local/opt/go/libexec/bin/guru"))

(provide 'general-settings)
