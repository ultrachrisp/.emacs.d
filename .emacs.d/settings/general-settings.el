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

;; format string used when creating CLOCKSUM lines and when generating a
;; time duration (avoid showing days)
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(provide 'general-settings)
