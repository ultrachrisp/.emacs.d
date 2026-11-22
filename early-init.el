;;; early-init.el --- Early initialization settings -*- lexical-binding: t; -*-

;; Temporarily raise GC threshold during startup for faster boot
(defvar default-gc-cons-threshold (* 32 1024 1024))
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold default-gc-cons-threshold)))

;; Disable built-in package.el early so straight.el takes full control
(setq package-enable-at-startup nil)

;; Avoid unnecessary frame resizing when setting the initial font
(setq frame-inhibit-implied-resize t)

;; Prevent UI elements and font swaps from flashing during boot
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
;; JetBrains Mono on both macOS and Linux; Linux needs a smaller point size
;; for the same look. Emacs falls back to its default font if it's missing.
(push `(font . ,(if (eq system-type 'darwin) "JetBrains Mono-15" "JetBrains Mono-12"))
      default-frame-alist)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
