;;; early-init.el --- Early initialization settings -*- lexical-binding: t; -*-

;; Disable built-in package.el early so straight.el takes full control
(setq package-enable-at-startup nil)

;; Prevent UI elements from rendering during boot to avoid window flashing
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
