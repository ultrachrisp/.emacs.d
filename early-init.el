;;; early-init.el --- Early initialization settings -*- lexical-binding: t; -*-

;; Disable built-in package.el early so straight.el takes full control
(setq package-enable-at-startup nil)

;; Log native-compilation warnings silently instead of popping up *Warnings*
(setq native-comp-async-report-warnings-errors 'silent)

;; Prevent UI elements from rendering during boot to avoid window flashing
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
