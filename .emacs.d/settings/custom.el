
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(python-check-command "/usr/local/bin/pyflakes"))

(load-theme 'wombat)
; color theme
;(add-to-list 'custom-theme-load-path (make-plugin-path "color-theme-solarized"))
;(load-theme 'solarized 1)
;(setq solarized-termcolors 256)

;; does the trick of exporting org files to html
(defun chrisp-org2html ()
  "used for converting the org buffer to an html file"
  (interactive)
  (setq org-export-headline-levels 9)
  (setq org-export-with-sub-superscripts nil)
  (browse-url-of-buffer (switch-to-buffer (command-execute 'org-html-export-as-html))))
  ;(add-hook 'org-mode-hook 'chrisp-org2html)

;; these lines below create the dblock, and populate it
(defun chrisp-org2tsv ()
  "Turn estimate into tsv file"
  (interactive)
  (org-create-dblock '(:name "columnview" :hlines 1 :id local))
  (org-update-dblock)
  (search-forward "#+END:")
  (setq maxpoint (point))
  (search-backward "#+BEGIN:")
  (while (re-search-forward "*" nil t)
       (replace-match "|"))
  (search-backward "#+BEGIN:")
  (next-line)
  (next-line)
  (org-table-export "~/TEMPTSV.tsv" "orgtbl-to-tsv")
  (find-file "~/TEMPTSV.tsv"))
;(add-hook 'org-mode-hook 'gloo-org2tsv)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
