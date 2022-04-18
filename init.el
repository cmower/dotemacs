;; Load Emacs.org
;; https://emacs.stackexchange.com/a/3147
(org-babel-load-file (expand-file-name "Emacs.org" user-emacs-directory))

;; Finish up
(message "finished Emacs initialization")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook 'org-html-export-to-html t t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
