;; MELPA issue:
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Load Emacs.org
;; https://emacs.stackexchange.com/a/3147
(org-babel-load-file (expand-file-name "Emacs.org" user-emacs-directory))

;; Finish up
(message "finished Emacs initialization")
