;; control use of local variables in files you visit
;; :safe means set the safe variables, and ignore the rest
;; https://emacs.stackexchange.com/a/38
(setq enable-local-variables :safe)

;; Load Emacs.org
;; https://emacs.stackexchange.com/a/3147
(org-babel-load-file (expand-file-name "Emacs.org" user-emacs-directory))

;; Finish up
(message "finished Emacs initialization")
