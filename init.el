;;; init.el --- Personal Emacs init  -*- lexical-binding: t; -*-
;;; Commentary:
;; A modular, Git‑tracked Emacs configuration with Copilot integration.
;; Each top‑level section is separated by a header made of semicolons
;; so that outline‑minor‑mode (`C-c C-o` in Prog‑mode) can fold them.

;;; Code:

;;;; Paths --------------------------------------------------------------------
;; Core Lisp directory (your own elisp files)
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

;; Automatically add every first‑level directory inside site-lisp/ (e.g. Git
;; submodules like copilot.el) to the load‑path.  Keeps cloning simple: just
;; `git submodule add … site-lisp/PKG`.
(let ((site-lisp (locate-user-emacs-file "site-lisp")))
  (when (file-directory-p site-lisp)
    (dolist (dir (directory-files site-lisp t "^[^.].*"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

;;;; Custom file --------------------------------------------------------------
;; Keep Custom‑UI changes out of version control.
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;;;; Core modules -------------------------------------------------------------
(require 'core)         ;; low-level defaults & sanity tweaks
(require 'packages)     ;; package archives + use-package bootstrap
(require 'ui)           ;; appearance & UX
(require 'org-config)   ;; org-mode and agenda rules
(require 'programming)  ;; language-specific extras (Python, etc.)
(require 'keybindings)  ;; global keymaps
(require 'custom-funcs) ;; helper functions you wrote

;;;; Copilot ------------------------------------------------------------------
;; The repository is expected at site-lisp/copilot.el (Git submodule).
(require 'copilot)

;; Enable in all programming buffers.
(add-hook 'prog-mode-hook #'copilot-mode)

;; Completion keys
(let ((map copilot-completion-map))
  (define-key map (kbd "TAB")        #'copilot-accept-completion)
  (define-key map (kbd "<tab>")      #'copilot-accept-completion)
  (define-key map (kbd "C-TAB")      #'copilot-accept-completion-by-word)
  (define-key map (kbd "C-<tab>")    #'copilot-accept-completion-by-word)
  (define-key map (kbd "C-n")        #'copilot-next-completion)
  (define-key map (kbd "C-p")        #'copilot-previous-completion))

;; Behaviour tweaks
(setq copilot-idle-delay 0.2)            ; Wait 200 ms before querying
(add-to-list 'copilot-indentation-alist '(prog-mode 2))
(add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))

(provide 'init)
;;; init.el ends here
