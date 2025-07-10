;; -*- lexical-binding: t; -*-

;; Add ~/emacs.d/lisp to `load-path`
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

;; Load the modules in the order you want
(require 'core)         ;; low-level defaults & sanity tweaks
(require 'packages)     ;; package archives + use-package bootstrap
(require 'ui)           ;; appearance & UX
(require 'org-config)   ;; org-mode and agenda rules
(require 'programming)  ;; language-specific extras (Python, etc.)
(require 'keybindings)  ;; global keymaps
(require 'custom-funcs) ;; helper functions you wrote

;; Keep user customisations out of git
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)
