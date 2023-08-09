;; -*- lexical-binding: t; -*-

;; Constants
(setq-default cursor-type '(bar . 3))
(setq
 gc-cons-threshold 50000000
 linum-format " %3d "
 inhibit-startup-echo-area-message t
 inhibit-splash-screen t
 backup-directory-alist `(("." . "~/.backups"))
 ispell-dictionary "en_US"
 pdf-open-application
 (cond
  ((string-equal system-type "gnu/linux") "evince")
  ((string-equal system-type "darwin") "open")))

;; Startup
(defun display-startup-echo-area-message ()
  (message nil))

;; Font
;; * https://dtinth.github.io/comic-mono-font/
;; * https://tosche.net/fonts/comic-code
(add-to-list 'default-frame-alist '(font . "Comic Mono-12" ))

;; Misc appearance
(scroll-bar-mode 0) ;; remove scroll bar
(tool-bar-mode 0) ;; remove tool bar
(menu-bar-mode 0) ;; remove menu bar
(show-paren-mode 1) ;; highlight parentheses
(global-hl-line-mode 1) ;; highlight current line
(global-linum-mode 1) ;; show line numbers
(add-to-list 'default-frame-alist '(internal-border-width . 6))

;; Packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Theme
(setq custom-theme-directory "~/.emacs.d/themes")
(load-theme 'mymy t)
(set-face-foreground 'linum "#5D6B99")
(set-face-background 'linum "white")


;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Magit
(use-package magit :ensure t)

(defun magit-open-repo ()
  "Open remote repo URL."
  (interactive)
  (let ((url (magit-get "remote" "origin" "url")))
    (progn
      (browse-url (if (string-match "^http" url) url
		    (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
					      "https://\\2/\\3"
					      url)))
      (message "Opening %s" url))))

(add-hook 'magit-mode-hook
          (lambda ()
            (local-set-key (kbd "o") 'magit-open-repo)))

;; Helm
(use-package helm
  :ensure t
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files))
  :config
  (setq
   helm-mode-line-string nil
   helm-find-files-doc-header nil
   helm-display-mode-line nil
   )
  (fset 'helm-display-mode-line #'ignore)
  (add-hook 'helm-after-initialize-hook
	    (defun hide-mode-line-in-helm-buffer ()
	      "Hide mode line in `helm-buffer'."
	      (with-helm-buffer
		(setq-local mode-line-format nil)))))

;; AucTeX
(use-package auctex :defer t :ensure t)

;; Python programming
(use-package python-black
  :demand t
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

;; Additional modes
(use-package yaml-mode :ensure t)
(use-package cmake-mode :ensure t)
(use-package clang-format :ensure t)

;; Mode line
(defun my-custom-mode-line-format ()
  (let ((file-name (buffer-file-name)))
    (if file-name
        (concat "   " (file-name-nondirectory file-name) " [" mode-name "]")
      "")))

(setq-default mode-line-format '(:eval (my-custom-mode-line-format)))

;; Spelling
(add-hook 'org-mode-hook '(lambda () (flyspell-mode)))
(add-hook 'LaTeX-mode-hook '(lambda () (flyspell-mode)))

;; Use right-mouse button to correct spelling
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map (kbd "<mouse-3>") #'flyspell-correct-word)))
