;; -*- lexical-binding: t; -*-

;; Constants
(setq-default cursor-type '(bar . 3))
(setq
 gc-cons-threshold 50000000
 linum-format " %3d "
 inhibit-startup-echo-area-message t
 calendar-week-start-day 1
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

;; Org mode
(setq org-directory (expand-file-name "Dropbox/Documents/org" (getenv "HOME")))

(setq
 org-hide-emphasis-markers t
 org-startup-indented t
 org-default-notes-file (concat org-directory "/quick.org")
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-src-window-setup 'current-window)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; recursively find .org files in provided directory
;; modified from an Emacs Lisp Intro example
(defun sa-find-org-file-recursively (&optional directory filext)
  "Return .org and .org_archive files recursively from DIRECTORY.
If FILEXT is provided, return files with extension FILEXT instead."
  (interactive "DDirectory: ")
  (let* (org-file-list
         (case-fold-search t)         ; filesystems are case sensitive
         (file-name-regex "^[^.#].*") ; exclude dot, autosave, and backupfiles
         (filext (or filext "org$\\\|org_archive"))
         (fileregex (format "%s\\.\\(%s$\\)" file-name-regex filext))
         (cur-dir-list (directory-files directory t file-name-regex)))
    ;; loop over directory listing
    (dolist (file-or-dir cur-dir-list org-file-list) ; returns org-file-list
      (cond
       ((file-regular-p file-or-dir)             ; regular files
        (if (string-match fileregex file-or-dir) ; org files
            (add-to-list 'org-file-list file-or-dir)))
       ((file-directory-p file-or-dir)
        (dolist (org-file (sa-find-org-file-recursively file-or-dir filext)
                          org-file-list) ; add files found to result
          (add-to-list 'org-file-list org-file)))))))

;; the files to be used for agenda display
(setq org-agenda-files
      (sa-find-org-file-recursively (concat org-directory "/agenda")))
