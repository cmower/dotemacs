;; programming.el --- Language helpers -*- lexical-binding:t; -*-

(use-package python-black
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(use-package yaml-mode  :mode "\\.ya?ml\\'")
(use-package cmake-mode :mode "CMakeLists\\.txt\\'")
(use-package clang-format :commands clang-format-buffer)

(setq pdf-open-application
      (cond ((eq system-type 'gnu/linux) "evince")
            ((eq system-type 'darwin)    "open")))

;; Jedi for python
(use-package jedi
  :ensure t
  :hook (python-mode . jedi:setup)
  :config
  (setq jedi:complete-on-dot t    ; complete on typing "."
        jedi:use-shortcuts t))    ; enable M-. and M-,

;; Magit – the Git porcelain inside Emacs
(use-package magit
  :defer t                      ; load on first use
  :commands (magit-status)
  :bind (("C-x g" . magit-status))) ; the classic shortcut

(use-package python-black
  :demand t                          ;; load immediately so the hook exists
  :after python                      ;; ensure python-mode is already loaded
  :hook (python-mode . python-black-on-save-mode-enable-dwim)
  ;; Optional tweaks:
  ;; :custom
  ;; (python-black-extra-args '("--line-length" "100"))
  )

(add-hook 'python-mode-hook #'python-black-on-save-mode)

(provide 'programming)
