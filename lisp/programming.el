;; programming.el --- Language helpers -*- lexical-binding:t; -*-

(use-package python-black
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(use-package yaml-mode  :mode "\\.ya?ml\\'")
(use-package cmake-mode :mode "CMakeLists\\.txt\\'")
(use-package clang-format :commands clang-format-buffer)

(setq pdf-open-application
      (cond ((eq system-type 'gnu/linux) "evince")
            ((eq system-type 'darwin)    "open")))

;; Magit – the Git porcelain inside Emacs
(use-package magit
  :defer t                      ; load on first use
  :commands (magit-status)
  :bind (("C-x g" . magit-status))) ; the classic shortcut

(provide 'programming)
