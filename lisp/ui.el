;; ui.el --- Fonts, theme, look-and-feel -*- lexical-binding:t; -*-

;; Font & minor visual tweaks
(add-to-list 'default-frame-alist '(font . "Comic Code-12"))
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(show-paren-mode 1)
(global-hl-line-mode 1)
(add-to-list 'default-frame-alist '(internal-border-width . 6))

;; Theme
(setq custom-theme-directory (locate-user-emacs-file "themes"))
(load-theme 'mymy t)

;; Line numbers in prog buffers
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :custom (display-line-numbers-width 3))

;;; ───────────────────────────────────────────────────────────────────────────
;;; Minimal mode-line:  FILENAME ………  CUR/TOTAL
;;; ───────────────────────────────────────────────────────────────────────────
(setq-default
 mode-line-format
 '((:eval
    (let* ((name (file-name-nondirectory
                  (or buffer-file-name (buffer-name))))
           (cur  (line-number-at-pos))
           (tot  (line-number-at-pos (point-max)))
           (info (format " %d/%d " cur tot)))
      (list
       (propertize (concat " " name " ")
                   'face 'mode-line-buffer-id)
       (propertize " " 'display
                   `((space :align-to (- right-fringe ,(length info)))))
       info)))))

(provide 'ui)
