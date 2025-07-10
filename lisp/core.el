;; core.el --- Basic defaults -*- lexical-binding:t; -*-

(setq-default
 cursor-type '(bar . 3)
 backup-directory-alist `(("." . "~/.backups"))
 ispell-dictionary "en_US"
 inhibit-startup-echo-area-message t
 inhibit-splash-screen t
 initial-scratch-message nil
 calendar-week-start-day 1)

(defun display-startup-echo-area-message () (message nil))

(provide 'core)
