;; org-config.el --- Org-mode setup -*- lexical-binding:t; -*-

(use-package org
  :init
  (setq org-directory "~/org"
        org-hide-emphasis-markers t
        org-startup-indented t
        org-agenda-remove-tags t
        org-log-done 'time
        org-log-repeat 'time
        org-deadline-warning-days 0
        org-default-notes-file (expand-file-name "quick.org" org-directory)
        ;; ─ Agenda defaults ─
        org-agenda-span 'week                 ; show Mon–Sun
        org-agenda-start-on-weekday 1
        org-agenda-include-all-todo t         ; pull in unscheduled TODOs
        org-agenda-todo-ignore-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-files (directory-files-recursively org-directory "\\.org$"))
  :config
  ;; convenience hooks
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'flyspell-mode)

  ;; custom keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "DELEGATED(g)" "|"
                    "DONE(d)" "CANCELLED(c)")))

  ;; custom command: completed this week
  (setq org-agenda-custom-commands
        '(("D" "What I completed this week"
           agenda ""
           ((org-agenda-start-on-weekday 1)
            (org-agenda-span 'week)
            (org-agenda-start-with-log-mode t)
            (org-agenda-log-mode-items '(closed))
            (org-agenda-entry-types '(:closed))
            (org-agenda-overriding-header "✓ Completed this week\n"))))))

;;; --- Export helper: insert DELEGATED_TO property into LaTeX --------------
(defun my-org--insert-delegated-to ()
  (org-map-entries
   (lambda ()
     (when-let ((who (org-entry-get nil "DELEGATED_TO")))
       (save-excursion
         (end-of-line)
         (insert (format "\n\\textbf{Delegated to:} %s\n" who)))))))

(defun my-org-export-insert-delegated-to (backend)
  (when (org-export-derived-backend-p backend 'latex)
    (my-org--insert-delegated-to)))

(add-hook 'org-export-before-parsing-hook
          #'my-org-export-insert-delegated-to)

(setq org-export-with-properties nil
      org-export-with-drawers nil)

(provide 'org-config)
