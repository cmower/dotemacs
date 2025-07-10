;; custom-funcs.el --- Helper utilities -*- lexical-binding:t; -*-

(defun find-org-file-recursively (&optional directory filext)
  "Return ORG (or FILEXT) files found recursively under DIRECTORY."
  (interactive "DDirectory: ")
  (let* ((filext (or filext "org\\|org_archive"))
         (rx (format "^[^.#].*\\.\\(%s\\)$" filext)))
    (directory-files-recursively (or directory default-directory) rx)))

(provide 'custom-funcs)
