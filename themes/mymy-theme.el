;;; mymy-theme.el --- Cleaned-up “MyMy” light theme

(deftheme mymy "A light, colourful theme with an emphasis on Org-mode and a simple UI.")


;;; User options --------------------------------------------------------------

(defgroup mymy nil
  "Options for the `mymy' theme."
  :group 'faces)

(defcustom mymy-scale-headings 1.2
  "Scale factor for Org/Outline level-1 headings."
  :type 'number)

(defun mymy--scale (factor)
  "Return a `:height' plist scaled by FACTOR."
  (list :height (truncate (* 100 factor))))


;;; Palette -------------------------------------------------------------------

(let* ((class '((class color) (min-colors 89)))
       (fg         "#333333")
       (bg         "#FFFFFF")
       (comment    "#8D8D84")
       (keyword    "#0000FF")
       (builtin    "#006FE0")
       (const      "#D0372D")
       (type       "#6434A3")
       (var        "#BA36A5")
       (str        "#008000")
       (accent     "#1662AF")
       (region     "#8ED3FF")
       (hl         "#FFFDDD")
       (modeline-bg "#335EA8")
       (modeline-fg "#85CEEB")
       (success    "#3A993A")
       (warning    "#F4A939")
       (error      "#FE251E"))

  (custom-theme-set-faces
   'mymy
   ;; ---- Core ---------------------------------------------------------------
   `(default                         ((,class (:foreground ,fg :background ,bg))))
   `(cursor                          ((,class (:background "#21BDFF"))))
   `(region                          ((,class (:background ,region))))
   `(hl-line                         ((,class (:background ,hl))))

   ;; ---- Font-lock ----------------------------------------------------------
   `(font-lock-builtin-face          ((,class (:foreground ,builtin))))
   `(font-lock-keyword-face          ((,class (:foreground ,keyword))))
   `(font-lock-function-name-face    ((,class (:foreground ,accent))))
   `(font-lock-variable-name-face    ((,class (:foreground ,var))))
   `(font-lock-type-face             ((,class (:foreground ,type))))
   `(font-lock-constant-face         ((,class (:foreground ,const))))
   `(font-lock-string-face           ((,class (:foreground ,str))))
   `(font-lock-comment-face          ((,class (:slant italic :foreground ,comment))))

   ;; ---- Modeline -----------------------------------------------------------
   `(mode-line                       ((,class (:box (:line-width 1 :color ,modeline-bg)
                                                    :foreground ,modeline-fg
                                                    :background ,modeline-bg))))
   `(mode-line-inactive              ((,class (:box (:line-width 1 :color "#9B9C97")
                                                    :foreground "#F0F0EF"
                                                    :background "#9B9C97"))))

   ;; ---- Org & outline ------------------------------------------------------
   `(org-level-1                     ((,class (:weight bold :foreground ,fg
                                       ,@(mymy--scale mymy-scale-headings)))))
   `(org-level-2                     ((,class (:weight bold :foreground ,fg))))
   `(org-code                        ((,class (:foreground "#000088" :background "#FFFFE0"))))
   `(org-block                       ((,class (:inherit org-code))))
   `(org-block-begin-line            ((,class (:foreground "#555555" :background "#E2E1D5"))))
   `(org-block-end-line              ((,class (:inherit org-block-begin-line))))
   `(org-link                        ((,class (:underline t :foreground "#006DAF"))))
   `(org-todo                        ((,class (:weight bold :foreground "red"  :background ,bg))))
   `(org-done                        ((,class (:weight bold :foreground "black" :background "#C8F5AB"))))

   ;; ---- Generic diagnostic faces ------------------------------------------
   `(success                         ((,class (:foreground ,success))))
   `(warning                         ((,class (:weight bold :foreground ,warning))))
   `(error                           ((,class (:weight bold :foreground ,error))))
   ))


;;; Footer --------------------------------------------------------------------

;;;###autoload
(when (and load-file-name (boundp 'custom-theme-load-path))
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'mymy)
;;; mymy-theme.el ends here
