;; -*- lexical-binding: t; -*-

;;;; Workaround Mutter bug ----------------------------------------------------
;; https://chatgpt.com/share/686fe958-0de4-8009-a710-5591f250e136
(setq initial-frame-alist
      '((top . 40) (left . 40) (width . 120) (height . 50)))
(setq default-frame-alist initial-frame-alist)

(setq package-enable-at-startup nil   ; we'll init straight after start
      gc-cons-threshold (* 50 1000 1000)
      frame-inhibit-implied-resize t)

;; UI off ASAP (avoids flicker)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
