;; -*- lexical-binding: t; -*-
(setq package-enable-at-startup nil   ; we'll init straight after start
      gc-cons-threshold (* 50 1000 1000)
      frame-inhibit-implied-resize t)

;; UI off ASAP (avoids flicker)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
