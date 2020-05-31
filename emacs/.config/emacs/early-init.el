;;; early-init.el -*- lexical-binding: t -*-

;; Many things taken from doom-emacs

(let ((gc-ct gc-cons-threshold)
      (gc-cp gc-cons-percentage)
      (fha file-name-handler-alist))
  (add-hook 'after-init-hook
            (lambda ()
              (setq gc-cons-threshold gc-ct
                    gc-cons-percentage gc-cp
                    file-name-handler-alist fha))))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.8
      file-name-handler-alist nil)

(setq package-enable-at-startup nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)
