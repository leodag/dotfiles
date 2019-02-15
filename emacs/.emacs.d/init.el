(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; salva backups no emacs.d
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; tira frufru
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(setq inhibit-startup-screen t)
;; bota frufru
(setq-default indicate-empty-lines t) ; small indicator on lines that don't exist at the end of the file
(set-face-attribute 'default nil :font "Fira Mono-10")

(setq-default scroll-margin 4)
(setq-default scroll-step 1)

(setq-default indent-tabs-mode nil) ; use spaces instead of tabs
(setq whitespace-style '(face trailing tabs spaces newline empty space-after-tab space-before-tab tab-mark))
(global-whitespace-mode 1)
(set-face-attribute 'whitespace-tab nil :background "dim gray" :foreground "tan")

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;(setq use-package-always-ensure t)
(setq straight-use-package-by-default t)

(defun split-window-vertically-and-switch ()
  "After splitting the window, also switch to it."
  (interactive)
  (split-window-vertically)
  (other-window 1))
(defun split-window-horizontally-and-switch ()
  "After splitting the window, also switch to it."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun set-frame-alpha (value)
  "Set the transparency of the frame. 0 = transparent/100 = opaque"
  (interactive "nAlpha value (0-100): ")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun edit-init ()
  "Edit init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun reload-init ()
  "Reload init.el"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; Themes
(use-package solarized-theme
  :config
  (load-theme 'solarized-light t))

(use-package diminish)

;; General setup
(use-package evil
  :config
  (evil-mode 1))

(use-package nlinum-relative ; async relative line numbering
  :config
  (nlinum-relative-setup-evil)
  :hook (prog-mode . nlinum-relative-mode))

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-count-format "(%d/%d) ") ; display (current/total) instead of just total
  (setq ivy-format-function 'ivy-format-function-line) ; highlight the entire line
  (setq ivy-use-selectable-prompt t))

(use-package counsel
  :config
  (counsel-mode 1))

(use-package swiper ; better I-search
  :bind ("C-s" . swiper))

(use-package tramp :straight nil)

;; TODO: configure/make a .ssh
;; TODO: make the package not use package-installed-p
;(use-package counsel-tramp) ; tramp interface
;(use-package docker-tramp)
;(use-package vagrant-tramp)

(use-package projectile
  :after which-key
  :config
  (projectile-mode 1)
  (setq projectile-project-search-path '("~/proj/"))
  (setq projectile-completion-system 'ivy)
  (which-key-add-key-based-replacements "C-c p 4" "other-window"
                                        "C-c p 5" "other-frame"
                                        "C-c p s" "search"
                                        "C-c p x" "execute")
  :bind-keymap* ("C-c p" . projectile-command-map))

(use-package es-lib)
(use-package es-windows)

(use-package project-explorer)

(use-package counsel-projectile)

(use-package helm
  :config
  (helm-autoresize-mode 1)
  ;(setq helm-split-window-inside-p t) ; only split current buffer
  (setq helm-autoresize-min-height 25)
  (setq helm-autoresize-max-height 35)
  :bind (("C-x C-b" . helm-mini)
         ("C-x C-f" . helm-find-files)))

(use-package helm-company ; complete with helm
  :bind* ("C-:" . helm-company))

(use-package undo-tree ; undo history in a tree
  :config
  (global-undo-tree-mode 1))

(use-package which-key ; show prefix's key bindings
  :config
  (which-key-mode 1))
  ; TODO: not monospace
  ;(add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
  ;(add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
  ;(add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
  ;(add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil))))

(use-package all-the-icons-dired ; icons in dired
  :hook (dired-mode . all-the-icons-dired-mode))
  ; TODO: :if

(use-package uniquify :straight nil ; unique buffer names
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package highlight-parentheses ; TODO: cagado
  :config
  ;(set-face-attribute 'hl-paren-face nil :weight 'extra-bold)
  ;(setq hl-paren-attributes '(('weight 'extra-bold)))
  (setq hl-paren-colors nil)
  :hook (prog-mode . highlight-parentheses-mode))

(use-package rainbow-delimiters
  :config
  (set-face-attribute 'rainbow-delimiters-base-face nil :weight 'extra-bold) ; doesn't seem to work huh
  :hook ((prog-mode . rainbow-delimiters-mode)
         (prog-mode . show-paren-mode)))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package windmove ; directional movement
  :bind (("M-<up>"    . windmove-up   )
         ("M-<down>"  . windmove-down )
         ("M-<right>" . windmove-right)
         ("M-<left>"  . windmove-left )
         ("C-M-r"     . split-window-horizontally-and-switch)
         ("C-M-d"     . split-window-vertically-and-switch  )))

(use-package winner ; buffer layout history (C-c <left>/<right>)
  :config
  (winner-mode 1))

(use-package ws-butler ; cleans trailing whitespaces
  :hook (prog-mode . ws-butler-mode)
  :delight ws-butler-mode)

;; General programming
(use-package flycheck
  :after which-key
  :config
  (global-flycheck-mode 1)
  (which-key-add-key-based-replacements "C-c !" "flycheck-prefix")
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ; annoying init.el warning
  (add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . bottom)
              (reusable-frames . visible)
              (window-height   . 12)))) ; only really wanted to limit height tbh

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 1.5)
  :bind* ("C-<tab>" . company-complete-common-or-cycle))

;; Rust setup
(use-package toml-mode
  :mode "\\.toml\\'")

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package racer
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode)))

;; Elixir setup
(use-package alchemist
  :after which-key
  :config
  (which-key-add-major-mode-key-based-replacements 'elixir-mode
    "C-c a"     "alchemist"
    "C-c a m"   "mix"
    "C-c a m t" "mix-test"
    "C-c a X"   "hex"
    "C-c a c"   "compile"
    "C-c a e"   "execute"
    "C-c a p"   "project"
    "C-c a n"   "phoenix"
    "C-c a h"   "help"
    "C-c a i"   "iex"
    "C-c a v"   "eval"
    "C-c a o"   "macroexpand"
    "C-c a f"   "info"))

;; Clojure setup

(use-package cider)

;; C++ setup
(use-package irony
  :hook ((c-mode    . irony-mode)
         (c++-mode  . irony-mode)
         (objc-mode . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options)))

(use-package irony-eldoc
  :hook (irony-mode . irony-eldoc))

(use-package company-irony
  :after company
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :after company
  :config
  (add-to-list 'company-backends 'company-irony-c-headers))

(use-package flycheck-irony
  :after flycheck
  :hook (flycheck-mode . flycheck-irony-setup))
