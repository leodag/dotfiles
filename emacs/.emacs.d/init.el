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

;; save backups in emacs.d
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; remove interface elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(setq inhibit-startup-screen t)

(setq-default indicate-empty-lines t) ; small indicator on lines that don't exist at the end of the file
(set-face-attribute 'default nil :font "Fira Mono-10")

(setq-default scroll-margin 4)
(setq-default scroll-step 1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Straight bootstrapping and configuration
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

;; not needed with straight.el
;(setq use-package-always-ensure t)
(setq straight-use-package-by-default t)

;; Utility functions
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

;; Allows minor mode name manipulation
(use-package delight)

;;; Utilities
(use-package tramp :straight nil)

(use-package ag)
(use-package ripgrep)

(use-package all-the-icons)

;;; General setup
(use-package whitespace
  :delight global-whitespace-mode
  :custom-face (whitespace-tab ((t (:background "dim gray" :foreground "tan"))))
  :config
  (setq-default indent-tabs-mode nil) ; use spaces instead of tabs
  (setq whitespace-style
        '(face trailing tabs spaces newline empty space-after-tab
               space-before-tab tab-mark))
  (global-whitespace-mode 1))

(use-package display-line-numbers
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'relative))

;; Auto-reload modified files; warn on changes file
(use-package autorevert
  :delight auto-revert-mode
  :config
  (global-auto-revert-mode))

;; Show undo history in a tree
(use-package undo-tree
  :delight
  :config
  (global-undo-tree-mode 1))

;; vi keybindings
(use-package evil
  :config
  (evil-mode 1))

;; Show key bindings
(use-package which-key
  :delight
  :config
  (which-key-mode 1))
  ; TODO: not monospace
  ;(add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
  ;(add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
  ;(add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
  ;(add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil))))

(use-package ivy
  :delight
  :config
  (ivy-mode 1)
  (setq ivy-count-format "(%d/%d) ") ; display (current/total) instead of just total
  (setq ivy-format-function 'ivy-format-function-line) ; highlight the entire line
  (setq ivy-use-selectable-prompt t))

(use-package counsel
  :delight
  :bind ("M-X" . set-variable)
  :config
  (counsel-mode 1))

; Better I-search with ivy
(use-package swiper
  :bind ("C-s" . swiper))

;(use-package counsel-tramp) ; tramp interface
;(use-package docker-tramp)
;(use-package vagrant-tramp)

(use-package helm
  :bind (("C-x C-b" . helm-mini)
         ("C-x C-f" . helm-find-files))
  :config
  (helm-autoresize-mode 1)
  ;(setq helm-split-window-inside-p t) ; only split current buffer
  (setq helm-autoresize-min-height 25)
  (setq helm-autoresize-max-height 35))

; Complete with helm
(use-package helm-company
  :bind* ("C-:" . helm-company))

(use-package projectile
  :after which-key
  :bind-keymap* ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (setq projectile-project-search-path '("~/proj/"))
  (setq projectile-completion-system 'ivy)
  (which-key-add-key-based-replacements "C-c p 4" "other-window"
                                        "C-c p 5" "other-frame"
                                        "C-c p s" "search"
                                        "C-c p x" "execute"))

(use-package counsel-projectile)

(use-package treemacs
  :after evil
  :commands (treemacs-is-treemacs-window-selected? treemacs-select-window)
  :bind (([f8] . treemacs-open-select-or-close)
         :map treemacs-mode-map
         ([mouse-1] . treemacs-single-click-expand-action))
  :config
  (evil-set-initial-state 'treemacs-mode 'emacs)

  (let ((font "Fira Sans-9"))
    (set-face-attribute 'treemacs-root-face      nil :font font)
    (set-face-attribute 'treemacs-file-face      nil :font font)
    (set-face-attribute 'treemacs-directory-face nil :font font)
    (set-face-attribute 'treemacs-tags-face      nil :font font)
    (dolist
        (type '("added" "conflict" "ignored" "modified" "renamed" "unmodified" "untracked"))
      (set-face-attribute
       (intern (format "treemacs-git-%s-face" type))
       nil
       :font font)))

  (setq treemacs-indentation 1)

  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-fringe-indicator-mode 1)
  (treemacs-git-mode 'deferred))

(defun treemacs-open-select-or-close ()
  "Opens treemacs if it is not already open, select if it is visible but not selected, and closes it if it is selected."
  (interactive)
  (if (treemacs-is-treemacs-window-selected?)
      (delete-window (selected-window))
    (treemacs-select-window)))

;; Makes buffer names be unique
(use-package uniquify :straight nil
  :config
  (setq uniquify-buffer-name-style 'forward))

;; Highlights parentheses enclosing point
(use-package highlight-parentheses ; TODO: cagado
  :delight
  :hook (prog-mode . highlight-parentheses-mode)
  :custom-face (hl-paren-face ((t (:weight bold))))
  :config
  ;; only highlights as many levels of parens as the length of this list
  (setq hl-paren-colors '(nil nil nil nil nil nil)))

;; Highlight matching paren
(use-package paren
  :hook (prog-mode . show-paren-mode)
  :custom-face (show-paren-match ((t (:foreground "black")))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
  ;;; The following lines are not relevant anymore, but the bug is still
  ;;; present so they are kept for documentation purposes.
  ;; for sime reason, depth-%d faces don't actually inherit base-face,
  ;; even though it is specified when defined
  ;(set-face-attribute 'rainbow-delimiters-base-face nil :weight 'bold)
  ;; workaround for the above
  ;(dotimes (i 9)
  ;  (set-face-attribute
  ;   (intern (format "rainbow-delimiters-depth-%d-face" (1+ i)))
  ;   nil
  ;   :weight 'bold))

; Directional window selection
(use-package windmove
  :bind (("M-<up>"    . windmove-up   )
         ("M-<down>"  . windmove-down )
         ("M-<right>" . windmove-right)
         ("M-<left>"  . windmove-left )
         ("C-M-r"     . split-window-horizontally-and-switch)
         ("C-M-d"     . split-window-vertically-and-switch  )))

;; Whitespace cleaning (trailing whitespaces mostly)
(use-package ws-butler
  :delight
  :hook (prog-mode . ws-butler-mode))

;; Creates matching closing delimiter
(use-package elec-pair
  :hook (prog-mode . electric-pair-mode))

;; Buffer layout history (C-c <left>/<right>)
(use-package winner
  :config
  (winner-mode 1))

;;; General programming
(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

;; evil-magit needs transient, but can't find recipe
;(use-package transient)
;(use-package evil-magit)

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
                 (window-height   . 12)))) ; all this just to limit height

(use-package company
  :delight
  :hook (prog-mode . company-mode)
  :bind* ("C-<tab>" . company-complete-common-or-cycle)
  :config
  (setq company-idle-delay 1.5))

;;; Rust setup
(use-package toml-mode)

(use-package rust-mode)

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package racer
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode)))

;;; Elixir setup
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

;;; Clojure setup
(use-package cider)

;;; C++ setup
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

;;; Python setup
(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

(use-package company-anaconda)

;;;; SCRATCH

;; also not working, like the ones below
(use-package treemacs-projectile
  :bind (([M-f8] . treemacs-projectile)))

;; why doesn't this work?
;(use-package treemacs-evil)
;(use-package treemacs-magit)
;(use-package treemacs-icons-dired)

;;; C# setup
(use-package omnisharp
  :after flycheck company
  :hook ((csharp-mode . omnisharp-mode)
         (csharp-mode . flycheck-mode))
  :config
  (add-to-list 'company-backends #'company-omnisharp)
  (setq omnisharp-expected-server-version "1.32.18")
  (setq omnisharp-server-executable-path
        (concat user-emacs-directory "omnisharp/run"))
  (setq omnisharp-debug t))
