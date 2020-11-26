;;; init.el -*- lexical-binding: t -*-


;;; Custom

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


;;; General initial setup

;; save backups in emacs.d
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "backups/" user-emacs-directory) t)))

(blink-cursor-mode -1)

(setq inhibit-startup-screen t)
;; Must be its own line
(setq inhibit-startup-echo-area-message "leodag")


(setq
 ;; Amount of lines to keep above/below point
 scroll-margin 5
 ;; A value over 100 implies never recentering
 scroll-conservatively 101
 hscroll-margin 5
 hscroll-step 1)

(setq-default indent-tabs-mode nil)


;;; Windows

(prefer-coding-system 'utf-8-unix)

(when (eq system-type 'windows-nt)
  (defvar w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0 ; default in emacs 27
        ring-bell-function 'ignore))


;;; Fonts

(defvar monospace-font "Fira Mono"
  "Preferred monospace font")
(defvar monospace-serif-font "Linux Libertine"
  "Preferred monospace serif font")
(defvar sans-serif-font "Fira Sans"
  "Preferred sans serif font")

(defun set-font-if-installed (face font)
  (if (member monospace-font (font-family-list))
      (set-face-attribute face nil :family font)
    (message "Font %s not installed!" font)))

(defun do-set-font ()
  (when window-system
    (set-font-if-installed 'fixed-pitch-serif monospace-serif-font)
    (set-font-if-installed 'variable-pitch sans-serif-font)
    (set-font-if-installed 'default monospace-font)
    (remove-hook 'server-after-make-frame-hook 'do-set-font)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'do-set-font)
  (do-set-font))


;;; Package management setup

;; TODO: finish package.el
(eval-when-compile
  (require 'package nil t)
  (add-to-list 'load-path (concat user-emacs-directory "straight/repos/straight.el"))
  (require 'straight nil t))

(eval-and-compile
  (defvar package-manager 'straight
    "Package manager to be used by use-package")

  (pcase package-manager
    ('straight
     ;; Straight bootstrapping and configuration
     (let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
           (_bootstrap-version 3))
       (unless (file-exists-p bootstrap-file)
         (with-current-buffer
             (url-retrieve-synchronously
              "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
              'silent 'inhibit-cookies)
           (goto-char (point-max))
           (eval-print-last-sexp)))
       (load bootstrap-file nil 'nomessage))

     (straight-use-package 'use-package)
     (require 'use-package)
     (setq straight-use-package-by-default t))

    ;; not really working well
    ('package
     (require 'package)
     (setq package-enable-at-startup nil)
     (add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/"))
     (package-initialize t)

     (unless (package-installed-p 'use-package)
       (package-refresh-contents)
       (package-install 'use-package))

     (require 'use-package)
     (setq use-package-always-ensure t))))

(setq use-package-compute-statistics t)


;;; Utility functions

(defun split-window-vertically-and-switch ()
  "After splitting the window, also switch to it."
  (interactive)
  (select-window (split-window-vertically)))

(defun split-window-horizontally-and-switch ()
  "After splitting the window, also switch to it."
  (interactive)
  (select-window (split-window-horizontally)))

(defun set-frame-alpha (value)
  "Set the transparency of the frame. 0 = transparent/100 = opaque"
  (interactive "nAlpha value (0-100=opaque): ")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun edit-init ()
  "Edit init.el"
  (interactive)
  (find-file (find-lisp-object-file-name 'edit-init nil)))

(defun reload-init ()
  "Reload init.el"
  (interactive)
  (load-file (find-lisp-object-file-name 'edit-init nil)))

(defun back-to-indentation-or-beginning ()
  (interactive "^")
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(defun back-to-beginning-or-indentation ()
  (interactive "^")
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(global-set-key (kbd "<home>") 'back-to-beginning-or-indentation)


;;; Theming

;; Lower underline, plays nicely with solarized's modeline
(setq x-underline-at-descent-line t)

;; Small indicator on lines that don't exist at the end of the file
(setq-default indicate-empty-lines t)

;; Truncate only if window is narrow
(setq-default truncate-lines nil)
(setq truncate-partial-width-windows 140)

(use-package solarized-theme
  :config
  (add-to-list 'custom-theme-load-path solarized-theme-dir)
  (load-theme 'my-solarized-light t))

;; Allows minor mode indicator manipulation
(use-package delight :defer t)


;;; Utilities

(use-package tramp :defer t :straight nil)

(use-package sudo-edit :defer t)

(use-package ag :defer t)
(use-package rg
  :bind ("C-c s" . rg-menu))

(use-package all-the-icons :defer t)


;;; General setup

(use-package simple :straight nil
  :delight auto-fill-function
  :hook ((text-mode emacs-lisp-mode) . auto-fill-mode)
  :config
  (setq-default fill-column emacs-lisp-docstring-fill-column)
  (setq comment-auto-fill-only-comments t
        save-interprogram-paste-before-kill t
        suggest-key-bindings nil))

(use-package subword
  :delight
  :config
  (global-subword-mode))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package delsel
  :config
  (delete-selection-mode))

(use-package smart-delete
  :straight (:host github :repo "leodag/smart-delete")
  :hook (prog-mode . smart-delete-mode))

(use-package mwheel :straight nil
  :config
  (setq mouse-wheel-progressive-speed nil))

(use-package whitespace
  :delight whitespace-mode
  :hook ((prog-mode text-mode) . whitespace-mode)
  :config
  (setq-default whitespace-style
                '(face trailing tabs newline empty space-after-tab
                       space-before-tab tab-mark)))

;; Whitespace cleaning (trailing whitespaces mostly)
(use-package ws-butler
  :delight
  :hook (prog-mode . ws-butler-mode))

;; uses a strike-through face for ^L
(use-package ff-st
  :straight (:host github :repo "leodag/ff-st")
  :delight
  :config
  (global-ff-st-mode))

(use-package vlf
  :no-require
  :config
  (require 'vlf-setup))

(use-package so-long
  :config
  (setq so-long-action 'so-long-minor-mode)
  (global-so-long-mode))

(use-package tab-bar
  :bind (("C-S-t" . tab-new-to)
         ("C-S-w" . tab-close)
         ([C-next] . tab-next)
         ([C-prior] . tab-previous)
         ([C-S-next] . tab-move)
         ([C-S-prior] . tab-move-prev))
  :config
  (defun tab-move-prev (&optional arg)
    "Move the current tab ARG positions to the left.
If a negative ARG, move the current tab ARG positions to the right.
You should use tab-move for that instead, though."
    (interactive "p")
    (tab-move (- arg)))

  (setq tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-show 1
        tab-bar-close-tab-select 'left
        tab-bar-new-tab-to 'rightmost
        tab-bar-new-tab-choice nil))

(use-package tab-pad
  :straight (:host github :repo "leodag/tab-pad")
  :config
  (tab-pad-bar-mode))

(use-package display-line-numbers
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type t
        display-line-numbers-grow-only t))

;; Buffer layout history (C-c <left>/<right>)
(use-package winner
  :config
  (winner-mode 1))

(use-package treemacs
  :disabled
  :defer 2
  :bind (("<f8>" . treemacs-select-or-deselect)
         ("S-<f8>" . treemacs-follow-and-select)
         :map treemacs-mode-map
         ([mouse-1] . treemacs-single-click-expand-action))
  :config
  (defun treemacs-select-or-deselect ()
    "Opens treemacs if it is not already open, select if it is visible but not
selected, and select last selected window if it is selected."
    (interactive)
    (if (treemacs-is-treemacs-window-selected?)
        (select-window (get-mru-window))
      (treemacs-select-window)))

  (defun treemacs-follow-and-select ()
    "Opens treemacs if it is not open, follow current file and select treemacs
window."
    (interactive)
    (if (treemacs--find-project-for-buffer)
        (progn
          (treemacs-find-file)
          (treemacs-select-window))
      (user-error "Current file is not in treemacs' workspace")))

  (when (eq system-type 'windows-nt)
    (setq treemacs-python-executable "python"))

  (let ((font (sans-font 9)))
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

  (setq treemacs-indentation 2)

  (treemacs-follow-mode -1)
  (treemacs-filewatch-mode 1)
  (treemacs-fringe-indicator-mode 1)
  (treemacs-git-mode
   (pcase system-type
     ('gnu/linux 'deferred)
     ('windows-nt 'simple))))

;; Auto-reload modified files; warn on overlapping changes
(use-package autorevert
  :delight auto-revert-mode
  :config
  (global-auto-revert-mode))

;; Show undo history in a tree
(use-package undo-tree
  :delight
  :config
  (global-undo-tree-mode))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/markz-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Show key bindings
(use-package which-key
  :delight
  :config
  ;; TODO: not monospace
  ;;(add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
  ;;(add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
  ;;(add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
  ;;(add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil))))
  (which-key-mode 1))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-show-empty-filter-groups nil))

(use-package ibuffer-vc
  :after ibuffer
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root)
  :bind (:map ibuffer-mode-map
              ("/ V" . ibuffer-vc-set-filter-groups-by-vc-root)))

;;; select/swap/delete windows with (C-u)* M-o
(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package ivy
  :delight
  :config
  (ivy-mode 1)
  (setq ivy-count-format "(%d/%d) "
        ivy-use-selectable-prompt t)
  (setf (alist-get t ivy-format-functions-alist) 'ivy-format-function-default))

(use-package ivy-hydra :after ivy)

(use-package counsel
  :defer nil
  :after ivy
  :delight
  :bind ("M-X" . set-variable)
  :config
  (counsel-mode 1))

;; Better I-search with ivy
(use-package swiper
  :bind ("C-s" . swiper))

(use-package projectile
  :after which-key
  :bind-keymap* ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (setq projectile-project-search-path '("~/proj/")
        projectile-completion-system 'ivy
        projectile-indexing-method 'alien)
  (which-key-add-key-based-replacements
    "C-c p"   "projectile"
    "C-c p 4" "other-window"
    "C-c p 5" "other-frame"
    "C-c p s" "search"
    "C-c p x" "execute"))

(use-package projectile-header-line
  :straight (:host github :repo "leodag/projectile-header-line")
  :config
  (global-projectile-header-line-mode))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1))

;; Makes buffer names be unique
(use-package uniquify :straight nil
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package hideshow
  :delight hs-minor-mode
  :hook (prog-mode . hs-minor-mode))

;; Highlights parentheses enclosing point
(use-package highlight-parentheses
  :delight
  :hook (prog-mode . highlight-parentheses-mode)
  :custom-face
  (highlight-parentheses-highlight ((t (:weight bold))))
  :config
  ;; Only highlights as many levels of parens as the length of
  ;; this list nil makes the face be applied, but color unaltered
  ;; since we want rainbow-delimiters to do the coloring
  ;; Also set in my-solarized-theme for reasons described there,
  ;; if both values do not match will cause unexpected results
  ;; when reloading theme
  (setq highlight-parentheses-colors '(nil nil nil nil nil nil nil nil)))

;; Highlight matching paren
(use-package paren
  :hook (prog-mode . show-paren-mode)
  :config
  (setq show-paren-delay 0
        show-paren-when-point-inside-paren t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight color names
(use-package rainbow-mode
  :delight
  :hook prog-mode)

;; Highlight changed regions on undo/paste
(use-package volatile-highlights
  :delight
  :config
  (volatile-highlights-mode 1))

;; Directional window selection
(use-package windmove
  :bind* (("M-<up>" . windmove-up)
          ("M-<down>" . windmove-down)
          ("M-<right>" . windmove-right)
          ("M-<left>" . windmove-left)
          ("C-M-r" . split-window-horizontally-and-switch)
          ("C-M-d" . split-window-vertically-and-switch)))

;; Creates matching closing delimiter
(use-package elec-pair
  :hook (prog-mode . electric-pair-local-mode)
  :config
  (setq-default
   electric-pair-inhibit-predicate
   `(lambda (c)
      (if (char-equal c ?\")
          t
        (,electric-pair-inhibit-predicate c)))))


;;; General programming

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read
        magit-diff-refine-hunk t)
  (setq magit-repository-directories
        '(("~/proj" . 1)
          ("~/proj/pd" . 1))))

(use-package magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package diff-hl
  :config
  (diff-hl-margin-mode 1)
  (diff-hl-flydiff-mode 1)
  (diff-hl-dired-mode 1)
  (global-diff-hl-mode 1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package flycheck :defer 1
  :after which-key
  :bind-keymap ("C-c !" . flycheck-command-map)
  :config
  (global-flycheck-mode)
  (which-key-add-key-based-replacements "C-c !" "flycheck")
  ;; Remove annoying init.el warning
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  ;; TODO: make this work and exclude yamllint-document-start in yamllint's use
  ;;(add-to-list (default-value 'flycheck-disabled-checkers) 'emacs-lisp-checkdoc)
  ;; All this just to limit height
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 12))))

(use-package eldoc
  :delight
  :config
  (setq eldoc-idle-delay 0.1))

(use-package company :defer nil
  :delight
  :bind* ("C-<tab>" . #'company-manual-begin)
  :bind (:map company-active-map
              ("M-." . #'company-show-location)
              ("C-<tab>" . #'company-select-next)
              ("C-S-<tab>" . #'company-select-previous))
  :config
  (setq company-idle-delay 1.5
        company-tooltip-minimum-width 30
        company-tooltip-width-grow-only t
        company-tooltip-limit 15)
  (global-company-mode))

(use-package company-box
  :delight
  :hook (company-mode . company-box-mode))

;;(use-package org :defer t)


;;; Docker setup

(use-package docker
  :defer t
  :commands (docker))

(use-package dockerfile-mode :defer t)

;;; YAML setup
(use-package yaml-mode :defer t)

(use-package flycheck-yamllint
  :hook (flycheck-mode . flycheck-yamllint-setup))

;;; Jenkinsfile setup
(use-package groovy-mode)


;;; Common Lisp setup

(use-package slime :defer t
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package slime-company
  ;; slime--setup-contribs does (unless (feture-p c)), so we can't require here
  :no-require
  :after (slime company)
  :config
  (add-to-list 'slime-contribs 'slime-company t)
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space))


;;; Rust setup

(use-package toml-mode :defer t)

(use-package rust-mode :defer t)

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package racer
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode)))


;;; Elixir setup

(use-package lsp-mode :defer t
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (elixir-mode . lsp))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-clients-elixir-server-executable "elixir-ls"))

(use-package lsp-ivy :defer t)

(use-package elixir-mode :defer t)

(use-package alchemist
  :disabled
  :after (elixir-mode which-key)
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

(use-package cider :after clojure-mode)


;;; C++ setup

(use-package irony
  :hook ((c-mode    . irony-mode)
         (c++-mode  . irony-mode)
         (objc-mode . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options)))

(use-package irony-eldoc
  :disabled
  :hook (irony-mode . irony-eldoc))

(use-package company-irony
  :after irony
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :after irony
  :config
  (add-to-list 'company-backends 'company-irony-c-headers))

;; Disabled because it causes eager loading of irony
(use-package flycheck-irony
  :disabled
  :hook (flycheck-mode . flycheck-irony-setup))


;;; C# setup

(use-package omnisharp
  :after (flycheck company)
  :hook ((csharp-mode . omnisharp-mode)
         (csharp-mode . lonely-brace-mode))
  :config
  (add-to-list 'company-backends 'company-omnisharp)
  ;; (setq omnisharp-debug t)
  (setq omnisharp-expected-server-version "1.35.3"))


;;; Python setup

(use-package elpy
  :hook (python-mode . elpy-mode)
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-modules (delq 'elpy-module-yasnippet elpy-modules))
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
  (elpy-enable))


;;; JavaScript/TypeScript setup

(use-package ts-mode :defer t)

(use-package tide
  :hook (((typescript-mode js-mode) . tide-setup)
         ((typescript-mode js-mode) . eldoc-mode)
         (typescript-mode . tide-hl-identifier-mode)))

(use-package prettier-js :defer t
  :commands (prettier-js))


;;; Scratch

(use-package explain-pause-mode
  :disabled ; causes weird delays
  :straight (:host github :repo "lastquestion/explain-pause-mode")
  :config
  (explain-pause-mode 1))

(use-package lonely-brace-mode
  :straight (:host github :repo "leodag/lonely-brace-mode"))

;;; init.el ends here
