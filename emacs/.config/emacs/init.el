;;; init.el --- leodag's init -*- lexical-binding: t -*-

;;; Commentary:

;; I'm just writing this to appease emacs-lisp-checkdoc.

;;; Code:


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
 hscroll-step 1
 scroll-preserve-screen-position t)

(setq-default indent-tabs-mode nil)

;; Remove some unwanted behaviours
(setq confirm-kill-emacs #'yes-or-no-p)
;; If I ever want to suspend-frame I can M-x
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

(setq mouse-autoselect-window t)

(defvar leodag-map (define-keymap)
  "My keymap.")

(keymap-global-set "C-z" leodag-map)


;;; Windows

;; Use LF by default
(prefer-coding-system 'utf-8-unix)

(when (eq system-type 'windows-nt)
  (defvar w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0 ; default in emacs 27
        ring-bell-function 'ignore))


;;; Fonts

(defvar monospace-font "Fira Mono"
  "Preferred monospace font.")
(defvar monospace-serif-font "Linux Libertine Mono"
  "Preferred monospace serif font.")
(defvar sans-serif-font "Fira Sans"
  "Preferred sans serif font.")
(defvar emoji-font "Noto Color Emoji"
  "Preferred emoji font.")

(defun font-installed-p (font)
  "Check if FONT is avaliable in the system."
  (member font (font-family-list)))

(defun set-font-if-installed (face font)
  "Set FONT as FACE's family if it is detected in the system."
  (if (font-installed-p font)
      (set-face-attribute face nil :family font)
    (message "Font %s not installed!" font)))

(defun set-fontset-font-if-installed (target font &optional add)
  "Set FONT as a fontset font for TARGET if installed.
See `set-fontset-font' for ADD."
  (if (font-installed-p font)
      (set-fontset-font t target font nil add)
    (message "Font %s not installed!" font)))

(defun do-set-font ()
  "Actually set fonts.
Will only run on the first creation of a graphic frame, otherwise
font presence cannot be detected."
  (when (display-multi-font-p)
    (set-font-if-installed 'fixed-pitch-serif monospace-serif-font)
    (set-font-if-installed 'variable-pitch sans-serif-font)
    (set-font-if-installed 'default monospace-font)
    (set-fontset-font-if-installed 'symbol emoji-font t)
    (remove-hook 'server-after-make-frame-hook 'do-set-font)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'do-set-font)
  (do-set-font))


;;; Package management setup

(eval-and-compile
  (defvar package-manager 'straight
    "Package manager to be used by use-package.")

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

     (require 'straight)
     (straight-use-package 'use-package))

    ;; not really working well
    ('package
     (require 'package)
     (add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/"))
     (package-initialize)

     (unless (package-installed-p 'use-package)
       (package-refresh-contents)
       (package-install 'use-package)))))

(require 'use-package)

(setq use-package-compute-statistics t)
(when (eq package-manager 'straight)
  (setq straight-use-package-by-default t))
(setq use-package-always-ensure (eq package-manager 'package))


;;; Utility functions

(defun split-window-vertically-and-select ()
  "After splitting the window, also switch to it."
  (interactive)
  (select-window (split-window-vertically)))

(defun split-window-horizontally-and-select ()
  "After splitting the window, also switch to it."
  (interactive)
  (select-window (split-window-horizontally)))

(defun set-frame-alpha (value)
  "Set the transparency of the frame to VALUE.
0 = transparent/100 = opaque."
  (interactive "nAlpha value (0-100=opaque): ")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun edit-init ()
  "Edit init.el."
  (interactive)
  (find-file (find-lisp-object-file-name 'edit-init nil)))

(defun reload-init ()
  "Reload init.el."
  (interactive)
  (load-file (find-lisp-object-file-name 'reload-init nil)))

(defun back-to-indentation-or-beginning ()
  "Go back to indentation or to the beginning of the line."
  (interactive "^")
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(defun back-to-beginning-or-indentation ()
  "Go back to the beginning of the line or to indentation."
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
(use-package delight :defer)


;;; Utilities

(use-package tramp :defer)

(use-package sudo-edit :defer)

(use-package all-the-icons :defer)


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
  (setq mouse-wheel-progressive-speed nil)
  (setf (car mouse-wheel-scroll-amount) 5))

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
You should use `tab-move' for that instead, though."
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

(use-package neotree
  ;; not using #' since that inhibits `require'ing
  :bind (([f8] . neotree-select-or-deselect)
         ([\S-f8] . neotree-find-in-projectile-root))
  :hook ((neo-enter . neo-hide-on-enter-file)
         (neo-after-create . neo-graphical-setup)
         (neo-after-create . neo-set-projectile-header-line))
  :config
  (setq neo-theme 'ascii
        neo-autorefresh nil
        neo-mode-line-type 'custom
        neo-mode-line-custom-format (propertize " Neotree" 'face 'mode-line-buffer-id)
        neo-show-updir-line nil
        neo-window-width 30)

  (defun neotree-select-or-deselect ()
    "Select or deselect (selecting MRU window) neotree window."
    (interactive)
    (if (eq (current-buffer) (neo-global--get-buffer))
        (select-window (get-mru-window))
      (neotree-show)))

  (defun neotree-find-in-projectile-root ()
    "Find file in neotree using projectile's project root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neo-global--open)
      (unless (and project-dir (neo-global--window-exists-p))
        (error "Failed to find projectile's root"))
      (setq neo-buffer--start-node project-dir)
      (neotree-refresh)
      (neotree-find file-name)))

  (defun neo-hide-on-enter-file (type _full-path arg)
    "Meant to be added as a `neo-enter-hook'.
If entering a file, hide neotree.  TYPE ARG"
    (when (and (not arg) (eq type 'file))
      (neotree-hide)))

  (defun neo-graphical-setup (&optional _window)
    "Set graphical configurations for neotree."
    (setq cursor-type 'bar
          indicate-empty-lines nil))

  (defun neo-set-projectile-header-line (&optional _window)
    "Set neotree's header line to show the current project.
Akin to `projectile-header-line''s behaviour."
    (setq header-line-format
          `((:propertize " " display (space :width 1))
            "["
            (:propertize (:eval (projectile-project-name neo-buffer--start-node))
                         face projectile-header-line-project)
            "]"))))

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
  :demand
  :delight
  :commands (which-key-add-major-mode-key-based-replacements
              which-key-add-keymap-based-replacements
              which-key-add-key-based-replacements)
  :config
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

(use-package selectrum
  :demand
  :bind (:map leodag-map
              ("r" . selectrum-repeat))
  :config
  (selectrum-mode 1))

(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h b" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package consult
  :bind (("M-X" . set-variable)
         ;; C-c bindings (mode-specific-map)
         ("<f5>" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ;; ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Custom M-# bindings for fast register access
         :map leodag-map
         ("z z" . consult-register-load)
         ("z s" . point-to-register)
         ("z r" . consult-register))
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 1 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  (setq consult-narrow-key "<")

  (setq consult-project-root-function #'projectile-project-root))

(use-package consult-flycheck
  :bind ("M-g f" . consult-flycheck))

(use-package marginalia
  :demand
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

(use-package ivy
  :defer
  :delight
  :config
  (setq ivy-count-format "%-4d "
        ivy-use-selectable-prompt t)
  (setf (alist-get t ivy-format-functions-alist) 'ivy-format-function-default))

(use-package ivy-hydra :after ivy)

(use-package swiper
  :defer
  :bind (("C-s" . #'swiper)
         ("C-S-s" . #'isearch-forward)
         :map swiper-map
         ("M-%" . #'swiper-query-replace)))

(use-package projectile
  :delight
  :bind (:map leodag-map
              ("C-f" . projectile-find-file))
  :bind-keymap* ("C-c p" . projectile-command-map)
  :commands (projectile-project-root)
  :config
  (projectile-mode 1)
  (setq projectile-project-search-path '("~/proj/")
        projectile-completion-system 'default
        projectile-indexing-method 'alien)
  (which-key-add-keymap-based-replacements global-map
    "C-c p" '("projectile" . projectile-command-map))
  (which-key-add-keymap-based-replacements projectile-command-map
    "4" "other-window"
    "5" "other-frame"
    "s" "search"
    "x" "execute"))

(use-package project-header-line
  :straight (:host github :repo "leodag/project-header-line")
  :config
  (global-project-header-line-mode))

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
          ("C-M-r" . split-window-horizontally-and-select)
          ("C-M-d" . split-window-vertically-and-select)))

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

(use-package ediff
  :straight nil
  :config
  ;; uses the same frame for control buffer
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package diff-hl
  :config
  (diff-hl-margin-mode 1)
  (diff-hl-flydiff-mode 1)
  (diff-hl-dired-mode 1)
  (global-diff-hl-mode 1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package flycheck
  :demand
  :bind-keymap ("C-c !" . flycheck-command-map)
  :config
  (global-flycheck-mode)
  (which-key-add-key-based-replacements "C-c !" "flycheck")
  ;; Remove annoying init.el warning
  ;;(cl-pushnew 'emacs-lisp-checkdoc (default-value 'flycheck-disabled-checkers))
  ;; All this just to limit height
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 12))))

(use-package flycheck-posframe
  :hook ((flycheck-mode . flycheck-posframe-mode)
         (flycheck-posframe-mode . flycheck-posframe-configure-pretty-defaults))
  :config
  (setq posframe-mouse-banish nil
        flycheck-posframe-position 'window-bottom-left-corner
        flycheck-posframe-border-width 1))

(use-package eldoc
  :delight
  :config
  (setq eldoc-idle-delay 0.1))

(use-package xref :ensure t
  :bind ([remap xref-find-definitions] . #'ace-xref-find-definitions)
  :config
  (defun ace-xref-find-definitions (arg)
    "Find definition, in other window or other frame with ARG.
Finds definition in this window, or in other window with one
argument, or in other frame with two arguments."
    (interactive "p")
    (let* ((command (cl-case arg
                      (1
                       'xref-find-definitions)
                      (4
                       'xref-find-definitions-other-window)
                      (16
                       'xref-find-definitions-other-frame)))
           ;; `xref--read-identifier' checks these variables and
           ;;  will prompt for an identificer if they are incorrect
           (this-command command)
           (current-prefix-arg nil))
      (call-interactively command))))

(use-package company
  :demand
  :delight
  :bind* ("C-<tab>" . #'company-manual-begin)
  :bind (:map company-active-map
              ("M-." . #'company-show-location)
              ("C-<tab>" . #'company-select-next)
              ("C-S-<iso-lefttab>" . #'company-select-previous))
  :config
  (setq company-idle-delay 1.5
        company-tooltip-minimum-width 30
        company-tooltip-width-grow-only t
        company-tooltip-limit 15
        company-tooltip-align-annotations t)
  (global-company-mode))

(use-package company-box
  :delight
  :hook (company-mode . company-box-mode))

;;(use-package org :defer t)


;;; Docker setup

(use-package docker
  :defer
  :commands (docker))

(use-package dockerfile-mode :defer)

;;; YAML setup
(use-package yaml-mode :defer)

(use-package flycheck-yamllint
  :after (flycheck yaml-mode))

;;; Jenkinsfile setup
(use-package groovy-mode :defer)


;;; Common Lisp setup

(use-package slime
  :defer
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

(use-package toml-mode :defer)

(use-package rust-mode :defer)

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package racer
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode)))


;;; Elixir setup

(use-package lsp-mode
  :defer
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (elixir-mode . lsp))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-clients-elixir-server-executable '("elixir-ls")
        lsp-headerline-breadcrumb-enable nil))

(use-package elixir-mode :defer
  :hook (elixir-mode . yas-minor-mode)
  :config
  (add-hook 'elixir-mode-hook (lambda () (show-paren-mode -1))))

(use-package alchemist
  :disabled
  :after (elixir-mode)
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

(use-package cider :defer)


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
  :after company irony
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :after company irony
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
  :defer
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-modules (delq 'elpy-module-yasnippet elpy-modules))
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules)))


;;; JavaScript/TypeScript setup

(use-package typescript-mode :defer)

(use-package tide
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . eldoc-mode)
         (js-mode . (lambda () (unless (derived-mode-p 'json-mode)
                                 (tide-setup)
                                 (eldoc-mode))))
         (typescript-mode . tide-hl-identifier-mode)))

(use-package prettier-js
  :defer
  :commands (prettier-js))


;;; Scratch

(use-package explain-pause-mode
  :disabled ; causes weird delays
  :straight (:host github :repo "lastquestion/explain-pause-mode")
  :config
  (explain-pause-mode 1))

(use-package lonely-brace-mode
  :straight (:host github :repo "leodag/lonely-brace-mode"))

(defun pop-to-other-window ()
  "Go to previous buffer, taking current one to other window.
Very useful when a command decides to pop into the window you
were working on."
  (interactive)
  (let ((cur (current-buffer)))
    (switch-to-prev-buffer nil t)
    (display-buffer cur t)))

(define-key leodag-map (kbd "C-o") #'pop-to-other-window)

(use-package web-mode
  :mode ("\\.x?html?\\'"
         "\\.[lh]?eex\\'"))

(use-package lsp-tailwindcss
  :config
  (add-to-list 'lsp-language-id-configuration '(".*\\.[lh]?eex$" . "html")))

(defun project-create-dir-locals ()
  "Edit or create a .dir-locals.el file of the project."
  (interactive)
  (if-let ((pr (project-current)))
      (let ((file (expand-file-name ".dir-locals.el" (project-root pr))))
        (when (not (file-exists-p file))
          (make-empty-file file)))
    (error "Already exists")))

;;; init.el ends here
