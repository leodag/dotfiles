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
;; Disable "You can run the command {} with M-x {}" message
(setq suggest-key-bindings nil)

;; Small indicator on lines that don't exist at the end of the file
(setq-default indicate-empty-lines t)

(setq-default truncate-lines nil)

(setq ring-bell-function 'ignore)

;; Amount of lines to keep above/below point
(setq scroll-margin 5)
;; A value over 100 implies never recentering
(setq scroll-conservatively 101)

;; TODO: use-package mwheel
(setq mouse-wheel-progressive-speed nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(if (member "Fira Mono" (font-family-list))
    (setq monospace-font "Fira Mono")
  (setq monospace-font (face-attribute 'default :family))
  (message "Fira Mono not installed!"))

;; TODO: better base face?
(if (member "Fira Sans" (font-family-list))
    (setq sans-serif-font "Fira Sans")
  (setq sans-serif-font (face-attribute 'variable-pitch :family))
  (message "Fira Sans not installed!"))

(defun font-at-size (family pt)
  "Generates a font spec for the desired font at specified size (in points)"
  (font-spec :family family :size (float pt)))

(defun mono-font (pt)
  "Generates a font spec for the monospace font at specified size (in points)"
  (font-at-size monospace-font pt))

(defun sans-font (pt)
  "Generates a font spec for the sans serif font at specified size (in points)"
  (font-at-size sans-serif-font pt))

(set-face-attribute 'default nil :font (mono-font 10))

(setq package-manager 'straight)

(pcase package-manager
  ('straight
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
   (setq straight-use-package-by-default t))

  ('package
   (require 'package)
   (setq package-enable-at-startup nil)
   (add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/"))
   (package-initialize t)

   (unless (package-installed-p 'use-package)
     (package-refresh-contents)
     (package-install 'use-package))

   (require 'use-package)
   (setq use-package-always-ensure t)))

(setq use-package-compute-statistics t)

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
  (find-file (find-lisp-object-file-name 'edit-init nil)))

(defun reload-init ()
  "Reload init.el"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; Themes
(if window-system
    (use-package solarized-theme
      :config
      (load-theme 'solarized-light t)))

;; Allows minor mode name manipulation
(use-package delight :defer t)

;;; Utilities
(use-package tramp :defer t :straight nil)

(use-package ag :defer t)
(use-package ripgrep :defer t)

(use-package all-the-icons :defer t)

;;; General setup
(use-package whitespace
  :delight whitespace-mode
  :hook ((prog-mode text-mode) . whitespace-mode)
  :custom-face
  (whitespace-tab ((t (:background "dim gray" :foreground "tan"))))
  :config
  (setq-default indent-tabs-mode nil) ; use spaces instead of tabs
  (setq whitespace-style
        '(face trailing tabs spaces newline empty space-after-tab
               space-before-tab tab-mark)))

;; Changes tooltip color on Windows
(use-package tooltip :straight nil
  :config
  (custom-set-faces
   `(tooltip
     ((t (:background "white" :foreground "black" :font ,(sans-font 10)))))))

;; Maybe a better way?
(if (not (version<= emacs-version "26.0"))
    (use-package display-line-numbers
      :hook ((prog-mode text-mode) . display-line-numbers-mode)
      :config
      (setq display-line-numbers-type 'relative)
      (setq display-line-numbers-grow-only t))
  (use-package nlinum-relative
    :hook ((prog-mode text-mode) . nlinum-relative-mode)
    :config
    (setq nlinum-relative-redisplay-delay 0)
    (nlinum-relative-setup-evil)))

;; Highlight current line
(use-package hl-line
  :config
  ;; Don't highlight a window's line on other windows on that buffer
  (setq global-hl-line-sticky-flag nil)
  (global-hl-line-mode 1))

;; Command interaction mode
(use-package comint :straight nil
  :bind (:map comint-mode-map
              ([up] . comint-previous-input)
              ([down] . comint-next-input)))

;; Auto-reload modified files; warn on changes file
(use-package autorevert
  :delight auto-revert-mode
  :config
  (global-auto-revert-mode))

;; Show undo history in a tree
(use-package undo-tree
  :delight
  :config
  (global-undo-tree-mode))

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
  ;; Display (current/total) instead of just total
  (setq ivy-count-format "(%d/%d) ")
  ;; Highlight the entire line
  (setq ivy-format-function 'ivy-format-function-line)
  (setq ivy-use-selectable-prompt t))

(use-package ivy-hydra :after ivy)

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
  (setq helm-full-frame t)
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

(use-package counsel-projectile
  :after projectile)

(use-package treemacs :defer 2
  :after evil
  :commands (treemacs-is-treemacs-window-selected? treemacs-select-window)
  :bind (([f8] . treemacs-select-or-deselect)
         :map treemacs-mode-map
         ([mouse-1] . treemacs-single-click-expand-action))
  :config
  (evil-set-initial-state 'treemacs-mode 'emacs)

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

  (setq treemacs-indentation 1)

  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-fringe-indicator-mode 1)
  (treemacs-git-mode
   (pcase system-type
     ('gnu/linux 'deferred)
     ('windows-nt 'simple))))

(defun treemacs-open-select-or-close ()
  "Opens treemacs if it is not already open, select if it is visible but not selected, and closes it if it is selected."
  (interactive)
  (if (treemacs-is-treemacs-window-selected?)
      (delete-window (selected-window))
    (treemacs-select-window)))

(defun treemacs-select-or-deselect ()
  "Opens treemacs if it is not already open, select if it is visible but not selected, and select last selected window if it is selected."
  (interactive)
  (if (treemacs-is-treemacs-window-selected?)
      (select-window (get-mru-window))
    (treemacs-select-window)))

;; Makes buffer names be unique
(use-package uniquify :straight nil
  :config
  (setq uniquify-buffer-name-style 'forward))

;; Highlights parentheses enclosing point
(use-package highlight-parentheses
  :delight
  :hook (prog-mode . highlight-parentheses-mode)
  :custom-face
  (hl-paren-face ((t (:weight bold))))
  :config
  ;; only highlights as many levels of parens as the length of this list
  (setq hl-paren-colors '(nil nil nil nil nil nil nil nil)))

;; Highlight matching paren
(use-package paren
  :hook (prog-mode . show-paren-mode)
  :custom-face
  (show-paren-match ((t (:foreground "black")))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
  ;;; The following lines are not relevant anymore, but the bug is still
  ;;; present so they are kept in my init for documentation purposes.
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
  :bind* (("M-<up>"    . windmove-up   )
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

(use-package flycheck :defer nil
  :after (which-key evil)
  :bind (:map flycheck-error-list-mode-map
              ("k" . flycheck-error-list-previous-error)
              ("j" . flycheck-error-list-next-error))
  :bind-keymap ("C-c !" . flycheck-command-map)
  :config
  (global-flycheck-mode)
  (which-key-add-key-based-replacements "C-c !" "flycheck")
  (evil-set-initial-state 'flycheck-error-list-mode 'emacs)
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

(use-package company
  :delight
  :hook (prog-mode . company-mode)
  :bind* ("C-<tab>" . company-complete-common-or-cycle)
  :config
  (setq company-idle-delay 1.5))

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
(use-package alchemist
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

(use-package company-irony :after irony
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers :after irony
  :config
  (add-to-list 'company-backends 'company-irony-c-headers))

;; Disabled because it causes eager loading of irony
(use-package flycheck-irony
  :disabled
  :hook (flycheck-mode . flycheck-irony-setup))

;;; Python setup
(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

(use-package company-anaconda
  :after anaconda-mode
  :config
  (add-to-list 'company-backends 'company-anaconda))

;;;; SCRATCH
;; wasn't working before - idk why works now
(use-package treemacs-projectile
  :bind (([M-f8] . treemacs-projectile)))

(use-package treemacs-evil :after treemacs)
(use-package treemacs-magit :after (treemacs magit))
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-mode))

;;; C# setup
(use-package omnisharp
  :after (flycheck company)
  :hook ((csharp-mode . omnisharp-mode)
         (csharp-mode . flycheck-mode))
  :config
  (add-to-list 'company-backends 'company-omnisharp)
  ;(setq omnisharp-debug t)
  (setq omnisharp-expected-server-version "1.32.18"))
  ;;(setq omnisharp-server-executable-path
  ;;      (concat user-emacs-directory "omnisharp/run"))

(use-package persp-mode)

(use-package js2-mode
  :interpreter "node"
  :mode ("\\.js\\'"
         ("\\.jsx\\'" . js2-jsx-mode)))

(use-package js2-refactor
  :hook (js2-mode . js2-refactor-mode))

(use-package json-mode :defer t)

(use-package tern
  :hook (js2-mode . tern-mode)
  :config
  ;; tern tries to infer bin/tern path from  location - doesn't play
  ;; nicely with straight, since it relocates the elisp files.
  (setq tern-command
        `("node"
          ,(expand-file-name ; needed on Windows to expand ~
            (concat user-emacs-directory "straight/repos/tern/bin/tern")))))

(use-package company-tern
  :after tern
  :config
  (add-to-list 'company-backends 'company-tern))

;; js-doc
;; json-reformat
;; json-snatcher
;; flymake-json
;; eslint

(use-package yaml-mode :defer t)

(use-package flycheck-yamllint
  :hook (flycheck-mode . flycheck-yamllint-setup))
