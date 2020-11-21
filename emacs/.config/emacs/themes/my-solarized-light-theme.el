(require 'solarized)
(eval-when-compile
  (require 'solarized-palettes))

(deftheme my-solarized-light "The my-solarized-light colour theme of Solarized colour theme flavor.")

(solarized-with-color-variables 'light 'my-solarized-light
  solarized-light-color-palette-alist
  '((custom-theme-set-faces
     theme-name
;;;;; company-mode
     `(company-tooltip-selection ((,class (:background ,blue-2bg :weight bold))))
;;;;; ff-st
     `(ff-st-line ((,class (:strike-through ,s-line))))
;;;;; paren
     `(show-paren-match ((,class (:foreground ,base3 :weight bold))))
;;;;; projectile-header-line
     `(projectile-header-line-file ((,class (:foreground ,cyan :weight bold))))
     `(projectile-header-line-project ((,class (:foreground ,yellow :weight bold))))
;;;;; tab-bar
     `(tab-bar ((,class (:inherit default))))
     `(tab-bar-tab ((,class (:box nil))))
     `(tab-bar-tab-inactive ((,class (:background ,base02))))
;;;;; tooltip
     ;; Don't use that weird yellow tooltip on Windows
     `(tooltip ((,class (:background "white" :foreground "black" :inherit ,s-variable-pitch))))
;;;;; volatile highlights
     ;; Default green-lc/green-hc is way too dark
     `(vhl/default-face ((,class (:background ,green-1bg :foreground ,green-1fg))))
;;;;; whitespace
     `(whitespace-tab ((,class (:foreground ,base01 :inverse-video nil)))))
    (custom-theme-set-variables
     theme-name
;;;;; highlight-parentheses
     ;; I'd rather specify this in my init only but since it is
     ;; set in solarized-faces it's here too
     `(highlight-parentheses-colors '(nil nil nil nil nil nil nil nil)))))

(provide-theme 'my-solarized-light)

(provide 'my-solarized-light-theme)
