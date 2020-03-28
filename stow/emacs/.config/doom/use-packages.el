;;; ~/dotfiles/stow/emacs/.config/doom/use-packages.el -*- lexical-binding: t; -*-

;;; org
(use-package! org
  :config
  (map!
   :nm "\\" nil
   :desc "Go to org file" :nm "\\o"
   (lambda() (interactive) (find-file "~/Dropbox/org/notes.org"))))

;;; git-gutter+
(use-package! git-gutter+
  :config
  (map!
   :leader
   :nm "hh" 'git-gutter+-show-hunk-inline-at-point
   :nm "hu" 'my-git-gutter+-revert-hunks
   :nm "hs" 'git-gutter+-stage-hunks))

;;; neotree
(use-package! neotree
  :config
  (setq neo-smart-open t
        neo-theme 'icons)
  (map!
   :inmg "C-n" 'neotree-toggle
   (:map neotree-mode-map
     :n "h" 'neotree-select-up-node
     :n "p" 'neotree-quick-look)))
;; dont load the doom-neotree ui because I prefer the default appearance
(after! doom-themes
  (remove-hook 'doom-load-theme-hook #'doom-themes-neotree-config))

;;; fill-column-indicator
(use-package! fill-column-indicator
  :config
  (setq fci-rule-column 80 fci-rule-color "#3f3f3f"))

;;; avy
(use-package! avy
  :config
  (setq avy-timeout-seconds 0.2)
  (custom-set-faces
   '(avy-lead-face ((t (:foreground "limegreen" :background "black"))))
   '(avy-lead-face-0 ((t (:foreground "limegreen" :background "black"))))
   '(avy-goto-char-timer-face ((t (:foreground "limegreen" :background "black"))))))

;;; yascroll
(use-package! yascroll
  :config
  (setq yascroll:delay-to-hide nil))

;; my-fzf
(setq path-to-fzf (concat (getenv "DOOMDIR") "/local-packages/fzf"))
(use-package! fzf
  :load-path path-to-fzf
  :config
  (map!
   :gnm    "C-p"        'my-fzf-find-file
   :gnm    "C-S-p"      'my-fzf-find-file-from-home))

;; git-gutter
(use-package! git-gutter-fringe+
  :config
  (require 'git-gutter-fringe+)
  (if (fboundp 'fringe-mode) (fringe-mode '4))
  (fringe-helper-define 'git-gutter-fr+-added '(bottom repeated)
    "XX.....")
  (fringe-helper-define 'git-gutter-fr+-modified '(bottom repeated)
    "XX.....")
  (fringe-helper-define 'git-gutter-fr+-deleted '(bottom nil)
    "XX....."
    "XX....."
    "XX....."
    "XX....."
    "XX....."
    "XX....."))
