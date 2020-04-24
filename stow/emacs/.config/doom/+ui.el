;;; ~/dotfiles/stow/emacs/.config/doom/+ui.el -*- lexical-binding: t; -*-

;; centaur-tabs
(after! centaur-tabs
  (setq
   centaur-tabs-style "bar"
   centaur-tabs-set-bar 'left
   centaur-tabs-set-icons t
   centaur-tabs-set-close-button nil
   centaur-tabs-cycle-scope 'tabs
   centaur-tabs-icon-scale-factor 1
   centaur-tabs-icon-v-adjust -0.15
   centaur-tabs-modified-marker "")
  (map!
   (:leader :n "gt" #'centaur-tabs-counsel-switch-group)
   :n "gt"          #'centaur-tabs-forward-group
   :n "C-1"         #'centaur-tabs-select-visible-tab
   :n "C-2"         #'centaur-tabs-select-visible-tab
   :n "C-3"         #'centaur-tabs-select-visible-tab
   :n "C-4"         #'centaur-tabs-select-visible-tab
   :n "C-5"         #'centaur-tabs-select-visible-tab
   :n "<C-tab>"     #'centaur-tabs-forward)
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  (add-hook 'term-mode-hook #'centaur-tabs-local-mode))

;; git-gutter
(after! git-gutter+
  (map!
   :leader
   :nm "hh" #'git-gutter+-show-hunk-inline-at-point
   :nm "hu" #'my-git-gutter+-revert-hunks
   :nm "hs" #'git-gutter+-stage-hunks)
  (map!
   :nm "]c" #'git-gutter+-next-hunk
   :nm "[c" #'git-gutter+-previous-hunk))

;; git-gutter-fringe+
(use-package! git-gutter-fringe+
  :config
  (if (fboundp 'fringe-mode) (fringe-mode '4))
  (fringe-helper-define #'git-gutter-fr+-added '(bottom repeated)
    "XX.....")
  (fringe-helper-define #'git-gutter-fr+-modified '(bottom repeated)
    "XX.....")
  (fringe-helper-define #'git-gutter-fr+-deleted '(bottom nil)
    "XX....."
    "XX....."
    "XX....."
    "XX....."
    "XX....."
    "XX....."))

;; hl-todo
(after! hl-todo
  (setq hl-todo-keyword-faces
        '(;; For things that need to be done, just not today.
          ("TODO" success bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("FIXME" error bold)
          ("REFACTOR" highlight-numbers-number bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ("HOTFIX" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold))))

;; nav-flash
(after! nav-flash
  (advice-add #'undo-tree-undo :after #'+nav-flash-blink-cursor)
  (advice-add #'undo-tree-redo :after #'+nav-flash-blink-cursor)
  (advice-add #'goto-last-change :after #'+nav-flash-blink-cursor))

;; neotree
(map! :inmg "C-n" #'neotree-toggle)
(after! neotree
  (setq neo-smart-open t
        doom-themes-neotree-file-icons t
        neo-theme 'icons)
  (map! (:map neotree-mode-map
          :n "C-k" #'neotree-select-previous-sibling-node
          :n "C-j" #'neotree-select-next-sibling-node
          :n "h" #'neotree-select-up-node
          :n "x" #'neotree-delete-node
          :n "C" #'neotree-collapse-all
          :n "D" #'make-directory
          :n "n" #'evil-ex-search-next
          :n "p" #'evil-ex-search-previous
          :n "?" #'neotree-dispatch
          :n "C-SPC" #'neotree-quick-look)))
;; HOTFIX for some reason `(featurep 'hl-line)` evaluates to nil in the dashboard
(add-hook! 'neotree-mode-hook (hl-line-mode 1))
(setq-hook! 'neotree-mode-hook
  evil-normal-state-cursor '((bar . 0)) ;; hides the cursor
  yascroll:scroll-bar 'left-fringe)
(advice-add #'doom-themes-neotree-insert-root
            :override #'+neo-buffer--insert-root-entry)
(advice-add #'doom-themes--neotree-no-fringes
            :override #'+doom-themes--neotree-no-fringes)
;; force transient display buffer to the bottom
(add-hook! 'neo-after-create-hook
  (setq-local transient-display-buffer-action
              '(display-buffer-in-side-window bottom)))

;; yascroll
(after! yascroll
  (set-face-foreground 'yascroll:thumb-fringe "#da8548")
  (set-face-background 'yascroll:thumb-fringe "#da8548")
  (setq-default yascroll:disabled-modes '(+doom-dashboard-mode)
        yascroll:delay-to-hide nil))
