;;; ~/dotfiles/stow/emacs/.config/doom/use-packages.el -*- lexical-binding: t; -*-

;; avy
(after! avy
  (setq avy-timeout-seconds 0.2)
  (custom-set-faces
   '(avy-lead-face ((t (:foreground "limegreen" :background "black"))))
   '(avy-lead-face-0 ((t (:foreground "limegreen" :background "black"))))
   '(avy-goto-char-timer-face ((t (:foreground "limegreen" :background "black"))))))

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
  (add-hook 'term-mode-hook #'centaur-tabs-local-mode)
  (custom-set-faces
   '(centaur-tabs-modified-marker-selected   ((((class color) (background dark)) (:foreground "indianred" ))))
   '(centaur-tabs-modified-marker-unselected ((((class color) (background dark)) (:foreground "indianred" ))))))

;; company
(after! company
  (define-key! company-active-map
    "RET"        #'company-complete-selection
    [return]     #'company-complete-selection
    "TAB"        #'company-select-next
    [tab]        #'company-select-next
    [backtab]    #'company-select-previous
    (kbd "jk")   #'company-complete-selection
    "C-n"        #'company-select-next
    "C-p"        #'company-select-previous)
  (map! :i "M-c" #'company-dabbrev))

;; company-box
(after! company-box
  (setq company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-icons-all-the-icons
        (let ((all-the-icons-scale-factor 0.8))
          `(
            (Unknown       . ,(all-the-icons-octicon "file-text"))
            (Text          . ,(all-the-icons-faicon "file-text-o"))
            (Method        . ,(all-the-icons-faicon "cube"))
            (Function      . ,(all-the-icons-faicon "cube"))
            (Constructor   . ,(all-the-icons-faicon "cube"))
            (Field         . ,(all-the-icons-faicon "tag"))
            (Variable      . ,(all-the-icons-faicon "tag"))
            (Class         . ,(all-the-icons-faicon "cog"))
            (Interface     . ,(all-the-icons-faicon "cogs"))
            (Module        . ,(all-the-icons-alltheicon "less"))
            (Property      . ,(all-the-icons-faicon "wrench"))
            (Unit          . ,(all-the-icons-faicon "tag"))
            (Value         . ,(all-the-icons-faicon "tag"))
            (Enum          . ,(all-the-icons-faicon "file-text-o"))
            (Keyword       . ,(all-the-icons-material "format_align_center"))
            (Snippet       . ,(all-the-icons-material "content_paste"))
            (Color         . ,(all-the-icons-material "palette"))
            (File          . ,(all-the-icons-faicon "file"))
            (Reference     . ,(all-the-icons-faicon "tag"))
            (Folder        . ,(all-the-icons-faicon "folder"))
            (EnumMember    . ,(all-the-icons-faicon "tag"))
            (Constant      . ,(all-the-icons-faicon "tag"))
            (Struct        . ,(all-the-icons-faicon "cog"))
            (Event         . ,(all-the-icons-faicon "bolt"))
            (Operator      . ,(all-the-icons-faicon "tag"))
            (TypeParameter . ,(all-the-icons-faicon "cog"))
            (Template      . ,(all-the-icons-octicon "file-code"))
            (ElispFunction . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (ElispVariable . ,(all-the-icons-material "check_circle"             :face 'all-the-icons-blue))
            (ElispFeature  . ,(all-the-icons-material "stars"                    :face 'all-the-icons-orange))
            (ElispFace     . ,(all-the-icons-material "format_paint"             :face 'all-the-icons-pink)))))
  (setq company-box-max-candidates 100))

;; display-line-numbers
(after! display-line-numbers
  (setq display-line-numbers-type 't))

;; doom-modeline
(after! doom-modeline
  (doom-modeline-def-modeline 'my-mode-line
    '(bar workspace-name window-number modals matches remote-host word-count parrot selection-info vcs buffer-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug lsp minor-modes input-method indent-info buffer-encoding process buffer-position checker)))
;; Add to `doom-modeline-mode-hook` or other hooks
(defun setup-custom-doom-modeline ()
  (doom-modeline-set-modeline 'my-mode-line 'default))
(add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)

;; evil-collection-term
(after! evil-collection-term
    (evil-collection-define-key 'insert 'term-raw-map
    (kbd "C-j") 'term-send-raw))

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

;; ivy
(after! ivy
  (map! :in "M-p" #'counsel-yank-pop)
  (ivy-configure  #'counsel-yank-pop
    :height 10
    :format-fn #'counsel--yank-pop-format-function)
  (custom-set-faces
   '(ivy-minibuffer-match-face-1((((class color) (background dark)) (:foreground "white smoke"))))))

;; lsp-ui
(after! lsp-ui
  ;; (setq lsp-ui-sideline-mode t)
  (setq lsp-ui-doc-position 'top
        lsp-ui-doc-max-width 500
        lsp-ui-doc-alignment 'frame
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-enable t))

;; my-fzf
(setq path-to-fzf (concat (getenv "DOOMDIR") "/local-packages/fzf"))
(use-package! fzf
  :load-path path-to-fzf
  :config
  (map! :gnm "C-S-p" #'my-fzf-find-file
        :gnm "C-p"   #'my-fzf-find-file-from-home))

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
(add-hook! 'neotree-mode-hook (hl-line-mode 1)) ;; for some reason `(featurep 'hl-line)` evaluates to nil in the dashboard
(setq-hook! 'neotree-mode-hook
  evil-normal-state-cursor '((bar . 0)) ;; hide cursor
  yascroll:scroll-bar 'left-fringe)
(advice-add #'doom-themes-neotree-insert-root :override #'+neo-buffer--insert-root-entry)
(advice-add #'doom-themes--neotree-no-fringes :override #'+doom-themes--neotree-no-fringes)
;; force transient display buffer to the bottom
(define-transient-command neotree-dispatch ()
  "Invoke a Magit command from a list of available commands."
  :man-page "git-pull"
  [:description
   ("-r" "Rebase local commits" ("-r" "--rebase"))]
  ["Navigation"
   [("j"       "Down"              neotree-select-down-node)
    ("k"       "Up"                neotree-select-up-node)]
   [("C-j"     "Next sibling"      neotree-select-next-sibling-node)
    ("C-k"     "Previous Siblings" neotree-select-previous-sibling-node)]
   [("h"       "Parent"            +neotree/collapse-or-up)]]
  ["Actions"
   [("x"       "Remove"            neotree-delete-node)
    ("y"       "Copy"              neotree-copy-node)
    ("D"       "Create Directory"  make-directory)]
   [("C"       "Collapse All"      neotree-collapse-all)
    ("r"       "Rename or Move"    neotree-rename-node)
    ("C-SPC"   "Preview"           neotree-quick-look)
    ]])
(add-hook! 'neo-after-create-hook
  (setq-local transient-display-buffer-action '(display-buffer-in-side-window bottom)))

;; org
(setq org-directory "~/Dropbox/org") ;; must be loaded before =org= is loaded
(map!
 (:map org-mode-map :prefix ","
   :n "s" #'org-sort)
 (:leader                :n  "oa"   #'org-agenda))
(after! org
  (setq org-log-done t;; input timestamp when task is completed
        org-catch-invisible-edits t)
  ;; prettify
  (setq org-hide-emphasis-markers t
        org-ellipsis " ▾ "
        org-superstar-headline-bullets-list '("⁖")
        org-fontify-emphasized-text t
        org-emphasis-alist
        '(("*" bold)
          ("/" italic)
          ("_" underline)
          ("=" (org-verbatim :inherit rectangle-preview))
          ("~" (org-code :inherit rectangle-preview))
          ("+" (:strike-through t))))
  (appendq! +pretty-code-symbols
            '(:checkbox    "☐"
              :pending     "◼"
              :checkedbox  "☑"))
  (set-pretty-symbols! 'org-mode
    :merge t
    :checkbox    "[ ]"
    :pending     "[-]"
    :checkedbox  "[X]")
  ;; babel
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'org-babel-remove-result nil 'local ))))

;; undo-tree
(after! undo-tree
  :config
  (setq undo-tree-visualizer-relative-timestamps t)
  (map!
   (:map undo-tree-map "C-/") nil
   :g "C-/" nil
   :v "C-z" 'undo-tree-undo))
;; symbol-overlay
(after! symbol-overlay (custom-set-faces '(symbol-overlay-default-face ((t (:weight bold))))))

;; yascroll
(after! yascroll
  (set-face-foreground 'yascroll:thumb-fringe "#da8548")
  (set-face-background 'yascroll:thumb-fringe "#da8548")
  (setq yascroll:disabled-modes '(+doom-dashboard-mode))
  (setq yascroll:delay-to-hide nil))
