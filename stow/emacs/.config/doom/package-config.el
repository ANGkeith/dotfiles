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
   :n "C-1" #'centaur-tabs-select-visible-tab
   :n "C-2" #'centaur-tabs-select-visible-tab
   :n "C-3" #'centaur-tabs-select-visible-tab
   :n "C-4" #'centaur-tabs-select-visible-tab
   :n "C-5" #'centaur-tabs-select-visible-tab
   :n "<C-tab>" #'centaur-tabs-forward)
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

;; eyebrowse
(after! eyebrowse
  (map!
   :n "M-1" #'eyebrowse-switch-to-window-config-1
   :n "M-2" #'eyebrowse-switch-to-window-config-2
   :n "M-3" #'eyebrowse-switch-to-window-config-3
   :n "M-4" #'eyebrowse-switch-to-window-config-4
   :n "M-5" #'eyebrowse-switch-to-window-config-5
   :n "M-6" #'eyebrowse-switch-to-window-config-6
   :n "M-7" #'eyebrowse-switch-to-window-config-7
   :n "M-8" #'eyebrowse-switch-to-window-config-8
   :n "M-9" #'eyebrowse-switch-to-window-config-9))

;; git-gutter
(after! git-gutter+
  (map!
   :leader
   :nm "hh" #'git-gutter+-show-hunk-inline-at-point
   :nm "hu" #'my-git-gutter+-revert-hunks
   :nm "hs" #'git-gutter+-stage-hunks)
  (map!
   :nm "]h" #'git-gutter+-next-hunk
   :nm "[h" #'git-gutter+-previous-hunk))

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
          :n "C" #'neotree-collapse-all
          :n "D" #'make-directory
          :n "p" #'evil-ex-search-previous
          :n "P" #'neotree-quick-look)))
(add-hook! 'neotree-mode-hook (hl-line-mode 1)) ;; for some reason `(featurep 'hl-line)` evaluates to nil in the dashboard
(setq-hook! 'neotree-mode-hook
  evil-normal-state-cursor '((bar . 0)) ;; hide cursor
  yascroll:scroll-bar 'left-fringe)
(advice-add #'doom-themes-neotree-insert-root :override #'+neo-buffer--insert-root-entry)
(advice-add #'doom-themes--neotree-no-fringes :override #'+doom-themes--neotree-no-fringes)

;; org
(setq org-directory "~/Dropbox/org") ;; must be loaded before =org= is loaded
(map!
 (:map org-mode-map :prefix ","
   :n "s" #'org-sort)
 :desc "Go to org todo" :nm "\\ot" (lambda() (interactive) (find-file "~/Dropbox/org/todo.org"))
 :desc "Go to org notes"  :nm "\\ow" (lambda() (interactive) (find-file "~/Dropbox/org/notes.org"))
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
          ("=" (org-verbatim verbatim :inherit rectangle-preview))
          ("~" (org-code verbatim :inherit rectangle-preview))
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

;; symbol-overlay
(after! symbol-overlay (custom-set-faces '(symbol-overlay-default-face ((t (:weight bold))))))

;; yascroll
(after! yascroll
  (set-face-foreground 'yascroll:thumb-fringe "#da8548")
  (set-face-background 'yascroll:thumb-fringe "#da8548")
  (setq yascroll:delay-to-hide nil))
