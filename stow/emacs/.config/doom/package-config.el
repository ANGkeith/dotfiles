;;; ~/dotfiles/stow/emacs/.config/doom/use-packages.el -*- lexical-binding: t; -*-

;; atomic-chrome
(use-package! atomic-chrome
  :after-call after-focus-change-function
  :config
  (setq atomic-chrome-default-major-mode 'markdown-mode
        atomic-chrome-buffer-open-style 'frame)
  (atomic-chrome-start-server))

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
   :n "C-1" #'centaur-tabs-select-visible-tab
   :n "C-2" #'centaur-tabs-select-visible-tab
   :n "C-3" #'centaur-tabs-select-visible-tab
   :n "C-4" #'centaur-tabs-select-visible-tab
   :n "C-5" #'centaur-tabs-select-visible-tab
   :n "<C-S-iso-lefttab>" #'centaur-tabs-counsel-switch-group
   :n "<C-tab>" #'centaur-tabs-forward)
  (centaur-tabs-headline-match)
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
  (setq company-box-max-candidates 100
        company-box-doc-enable nil))

;; display-line-numbers
(after! display-line-numbers
  (setq display-line-numbers-type 't))

;; dont load the doom-neotree ui because I prefer the default appearance
(after! doom-themes
  (remove-hook 'doom-load-theme-hook #'doom-themes-neotree-config))

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
(after! neotree
  (setq neo-smart-open t
        neo-theme 'icons))
(map!
 :inmg "C-n" #'neotree-toggle
 (:map neotree-mode-map
   :n "h" #'neotree-select-up-node
   :n "C" #'neotree-collapse-all
   :n "D" #'make-directory
   :n "p" #'neotree-quick-look))

;; org
(map!
 :desc "Go to org file" :nm "\\o"
 (lambda() (interactive) (find-file "~/Dropbox/org/notes.org")))
(after! org
  (setq org-hide-emphasis-markers t
        org-ellipsis " ▾ "
        org-bullets-bullet-list '("⁖")
        org-fontify-emphasized-text t)
  (appendq! +pretty-code-symbols
            '(:checkbox    "☐"
              :pending     "◼"
              :checkedbox  "☑"))
  (set-pretty-symbols! 'org-mode
    :merge t
    :checkbox    "[ ]"
    :pending     "[-]"
    :checkedbox  "[X]"))

;; symbol-overlay
(after! symbol-overlay
  (custom-set-faces '(symbol-overlay-default-face ((t (:weight bold))))))

;; yascroll
(after! yascroll
  (setq yascroll:delay-to-hide nil))
