;;; ~/dotfiles/stow/emacs/.config/doom/use-packages.el -*- lexical-binding: t; -*-

;; Company-box
(use-package! company-box
  :init
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
            ( EnumMember   . ,(all-the-icons-faicon "tag"))
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
  :config
  (setq
   company-box-max-candidates 100
   company-box-doc-enable nil)
  (define-key! company-active-map
    "M-h"       #'company-box-doc-manually))

;; avy
(use-package! avy
  :config
  (setq avy-timeout-seconds 0.2)
  (custom-set-faces
   '(avy-lead-face ((t (:foreground "limegreen" :background "black"))))
   '(avy-lead-face-0 ((t (:foreground "limegreen" :background "black"))))
   '(avy-goto-char-timer-face ((t (:foreground "limegreen" :background "black"))))))


;; centaur-tabs
(use-package! centaur-tabs
  :config
  (setq
   centaur-tabs-set-bar 'under
   centaur-tabs-set-icons t
   centaur-tabs-set-close-button nil
   x-underline-at-descent-line t
   centaur-tabs-style "chamfer"
   centaur-tabs-icon-scale-factor 0.8
   centaur-tabs-modified-marker "")
  (centaur-tabs-headline-match)
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

;; display-line-numbers
(use-package! display-line-numbers
  :config
  (setq display-line-numbers-type 't))

;; evil-numbers
(use-package! evil-numbers)

;; git-gutter
(use-package! git-gutter+
  :config
  (map!
   :leader
   :nm "hh" 'git-gutter+-show-hunk-inline-at-point
   :nm "hu" 'my-git-gutter+-revert-hunks
   :nm "hs" 'git-gutter+-stage-hunks)
  (map!
   :nm "]h" 'git-gutter+-next-hunk
   :nm "[h" 'git-gutter+-previous-hunk))

;; git-gutter-fringe+
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

;; ivy
(use-package! ivy
  :config
  (map! :in "C-p" 'counsel-yank-pop)
  (ivy-configure  'counsel-yank-pop
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
  (map! :gnm "C-S-t" 'my-fzf-find-file
        :gnm "C-t"   'my-fzf-find-file-from-home))

;; neotree
(use-package! neotree
  :config
  (setq neo-smart-open t
        neo-theme 'icons)
  (map!
   :inmg "C-n" 'neotree-toggle
   (:map neotree-mode-map
     :n "h"    'neotree-select-up-node
     :n "p"    'neotree-quick-look)))
;; dont load the doom-neotree ui because I prefer the default appearance
(after! doom-themes
  (remove-hook 'doom-load-theme-hook #'doom-themes-neotree-config))

;; org
(use-package! org
  :config
  (setq org-hide-emphasis-markers t
        org-fontify-emphasized-text t)
  (map!
   :desc "Go to org file" :nm "\\o"
   (lambda() (interactive) (find-file "~/Dropbox/org/notes.org"))))

;; symbol-overlay
(use-package! symbol-overlay
  :config
  (custom-set-faces '(symbol-overlay-default-face ((t (:weight bold))))))

;; whitespace
(use-package! whitespace
  :config
  (setq whitespace-style '(face trailing tabs empty)))
  ;; (setq whitespace-style '(face tabs tab-mark)))

;; yascroll
(use-package! yascroll
  :config
  (setq yascroll:delay-to-hide nil))
