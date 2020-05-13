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
   my-centaur-tabs-common-group-name "General"
   my-centaur-tabs-special-group-name "Emacs"
   my-centaur-tabs-org-group-name "Org"
   centaur-tabs-modified-marker "")

  (defun my-custom-centaur-group-check ()
    (string-equal  (directory-file-name (projectile-project-root))
                   (expand-file-name org-directory)))

  (defun my-centaur-tabs-switch-to-org ()
    (interactive)
    (unless (position my-centaur-tabs-org-group-name (centaur-tabs-get-groups))
      (find-file "~/Dropbox/org/todo.org"))
    (centaur-tabs-switch-group my-centaur-tabs-org-group-name))

  (defun centaur-tabs-buffer-groups ()
    "My custom centaur tabs groups which consist of only 3 groups:
1. Main  - for my normal work
2. Org   - for my org workspace
3. Emacs - for emacs special buffers"
    (list
     (cond
      ((string-equal "*" (substring (buffer-name) 0 1)) my-centaur-tabs-special-group-name)
      ((condition-case _err
          (my-custom-centaur-group-check)
        (error nil)) my-centaur-tabs-org-group-name)
      (t my-centaur-tabs-common-group-name))))
  (centaur-tabs-group-buffer-groups)

  (map!
   (:leader :n "gt"       #'centaur-tabs-counsel-switch-group)
   :n "gt"                #'centaur-tabs-forward-group
   :n "C-1"               #'centaur-tabs-select-visible-tab
   :n "C-2"               #'centaur-tabs-select-visible-tab
   :n "C-3"               #'centaur-tabs-select-visible-tab
   :n "C-4"               #'centaur-tabs-select-visible-tab
   :n "C-5"               #'centaur-tabs-select-visible-tab
   :g "M-1"               (lambda! (centaur-tabs-switch-group my-centaur-tabs-common-group-name))
   :g "M-2"               #'my-centaur-tabs-switch-to-org
   :g "M-3"               (lambda! (centaur-tabs-switch-group my-centaur-tabs-special-group-name))
   :n "<C-S-iso-lefttab>" #'centaur-tabs-backward
   :n "<C-tab>"           #'centaur-tabs-forward)
  (centaur-tabs-headline-match)
  (add-hook 'term-mode-hook #'centaur-tabs-local-mode))                         ; Don't show centaur tabs in term mode

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

;; treemacs
(after! treemacs
  (setq
   doom-themes-treemacs-enable-variable-pitch nil
   doom-themes-treemacs-theme  "doom-colors"                                 ; use all-the-icons icons for file type icons
   doom-themes-treemacs-line-spacing 2))

;; pretty-symbol
(plist-put +pretty-code-symbols :return nil)
(plist-put +pretty-code-symbols :yield nil)
(plist-put +pretty-code-symbols :pipe nil)
(plist-put +pretty-code-symbols :and nil)
(plist-put +pretty-code-symbols :for nil)
(plist-put +pretty-code-symbols :or nil)
(plist-put +pretty-code-symbols :lambda nil)

;; yascroll
(after! yascroll
  (set-face-foreground 'yascroll:thumb-fringe "#da8548")
  (set-face-background 'yascroll:thumb-fringe "#da8548")
  (setq-default yascroll:disabled-modes '(+doom-dashboard-mode)
        yascroll:delay-to-hide nil))
