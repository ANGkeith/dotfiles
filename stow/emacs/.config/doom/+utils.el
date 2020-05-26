;;; ~/dotfiles/stow/emacs/.config/doom/+utils.el -*- lexical-binding: t; -*-

;; lsp
(after! lsp-ui
  (setq lsp-ui-doc-position 'top
        lsp-ui-doc-max-width 150
        lsp-ui-doc-max-height 52
        lsp-ui-doc-border "#dcaeea"
        lsp-ui-doc-alignment 'frame
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-enable t))
(add-hook! 'lsp-mode-hook
  (flycheck-add-next-checker 'lsp 'javascript-eslint))

;; my-fzf
(setq path-to-fzf (concat (getenv "DOOMDIR") "/local-packages/fzf"))
(use-package! fzf
  :load-path path-to-fzf
  :config
  (map! :gnm "C-S-p" #'my-fzf-find-file
        :gnm "C-p"   #'my-fzf-find-file-from-home))

;; undo-tree
(after! undo-tree
  (setq undo-tree-visualizer-relative-timestamps t)
  (map!
   (:map undo-tree-map "C-/") nil
   :g "C-/" nil
   :v "C-z" 'undo-tree-undo))

;; format-all--executable-table
(setq path-to-eslint-fix (concat (getenv "DOOMDIR") "/local-packages/eslint-fix"))
(use-package! eslint-fix
  :load-path path-to-eslint-fix)
