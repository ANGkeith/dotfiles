;;; ~/dotfiles/stow/emacs/.config/doom/+utils.el -*- lexical-binding: t; -*-

;; flycheck
(setq-hook! 'sh-mode-hook
  flycheck-checker (if (eq sh-shell 'zsh) 'sh-zsh 'sh-shellcheck))
(setq-hook! 'js2-mode-hook
  flycheck-checker 'javascript-eslint)

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
