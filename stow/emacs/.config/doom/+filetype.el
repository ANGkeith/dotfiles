;;; ~/dotfiles/stow/emacs/.config/doom/+filetype.el -*- lexical-binding: t; -*-

;; js2
(map! :map js2-mode-map
      :localleader
      "f" #'my-js-mode-lint)
(setq js-indent-level 2)

;; typescript
(map! :map typescript-mode-map
      :localleader
      "f" #'my-js-mode-lint)
(setq-hook! 'typescript-mode-hook
  typescript-indent-level 2)

;; sh-mode
(setq-hook! 'sh-mode-hook
  flycheck-checker (if (eq sh-shell 'zsh) 'sh-zsh 'sh-shellcheck))
