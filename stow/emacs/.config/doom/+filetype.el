;;; ~/dotfiles/stow/emacs/.config/doom/+filetype.el -*- lexical-binding: t; -*-

;; js2
(map! :map js2-mode-map
      :localleader
      "f" #'my-js-mode-lint)
(setq js-indent-level 2)
