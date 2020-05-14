;;; ~/dotfiles/stow/emacs/.config/doom/+filetype.el -*- lexical-binding: t; -*-

(map! :map js2-mode-map
      :localleader
      "f" #'my-js-mode-lint)
