;;; ~/dotfiles/stow/emacs/.config/doom/hooks.el -*- lexical-binding: t; -*-

(add-hook 'before-save-hook
          ;; TODO fix the annoying jumping around after saving
          (lambda ()
            (delete-trailing-whitespace)
            (doom/delete-trailing-newlines)))
