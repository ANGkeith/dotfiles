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

;; html
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq tab-width 2)                                                            ; html-tidy --indent-spaces inherits this value
  (setq web-mode-indent-style 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; sh-mode
(setq-hook! 'sh-mode-hook
  flycheck-checker (if (eq sh-shell 'zsh) 'sh-zsh 'sh-shellcheck))
