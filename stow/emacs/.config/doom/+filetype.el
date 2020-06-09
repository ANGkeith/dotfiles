;;; ~/dotfiles/stow/emacs/.config/doom/+filetype.el -*- lexical-binding: t; -*-

;; js2
(map!
 (:map js2-mode-map
  :n "C-c i" #'js-import
  :localleader "f" #'my-js-mode-lint))
(setq js-indent-level 2)

;; typescript
(map!
 (:map typescript-mode-map
  :n "C-c i" #'js-import
  :localleader
  "f" #'my-js-mode-lint))
(setq-hook! 'typescript-mode-hook
  typescript-indent-level 2)

;; html
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (sp-local-pair 'web-mode "<" nil :actions :rem)
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        tab-width 2                                                             ; html-tidy --indent-spaces inherits this value
        web-mode-auto-close-style nil                                           ; use emmet so don't need auto close
        web-mode-enable-current-column-highlight t
        web-mode-indent-style 2))
(add-hook 'web-mode-hook  #'my-web-mode-hook)

(defun my-rjsx-mode-hook ()
  (setq
   css-indent-offset 2)
  (sp-local-pair 'rjsx-mode "<" nil :actions :rem))
(add-hook 'rjsx-mode-hook  #'my-rjsx-mode-hook)

;; sh-mode
(setq-hook! 'sh-mode-hook
  flycheck-checker (if (eq sh-shell 'zsh) 'sh-zsh 'sh-shellcheck))
