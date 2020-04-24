;;; ~/dotfiles/stow/emacs/.config/doom/+modeline.el -*- lexical-binding: t; -*-

;; doom-modeline
(display-battery-mode 1)
(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-nil
        doom-modeline-major-mode-icon t
        doom-modeline-persp-name t
        doom-modeline-buffer-modification-icon nil
        doom-modeline-major-mode-color-icon nil)
  (doom-modeline-def-modeline 'my-mode-line
    '(bar workspace-name window-number modals matches remote-host word-count parrot selection-info vcs buffer-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug lsp minor-modes input-method indent-info buffer-encoding process buffer-position checker)))
;; Add to `doom-modeline-mode-hook` or other hooks
(defun setup-custom-doom-modeline ()
  (doom-modeline-set-modeline 'my-mode-line 'default))
(add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)
