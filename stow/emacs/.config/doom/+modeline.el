;;; ~/dotfiles/stow/emacs/.config/doom/+modeline.el -*- lexical-binding: t; -*-

;; doom-modeline
(display-battery-mode 1)
(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-nil
        doom-modeline-major-mode-icon t
        doom-modeline-persp-name nil
        doom-modeline-buffer-modification-icon nil
        doom-modeline-major-mode-color-icon nil)
  (doom-modeline-def-segment time
    "Time"
    (when (doom-modeline--active)
      (propertize
       (format-time-string " %b %d, %Y - %H:%M  ")
       'face (when (doom-modeline--active) `(:foreground "#1b335f" :background "#eeba76")))))
  ;; (((background light)) :foreground "#ffffff" :background "#ffaf00"))))))

  (doom-modeline-def-modeline 'my-mode-line
    '(bar workspace-name window-number modals matches remote-host word-count parrot selection-info buffer-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug lsp minor-modes input-method indent-info buffer-encoding major-mode process buffer-position vcs checker time)))

;; Add to `doom-modeline-mode-hook` or other hooks
(defun setup-custom-doom-modeline ()
  (doom-modeline-set-modeline 'my-mode-line 'default))
(add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)
