;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Keep this at the top
(load! "functions" doom-private-dir)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")
(setq user-full-name "Ang Kok Jun Keith" user-mail-address "angkeith@hotmail.sg")
(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "Source Code Pro" :size 12))
;; viable font
;; (setq doom-font (font-spec :family "Source Code Pro" :size 14))
;; (setq doom-font (font-spec :family "MesloLGL Nerd Font" :size 12))
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
(setq display-line-numbers-type 't)
(setq which-key-idle-delay 0.2)

(setq whitespace-style '(face trailing tabs lines-tail empty big-indent))

;; better defaults for clipboard
;; yank whole line with Y
(setq select-enable-clipboard nil)
(evil-put-command-property 'evil-yank-line :motion 'evil-line)

;;; Global-modes
(global-fci-mode t)
(global-git-gutter+-mode t)
(global-pretty-mode t)
(global-whitespace-mode t)
(global-yascroll-bar-mode t)
(all-the-icons-ibuffer-mode 1)
(company-flx-mode t)
(symbol-overlay-mode t)

;;; custom
;; (custom-set-variables '(yascroll:delay-to-hide nil))
;; default one is difficult to differentiate from currently selected search
(custom-set-faces
 '(evil-ex-lazy-highlight ((t (:foreground "black" :background "yellow")))))

(load! "keybindings" doom-private-dir)
(load! "use-packages" doom-private-dir)
(load! "hooks" doom-private-dir)
