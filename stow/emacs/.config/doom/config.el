;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Keep this at the top
(load! "functions" doom-private-dir)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;;; Global-modes
(global-fci-mode t)
(global-git-gutter+-mode t)
(global-pretty-mode t)
(global-whitespace-mode t)
(global-yascroll-bar-mode t)
(all-the-icons-ibuffer-mode 1)

;;; custom
(custom-set-faces
 '(avy-lead-face ((t (:foreground "limegreen" :background "black"))))
 '(avy-lead-face-0 ((t (:foreground "limegreen" :background "black"))))
 '(avy-goto-char-timer-face ((t (:foreground "limegreen" :background "black")))))
(custom-set-variables '(yascroll:delay-to-hide nil))
;; default one is difficult to differentiate from currently selected search
(custom-set-faces
 '(evil-ex-lazy-highlight ((t (:foreground "black" :background "yellow")))))

;;; setq
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq user-full-name "Ang Kok Jun Keith" user-mail-address "angkeith@hotmail.sg")
(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "FuraCode Nerd Font" :size 12))
;; viable font
;; (setq doom-font (font-spec :family "Source Code Pro" :size 14))
;; (setq doom-font (font-spec :family "MesloLGL Nerd Font" :size 12))

(setq display-line-numbers-type 't)
(setq fci-rule-column 80 fci-rule-color "#3f3f3f")
(setq avy-timeout-seconds 0.2)
;; more natural window splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)
(setq which-key-idle-delay 0.2)
(setq whitespace-style '(face trailing tabs lines-tail empty big-indent))
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

;; better defaults for clipboard
;; yank whole line with Y
(evil-put-command-property 'evil-yank-line :motion 'evil-line)
(setq select-enable-clipboard nil)

(setq neo-smart-open t
      neo-theme 'icons)
;; dont load the doom-neotree ui because I prefer the default appearance
(after! doom-themes
  (remove-hook 'doom-load-theme-hook #'doom-themes-neotree-config))

(load! "keybindings" doom-private-dir)
(load! "hooks" doom-private-dir)
