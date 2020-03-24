;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ang Kok Jun Keith"
      user-mail-address "angkeith@hotmail.sg")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "Source Code Pro" :size 14))
(setq doom-font (font-spec :family "FuraCode Nerd Font" :size 14))
;; (setq doom-font (font-spec :family "MesloLGL Nerd Font" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
;; can consider:
;; oceanic-next (dark)
;; doom-one-light (light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(setq fci-rule-column 80 fci-rule-color "#3f3f3f")
(define-globalized-minor-mode global-fci-mode fci-mode turn-on-fci-mode)
(global-fci-mode t)
(global-pretty-mode t)

(global-yascroll-bar-mode t)
(custom-set-variables '(yascroll:delay-to-hide nil))

(setq avy-timeout-seconds 0.2)
(custom-set-faces
 '(avy-lead-face ((t (:foreground "limegreen" :background "black"))))
 '(avy-lead-face-0 ((t (:foreground "limegreen" :background "black"))))
 '(avy-goto-char-timer-face ((t (:foreground "limegreen" :background "black")))))

;; bloat but cool
(custom-set-variables
 '(minimap-width-fraction 0.05)
 '(minimap-minimum-width 15)
 '(minimap-update-delay 0.3)
 '(minimap-dedicated-window nil)
 '(minimap-recenter-type 'relative)
 '(minimap-window-location 'right))

;; more natural window splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq which-key-idle-delay 0.2)

(setq fzf/args "--height 100")

(setq doom-modeline-indent-info nil
      doom-modeline-buffer-file-name-style 'relative-from-project)

;; yank whole line with Y
(evil-put-command-property 'evil-yank-line :motion 'evil-line)
(setq select-enable-clipboard nil)

(setq neo-smart-open t
      neo-theme 'icons)
;; dont load the doom-neotree ui because I prefer the default appearance
(after! doom-themes
  (remove-hook 'doom-load-theme-hook #'doom-themes-neotree-config))


(load! "functions" doom-private-dir)
(load! "keybindings" doom-private-dir)
;; optimization
;; Garbage-collect on focus-out, Emacs should feel snappier.
(add-hook 'focus-out-hook #'garbage-collect)
