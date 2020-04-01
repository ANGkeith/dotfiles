;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(load! "functions" doom-private-dir)

(map! :nm "\\" nil )
(setq exec-path (append exec-path (list (getenv "NODE_PATH"))))

(setq user-full-name "Ang Kok Jun Keith" user-mail-address "angkeith@hotmail.sg")

;;; ui
(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "Source Code Pro" :size 12))
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
;; modeline
(display-battery-mode 1)

;;; behaviour
;; better defaults for clipboard
(setq select-enable-clipboard nil)
;; yank whole line with Y
(evil-put-command-property 'evil-yank-line :motion 'evil-line)
;; dont auto add comments
(setq +evil-want-o/O-to-continue-comments nil)
(setq tab-always-indent nil)
(setq evil-vsplit-window-right t evil-split-window-below t)

;;; Global-modes
(global-display-fill-column-indicator-mode t)
(global-git-gutter+-mode t)
(global-pretty-mode t)
(global-whitespace-mode t)
(global-yascroll-bar-mode t)
(all-the-icons-ibuffer-mode 1)
(company-flx-mode t)
(add-hook 'after-change-major-mode-hook #'symbol-overlay-mode)

;;; custom
(custom-set-variables '(yascroll:delay-to-hide nil))
;; default one is difficult to differentiate from currently selected search
(custom-set-faces
 '(evil-ex-lazy-highlight ((t (:foreground "black" :background "yellow")))))

(map!
 (:leader
   :desc "Next window"                       "`"   #'evil-window-next
   :desc "Toggle back and forth buffer"      "TAB" #'alternate-buffer
   :desc "Close and kill window"             "q"   #'kill-buffer-and-window
   :desc "Configurations"                    "ev"  #'doom/find-file-in-private-config
   :desc "Load doom configurations"          "sv"  #'doom/reload
   :desc "Clean up projectile cache"         "pl"  #'projectile-invalidate-cache
   :desc "M-x"                               "SPC" #'counsel-M-x
   :desc "Jump to last changed"              "jc"  #'goto-last-change
   :desc "Jump to sections"                  "ji"  #'counsel-imenu
   :desc "Evil-avy"                          "jj"  #'evil-avy-goto-char-timer
   :desc "List buffers"                      "gb"  #'ibuffer)

 (:prefix ","
   :desc "eval-last-sexp"               :n   "ee"  #'eval-last-sexp)

 :n      "<tab>"                                   #'evil-jump-item
 :n      "p"                                       #'paste-and-indent-after
 :n      "P"                                       #'paste-and-indent-before
 :nm     "C-<down>"                                #'evil-window-decrease-height
 :nm     "C-<up>"                                  #'evil-window-increase-height
 :nm     "C-<left>"                                #'evil-window-decrease-width
 :nm     "C-<right>"                               #'evil-window-increase-width
 :g      "C-S-v"                                   #'clipboard-yank
 :g      "C-S-c"                                   #'clipboard-kill-ring-save
 :n      "C-a"                                     (kbd "ggVG")
 :g      (kbd "<mouse-8>")                         #'better-jumper-jump-backward
 :g      (kbd "<mouse-9>")                         #'better-jumper-jump-forward)

(load! "package-config" doom-private-dir)

;;; popup rules
(after! flycheck
  (set-popup-rule! "^\\*Flycheck errors\\*" :side 'bottom))

;;; hooks
(add-hook 'before-save-hook
          (lambda () (save-excursion
                  (delete-trailing-whitespace)
                  (doom/delete-trailing-newlines))))
