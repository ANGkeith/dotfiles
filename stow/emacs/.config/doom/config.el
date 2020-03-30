;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Keep this at the top
(load! "functions" doom-private-dir)

(map! :nm "\\" nil )

(setq user-full-name "Ang Kok Jun Keith" user-mail-address "angkeith@hotmail.sg")
(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "Source Code Pro" :size 12))
;; viable font
;; (setq doom-font (font-spec :family "Source Code Pro" :size 14))
;; (setq doom-font (font-spec :family "MesloLGL Nerd Font" :size 12))
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
;; better defaults for clipboard
;; yank whole line with Y
(setq select-enable-clipboard nil)
(evil-put-command-property 'evil-yank-line :motion 'evil-line)
(setq exec-path (append exec-path (list (getenv "NODE_PATH"))))

;;; Global-modes
(global-fci-mode t)
(global-git-gutter+-mode t)
(global-pretty-mode t)
(global-whitespace-mode t)
(global-yascroll-bar-mode t)
(all-the-icons-ibuffer-mode 1)
(company-flx-mode t)
(add-hook 'after-change-major-mode-hook #'symbol-overlay-mode)

;;; custom
;; (custom-set-variables '(yascroll:delay-to-hide nil))
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
 :n      "p"          'paste-and-indent-after
 :n      "P"          'paste-and-indent-before
 :nm     "C-<down>"   'evil-window-decrease-height
 :nm     "C-<up>"     'evil-window-increase-height
 :nm     "C-<left>"   'evil-window-decrease-width
 :nm     "C-<right>"  'evil-window-increase-width
 :g      "C-S-v"      'clipboard-yank
 :g      "C-S-c"      'clipboard-kill-ring-save
 ;; inspired by web M-d for changing url
 :nm     "M-d"        'counsel-find-file
 :n      "C-a"        (kbd "ggVG")
 :g      (kbd "<mouse-8>")  'previous-buffer
 :g      (kbd "<mouse-9>")  'next-buffer)

(load! "package-config" doom-private-dir)

;;; popup rules
(after! flycheck
  (set-popup-rule! "^\\*Flycheck errors\\*" :side 'bottom))

;;; hooks
(add-hook 'before-save-hook
          ;; TODO fix the annoying jumping around after saving
          (lambda ()
            (delete-trailing-whitespace)
            (doom/delete-trailing-newlines)))
