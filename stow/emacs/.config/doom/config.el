;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Ang Kok Jun Keith" user-mail-address "angkeith@hotmail.sg")
(load! "functions" doom-private-dir)

(map! :nm  "\\" nil
      :g "M-<ESC>" nil
      :g   "M-`" nil)

;;; paths
(setq exec-path (append exec-path (list (getenv "NODE_PATH"))))
(setq auth-sources
      (append (list (concat (getenv "DOOMDIR") "/authinfo.gpg")) auth-sources))

;;; ui
(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "SauceCodePro Nerd Font" :size 14)
      doom-variable-pitch-font (font-spec :family "SauceCodePro Nerd Font" :size 14))
(setq doom-modeline-buffer-file-name-style 'truncate-nil)
(setq doom-modeline-major-mode-icon t
      doom-modeline-persp-name t
      doom-modeline-buffer-modification-icon nil
      doom-modeline-major-mode-color-icon nil)

;; modeline
(display-battery-mode 1)

;;; behaviour
(setq show-trailing-whitespace t)
;; better defaults for clipboard
(setq select-enable-clipboard nil)
(setq evil-want-fine-undo t)
;; yank whole line with Y
(evil-put-command-property 'evil-yank-line :motion 'evil-line)
;; dont auto add comments
(setq +evil-want-o/O-to-continue-comments nil)
;; (setq tab-always-indent nil)
(setq evil-vsplit-window-right t evil-split-window-below t)
(setq trash-directory "/tmp/trash")
(setq-default delete-by-moving-to-trash t)
;; maximize emacs on startup
(if (eq initial-window-system 'x)
    (toggle-frame-maximized))

;;; Global-modes
(global-display-fill-column-indicator-mode t)
(global-git-gutter+-mode t)
(global-yascroll-bar-mode t)
(company-flx-mode t)
(add-hook 'after-change-major-mode-hook #'my-symbol-overlay-mode)

;;; custom
(setq eros-eval-result-prefix "⇒ ")
(custom-set-faces
 '(evil-ex-lazy-highlight ((t (:foreground "black" :background "goldenrod3")))))

(map!
 (:leader
   :desc "Next window"                       "`"   #'evil-window-next
   :desc "Toggle back and forth buffer"      "TAB" #'evil-switch-to-windows-last-buffer
   :desc "Close and kill window"             "q"   #'kill-current-buffer
   :desc "Configurations"                    "ev"  #'doom/find-file-in-private-config
   :desc "Load doom configurations"          "sv"  #'doom/reload
   :desc "Clean up projectile cache"         "pl"  #'projectile-invalidate-cache
   :desc "M-x"                               "SPC" #'ivy-resume
   :desc "Jump to last changed"              "jc"  #'goto-last-change
   :desc "Correct word"                      "cw"  #'flyspell-correct-at-point
   :desc "Jump to sections"                  "ji"  #'counsel-imenu
   :desc "Evil-avy"                          "jj"  #'evil-avy-goto-char-timer
   :desc "List buffers"                      "gb"  #'persp-switch-to-buffer)

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

;; ;;; patch for doom-one-light themes. ONLY UNCOMMENT IF USING DOOM-ONE-LIGHT themes
;;   (custom-set-faces
;;   '(fill-column-indicator ((t (:foreground "#4078f2"))))
;;   '(font-lock-keyword-face ((t (:foreground "#4078f2"))))
;;   '(font-lock-string-face ((t (:foreground "#0d850b"))))
;;   '(font-lock-comment-face ((t (:weight semi-bold :slant italic))))
;;   '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
;;   '(success ((t (:foreground "#2aa34d"))))))

;;; flycheck
(setq-hook! 'sh-mode-hook
  flycheck-checker (if (eq sh-shell 'zsh) 'sh-zsh 'sh-shellcheck))

;; workspace for org
(setq org-workspace-name "org")
(defun setup-org-workspace ()
  (unless (+workspace-exists-p org-workspace-name)
    (message "Created org workspace")
    (+workspace/new org-workspace-name)
    (find-file "~/Dropbox/org/todo.org")))
(advice-add '+workspace/switch-to-1 :before #'setup-org-workspace)
