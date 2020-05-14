;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Ang Kok Jun Keith" user-mail-address "angkeith@hotmail.sg")
(load! "functions" doom-private-dir)
;;; overrides
(map! :nm  "\\"      nil
      :g   "M-<ESC>" nil
      :g   "C-k"     nil
      :n   "C-u"     'universal-argument
      :img "C-z"     nil
      :g   "M-`"     nil)

;;; paths
(setq exec-path (append exec-path (list (getenv "NODE_PATH")))
      org-directory "~/Dropbox/org"
      auth-sources (append (list (concat (getenv "DOOMDIR") "/authinfo.gpg")) auth-sources)
      trash-directory "/tmp/trash")

;;; ui
(setq doom-theme 'doom-one
      fancy-splash-image (concat doom-private-dir "/splashImage.png")
      doom-font (font-spec :family "SauceCodePro Nerd Font" :size 14)
      doom-variable-pitch-font (font-spec :family "MesloLGS Nerd Font" :size 14))

;;; overriding some doom defaults
(if (eq initial-window-system 'x) (toggle-frame-maximized))                     ; maximize emacs on startup
(evil-put-command-property 'evil-yank-line :motion 'evil-line)                  ; yank whole line with Y
(setq display-line-numbers-type 'relative
      evil-goggles-duration 0.2                                                 ; longer operation hinting
      show-trailing-whitespace t
      evil-escape-unordered-key-sequence t                                      ; kj will also be evil-escape
      avy-timeout-seconds 0.2                                                   ; i am inpatient

      select-enable-clipboard nil                                               ; don't use system clipboard with evil
      evil-want-fine-undo t                                                     ; precision is key
      +evil-want-o/O-to-continue-comments nil                                   ; dont auto add comments
      delete-by-moving-to-trash t                                               ; prevent data loss
      eros-eval-result-prefix "⇒ "                                              ; nicer symbol
      evil-snipe-scope 'buffer                                                  ; why snipe line when you can snipe the buffer?
      +workspaces-on-switch-project-behavior nil

      ;; nicer way of constructing unique buffer name but note that this breaks
      ;; `perspective'.
      uniquify-buffer-name-style 'forward
      ;; more natural window splitting
      evil-vsplit-window-right t
      evil-split-window-below t
      ;; other configurations can be found in `RIPGREP_CONFIG_PATH` conf
      counsel-rg-base-command "rg -M 200 --with-filename --no-heading --line-number --color never %s 2>&1; echo 'End of Search'")

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
   :desc "Toggle popups"                     "ph"  #'+popup/toggle
   :desc "Search from home directory"        "sP"  #'+default/search-other-cwd
   :desc "Evil-avy"                          "jj"  #'evil-avy-goto-char-timer
   :desc "format buffer"                     "bf"  #'+format/buffer
   :desc "Close window"                      "wq"  #'evil-quit)

 (:prefix ","
   :desc "eval-last-sexp"               :n   "ee"  #'eval-last-sexp)
 (:map emacs-lisp-mode-map :n "ge"                 #'my-evaluate-nearest-function)
 :n      "p"                                       #'my-paste-and-indent-after
 :n      "P"                                       #'my-paste-and-indent-before
 :g      "C-S-v"                                   #'clipboard-yank
 :g      "C-S-c"                                   #'clipboard-kill-ring-save
 :n      "C-a"                                     #'my-visual-select-whole-buffer
 :n      "C-;"                                     #'my-append-semicolon
 :n      "C-e"                                     #'evil-scroll-up             ; make it easier to scroll with only left hand

 ;; manage window
 :nm     "C-<down>"                                #'evil-window-decrease-height
 :nm     "C-<up>"                                  #'evil-window-increase-height
 :nm     "C-<left>"                                #'evil-window-decrease-width
 :nm     "C-<right>"                               #'evil-window-increase-width
 :n      "M-s"                                     #'+evil-window-split-a
 :n      "M-v"                                     #'+evil-window-vsplit-a
 :n      "gb"                                      #'persp-switch-to-buffer
 :n      "<tab>"                                   #'evil-jump-item
 :g      (kbd "<mouse-8>")                         #'better-jumper-jump-backward
 :g      (kbd "<mouse-9>")                         #'better-jumper-jump-forward
 :n      "C-S-t"                                   #'my-reopen-killed-file

 ;; my custom functions
 (:prefix "\\"
   (:map evil-org-mode-map
     :n :desc "View current commit in magit" "gm"  #'my-magit-from-dotfile-github-link
     :n :desc "Generate dotfile github link" "gl"  #'my-generate-dotfile-github-link)))

;; allow C-j to be interpreted by terminal
(after! evil-collection
  (evil-collection-define-key 'insert 'term-raw-map (kbd "C-j") 'term-send-raw))

;;; popup rules
(after! flycheck (set-popup-rule! "^\\*Flycheck errors\\*" :side 'bottom))
(set-popup-rule! "^\\*doom:scratch\\*" :side 'right :size .50 :select t)

;;; Global-modes
(global-git-gutter+-mode t)
(global-yascroll-bar-mode t)
(company-flx-mode t)
(add-hook! 'prog-mode-hook 'display-fill-column-indicator-mode)

;;; my-modules
(load! "+alias"      doom-private-dir)
(load! "+completion" doom-private-dir)
(load! "+modeline"   doom-private-dir)
(load! "+org"        doom-private-dir)
(load! "+filetype"   doom-private-dir)
(load! "+theme"      doom-private-dir)
(load! "+transient"  doom-private-dir)
(load! "+ui"         doom-private-dir)
(load! "+utils"      doom-private-dir)
