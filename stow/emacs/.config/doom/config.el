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
(setq doom-theme 'doom-one-light
      fancy-splash-image (concat doom-private-dir "/splashImage.png"))

(if (eq doom-theme 'doom-one-light)
    (progn
      (setq doom-variable-pitch-font (font-spec :family "Noto Mono" :size 14))
      (setq doom-font (font-spec :family "SauceCodePro Nerd Font" :size 14))))
(if (eq doom-theme 'doom-one)
  (progn
    (setq doom-variable-pitch-font (font-spec :family "MesloLGS Nerd Font" :size 14))
    (setq doom-font (font-spec :family "SauceCodePro Nerd Font" :size 14))))

;;; overriding some doom defaults
(if (eq initial-window-system 'x) (toggle-frame-maximized))                     ; maximize emacs on startup
(evil-put-command-property 'evil-yank-line :motion 'evil-line)                  ; yank whole line with Y
(setq display-line-numbers-type nil
      +format-with-lsp nil
      evil-goggles-duration 0.2                                                 ; longer operation hinting
      show-trailing-whitespace t
      evil-escape-unordered-key-sequence t                                      ; kj will also be evil-escape
      avy-timeout-seconds 0.2                                                   ; i am inpatient
      solaire-mode-remap-modeline nil                                           ; i want to customize my modeline color
      doom-one-light-brighter-modeline nil
      expand-region-contract-fast-key "e"

      select-enable-clipboard nil                                               ; don't use system clipboard with evil
      evil-want-fine-undo t                                                     ; precision is key
      +evil-want-o/O-to-continue-comments nil                                   ; dont auto add comments
      delete-by-moving-to-trash t                                               ; prevent data loss
      eros-eval-result-prefix "⇒ "                                              ; nicer symbol
      evil-snipe-scope 'buffer                                                  ; why snipe line when you can snipe the buffer?
      doom-themes-neotree-enable-variable-pitch nil                             ; don't use variable pitch for neotree
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
   ;; :desc "Configurations"                    "ev"  #'doom/find-file-in-private-config
   :desc "Project find file"                 "ev"  #'+ivy/projectile-find-file
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
   :desc "list errors"                       "el"  #'flycheck-list-errors
   :desc "Default file manager"              "oc"  #'my-browse-file-directory
   :desc "Close window"                      "wq"  #'evil-quit)

 (:prefix ","
   :desc "eval-last-sexp"               :n   "ee"  #'eval-last-sexp)
 (:map web-mode-map
  :n  "C-k"                                        #'web-mode-tag-previous
  :n  "C-j"                                        #'web-mode-tag-next
  :vn "<tab>"                                      #'web-mode-navigate)
 (:map rjsx-mode-map
  :n  "C-k"                                        #'web-mode-tag-previous
  :n  "C-j"                                        #'web-mode-tag-next
  :n  "[t"                                         #'rjsx-jump-opening-tag
  :n  "]t"                                         #'rjsx-jump-closing-tag
  :i  "<"                                          #'self-insert-command        ; if i am not wrong this inserts the literal <
  :i  ">"                                          #'self-insert-command
  :vn "<tab>"                                      #'rjsx-jump-tag)
 (:map dired-mode-map
  :n     "l"                                       #'dired-find-file
  :n     "h"                                       #'dired-up-directory)
 (:map emacs-lisp-mode-map :n "ge"                 #'my-evaluate-nearest-function)
 :n     "M-h"                                      #'evil-shift-left
 :n     "M-l"                                      #'evil-shift-right
 :n      "p"                                       #'my-paste-and-indent-after
 :n      "P"                                       #'my-paste-and-indent-before
 :g      "C-S-v"                                   #'clipboard-yank
 :g      "C-S-c"                                   #'clipboard-kill-ring-save
 :n      "C-w o"                                   #'centaur-tabs-kill-other-buffers-in-current-group
 :n      "C-a"                                     #'evil-insert-line
 :n      "C-;"                                     #'my-append-semicolon
 :n      "C-,"                                     #'my-append-comma
 :n      "C-e"                                     #'evil-scroll-up             ; make it easier to scroll with only left hand

 ;; manage window
 :nm     "C-<down>"                                #'evil-window-decrease-height
 :nm     "C-<up>"                                  #'evil-window-increase-height
 :nm     "C-<left>"                                #'evil-window-decrease-width
 :nm     "C-<right>"                               #'evil-window-increase-width
 :n      "M-K"                                     #'my-move-line-up
 :n      "M-J"                                     #'my-move-line-down
 :n      "M-s"                                     #'+evil-window-split-a
 :g      "C-s"                                     #'save-buffer
 :n      "gr"                                      #'er/expand-region
 :n      "M-v"                                     #'+evil-window-vsplit-a
 :g      "M-q"                                     #'+vterm/toggle
 :n      "gb"                                      #'persp-switch-to-buffer
 :n      "<tab>"                                   #'evil-jump-item
 :g      (kbd "<mouse-8>")                         #'better-jumper-jump-backward
 :g      (kbd "<mouse-9>")                         #'better-jumper-jump-forward
 :n      "C-S-t"                                   #'my-reopen-killed-file
 :gin      "M-`"                                   #'evil-window-next

 ;; my custom functions
 (:map evil-org-mode-map
  (:prefix "\\"
   :n :desc "View current commit in magit" "gm"  #'my-magit-from-dotfile-github-link
   :n :desc "Generate dotfile github link" "gl"  #'my-generate-dotfile-github-link)))

;;; popup rules
(after! flycheck (set-popup-rule! "^\\*Flycheck errors\\*" :side 'bottom))
(set-popup-rule! "^\\*doom:scratch\\*" :side 'right :size .50 :select t)
(set-popup-rule! "^\\*doom:vterm-popup.*" :side 'bottom :size .50 :select t)

;;; Global-modes
(global-git-gutter+-mode t)
(global-yascroll-bar-mode t)
(company-flx-mode t)
(add-hook! 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook! 'after-change-major-mode-hook #'my-symbol-overlay-mode)

;; vterm
(after! evil-collection
  (evil-collection-define-key 'insert 'term-raw-map (kbd "C-j") 'term-send-raw)); allow C-j to be interpreted by terminal
(map! (:map vterm-mode-map
       :i [return] #'vterm-send-return
       :g "<escape>" nil
       :g [return] #'vterm-send-return
       :g "M-q" #'+vterm/toggle))
(add-hook! 'doom-escape-hook
  (defun forward-escape-key ()
    (when (derived-mode-p 'vterm-mode)
      (vterm--self-insert))))

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

;; yasnippet
;; file named '__' in `+snippets-dir' to be used as file-template
(set-file-template! "\\.html$" :trigger "__")
(set-file-template! "\\.jsx$"  :trigger "__jsx")

(keychain-refresh-environment)
