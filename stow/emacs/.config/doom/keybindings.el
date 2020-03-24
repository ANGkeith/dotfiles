;;; ~/.doom.d/keybindings.el -*- lexical-binding: t; -*-
;; overrides
(map! :leader
      ;; overrides
      "`"     nil
      "tab"   nil
      "q"     nil)
(map!          "M-`" nil)
(map! :nm      "C-a" nil)
(map! :nm      "\\"  nil)
(map! :inmg    "C-z" nil)
(map! :inmg    "C-n" nil)

;; <space> mappings
(map! :leader
      :desc "Next window"                       "`"   #'evil-window-next
      :desc "Toggle back and forth buffer"      "TAB" #'alternate-buffer
      :desc "Close and kill window"             "q"   #'kill-buffer-and-window
      :desc "Configurations"                    "ev"  #'doom/find-file-in-private-config
      :desc "Load doom configurations"          "sv"  #'doom/reload
      :desc "M-x"                               "SPC" #'counsel-M-x
      :desc "Jump to last changed"              "jc"  #'goto-last-change
      :desc "Jump to sections"                  "ji"  #'counsel-imenu
      :desc "Evil-avy"                          "jj"  #'evil-avy-goto-char-timer
      :desc "List buffers"                      "gb"  #'ibuffer)

;; , mappings
(map! :prefix ","
      :desc "eval-last-sexp"               :n   "ee"  #'eval-last-sexp)

(map! :n "C-a" (kbd "ggVG"))


(map!
 (:inmg "C-n" 'neotree-toggle)
 (:map neotree-mode-map "C-n" 'neotree-toggle))

(map! :n      "p"         'paste-and-indent-after
      :n      "P"         'paste-and-indent-before
      :nm     "C-<down>"  'evil-window-decrease-height
      :nm     "C-<up>"    'evil-window-increase-height
      :nm     "C-<left>"  'evil-window-decrease-width
      :nm     "C-<right>" 'evil-window-increase-width
      :g      "C-S-v"     'clipboard-yank
      :g      "C-S-c"     'clipboard-kill-ring-save
      :gnm    "C-p"       'my-fzf-find-file
      :gnm    "C-S-p"     'my-fzf-find-file-from-home)

;; org kbd
(map! :desc "Go to org file" :nm "\\o"  (lambda() (interactive) (find-file "~/Dropbox/org/notes.org")))

(map! :g (kbd "<mouse-9>") #'next-buffer)
(map! :g (kbd "<mouse-8>") #'previous-buffer)


;; (define-key (current-global-map) (kbd "<mouse-9>") 'next-buffer)
;; (define-key (current-global-map) (kbd "<mouse-8>") 'previous-buffer)

