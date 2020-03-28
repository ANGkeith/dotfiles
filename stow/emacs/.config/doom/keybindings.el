;;; ~/.doom.d/keybindings.el -*- lexical-binding: t; -*-

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
