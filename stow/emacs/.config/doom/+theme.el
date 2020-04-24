;;; ~/dotfiles/stow/emacs/.config/doom/+theme.el -*- lexical-binding: t; -*-

(custom-set-faces
 '(centaur-tabs-modified-marker-selected
   ((t (:foreground "indianred" ))))
 '(centaur-tabs-modified-marker-unselected
   ((t (:foreground "indianred" )))))

(custom-set-faces
 '(avy-lead-face ((t (:foreground "limegreen" :background "black"))))
 '(avy-lead-face-0 ((t (:foreground "limegreen" :background "black"))))
 '(avy-goto-char-timer-face ((t (:foreground "limegreen" :background "black")))))

;; For ivy-flx, default face is not readable
(custom-set-faces '(ivy-minibuffer-match-face-1
                    ((t (:foreground "white smoke")))))

(custom-set-faces
 '(evil-ex-lazy-highlight ((t (:foreground "black" :background "goldenrod3")))))


;; ;;; patch for doom-one-light themes. ONLY UNCOMMENT IF USING DOOM-ONE-LIGHT themes
;;   (custom-set-faces
;;   '(fill-column-indicator ((t (:foreground "#4078f2"))))
;;   '(font-lock-keyword-face ((t (:foreground "#4078f2"))))
;;   '(font-lock-string-face ((t (:foreground "#0d850b"))))
;;   '(font-lock-comment-face ((t (:weight semi-bold :slant italic))))
;;   '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
;;   '(success ((t (:foreground "#2aa34d"))))))

(custom-set-faces '(symbol-overlay-default-face ((t (:weight bold)))))
