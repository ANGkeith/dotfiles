;;; ~/dotfiles/stow/emacs/.config/doom/+theme.el -*- lexical-binding: t; -*-

;; ya-snippet, default color is not obvious enough
(custom-set-faces '(yas-field-highlight-face ((t (:inherit region)))))
;; highlight-indent-guide
(custom-set-faces '(symbol-overlay-default-face ((t (:weight bold)))))

(custom-set-faces
 '(evil-ex-lazy-highlight ((t (:foreground "black" :background "goldenrod3")))))

(custom-set-faces
 '(centaur-tabs-modified-marker-selected
   ((t (:foreground "indianred" ))))
 '(centaur-tabs-modified-marker-unselected
   ((t (:foreground "indianred" )))))

;; doom-one overrides
(if (eq doom-theme 'doom-one)
    (progn
      (custom-set-faces
       '(avy-lead-face ((t (:foreground "limegreen" :background "black"))))
       '(avy-lead-face-0 ((t (:foreground "limegreen" :background "black"))))
       '(avy-goto-char-timer-face ((t (:foreground "limegreen" :background "black")))))

      ;; For ivy-flx, default face is not readable
      (custom-set-faces '(ivy-minibuffer-match-face-1
                          ((t (:foreground "white smoke")))))

      ;; more vibrant rather than material green
      (custom-set-faces '(font-lock-string-face ((t (:foreground "#7bc275")))))
      (custom-set-faces '(success ((t (:foreground "#7bc275")))))

      ;; org-agenda
      (custom-set-faces
       '(org-agenda-current-time
         ((t (:inherit warning))))
       '(org-agenda-date-today
         ((t (:height 1.1))))
       '(org-agenda-date
         ((t (:foreground "#656087")))))

      (custom-set-faces
       '(highlight-indent-guides-character-face ((t (:inherit fill-column-indicator)))))))

;; doom-one-light overrides

;;; patch for doom-one-light themes. ONLY UNCOMMENT IF USING DOOM-ONE-LIGHT themes
(if (eq doom-theme 'doom-one-light)
    (progn
      (custom-set-faces
       '(fill-column-indicator ((t (:foreground "#4078f2"))))
       '(font-lock-keyword-face ((t (:foreground "#4078f2"))))
       '(font-lock-string-face ((t (:foreground "#0d850b"))))
       '(font-lock-comment-face ((t (:weight semi-bold :slant italic))))
       '(default ((t (:background "#ffffff"))))
       '(solaire-default-face ((t (:background "#f3f3f3"))))
       '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
       '(success ((t (:foreground "#2aa34d")))))

      ;; my custom modeline color
      (custom-set-faces
       '(mode-line ((t (:background "#dee8ff" ))))
       '(doom-modeline-info ((t (:foreground "#008081"))))
       '(doom-modeline-battery-charging ((t (:foreground "#008081")))))


      ;; (custom-set-faces '(ivy-minibuffer-match-face-1
      ;;                     ((t (:foreground "gray47")))))
      ))
