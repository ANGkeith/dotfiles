;;; ~/dotfiles/stow/emacs/.config/doom/+org.el -*- lexical-binding: t; -*-

;; Use mixed-pitch in `org-mode'
(defun my-org-mode-font-settings ()
  "Use mixed-pitch and larger fonts in org mode"
  (buffer-face-mode)
  (mixed-pitch-mode))
(defun my-org-agenda-mode-font-settings ()
  (my-org-mode-font-settings)
  (evil-normal-state))                                                          ; Forces the cursor to be in normal-state
(add-hook #'org-mode-hook #'my-org-mode-font-settings)
(add-hook #'org-agenda-mode-hook #'my-org-agenda-mode-font-settings)

;; org
(map!
 (:map org-mode-map :prefix ","
   :n "s" #'org-sort)
 (:leader                :n  "oa"   #'org-agenda))
(after! org
  (with-no-warnings
    (custom-declare-face '+org-todo-refactor '((t (:inherit (bold highlight-numbers-number org-todo)))) "")
    (custom-declare-face '+org-todo-fixme '((t (:inherit (bold error org-todo)))) "")
    (custom-declare-face '+org-todo-inprogress '((t (:inherit (bold font-lock-keyword-face org-todo)))) ""))
  (setq-default
   org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p)"
                                 "WAIT(w)" "HOLD(h)"
                                 "|" "DONE(d)" "KILL(k)" "ASSIGNED(a)")
                       (sequence "[ ](T)" "[-](P)" "|" "[X](D)")
                       (sequence "REFACTOR(r)" "FIXME(x)" "|" "DONE"))
   org-todo-keyword-faces '(("[-]" . +org-todo-inprogress)
                            ("IN-PROGRESS" . +org-todo-inprogress)
                            ("WAIT" . +org-todo-onhold)
                            ("HOLD" . +org-todo-onhold)
                            ("REFACTOR" . +org-todo-refactor)
                            ("FIXME" . +org-todo-fixme)
                            ))

  (setq org-log-done t                                                          ; input timestamp when task is completed
        org-tags-column 80
        org-catch-invisible-edits t
        ;; prettify
        org-hide-emphasis-markers t
        org-ellipsis " ▾ "
        org-superstar-headline-bullets-list '("⁖")
        org-fontify-emphasized-text t
        org-emphasis-alist
        '(("*" bold)
          ("/" italic)
          ("_" underline)
          ("=" (org-verbatim :inherit rectangle-preview))
          ("~" (org-code :inherit rectangle-preview))
          ("+" (:strike-through t))))
  (appendq! +pretty-code-symbols
            '(:checkbox    "☐"
                           :pending     "◼"
                           :checkedbox  "☑"))
  (set-pretty-symbols! 'org-mode
    :merge t
    :checkbox    "[ ]"
    :pending     "[-]"
    :checkedbox  "[X]")
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'org-babel-remove-result nil 'local))))

;; org-agenda
(add-hook 'org-agenda-mode-hook #'org-super-agenda-mode)

;; HACK my work around to get `org-refile' to inherit tags.
(defun my-inherit-tags ()
  "Current heading to inherit parents tags"
  (interactive)
  (let ((inherited-tags (org-get-tags) ))
    (org-back-to-heading)
    (org-set-tags inherited-tags)))
(advice-add #'org-refile :before #'my-inherit-tags)

;;; org-agenda configurations
(after! org
  (setq org-agenda-start-day "-1d"
        org-agenda-span 'week
        org-agenda-start-with-log-mode t
        org-agenda-align-tags-to-column 'auto                                   ; flush tags to right
        ;; add a line in between each day
        org-agenda-format-date (lambda (date)
                                 (concat "\n"
                                         (make-string (window-width) 9472)
                                         "\n"
                                         (org-agenda-format-date-aligned date)))
        org-super-agenda-groups
        '((:name "Important"
                 :face (:inherit warning)
                 :priority "A")
          (:name "Schedule"
                 :time-grid t)
          (:name "Due Today"
                 :deadline today)
          (:name "Toady's Task"
                 :date today)
          (:name "Overdue"
                 :scheduled past
                 :deadline past)
          (:name "Upcoming"
                 :deadline future))))
;; fixes the weird `org-super-agenda' "j-k" behaviour
(map! (:map org-agenda-mode-map
        "j"   #'org-agenda-next-line
        "k"   #'org-agenda-previous-line))
(after! evil-org-agenda
  (map!
   (:map evil-org-agenda-mode-map
     :m "w"   #'org-agenda-week-view
     :m "d"   #'org-agenda-day-view
     :m "ci"   #'org-clock-in
     :m "co"   #'org-clock-out
     :m "<escape>"   #'evil-force-normal-state
     :m "C-j" #'org-agenda-next-date-line
     :m "C-k" #'org-agenda-previous-date-line)))
