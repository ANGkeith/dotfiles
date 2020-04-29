;;; ~/dotfiles/stow/emacs/.config/doom/+org.el -*- lexical-binding: t; -*-

(defun setup-org-workspace ()
  "Used to create my org workspace"
  (interactive)
  (let ((org-workspace-name "org"))
    (unless (+workspace-exists-p org-workspace-name)
      (message "Created org workspace")
      (+workspace/new org-workspace-name)
      (find-file "~/Dropbox/org/todo.org"))))
(advice-add '+workspace/switch-to-1 :before #'setup-org-workspace)

;; Use mixed-pitch in `org-mode'
(defun my-org-mode-font-settings ()
  "Use mixed-pitch and larger fonts in org mode"
  (setq-local  buffer-face-mode-face '(:height 150))                            ; HACK Use a larger font inside orgmode
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
        org-agenda-align-tags-to-column org-tags-column
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

;; (setq org-agenda-align-tags-to-column nil)

;; (defun my-monthly-agenda ()
;;   (interactive)
;;   (let ((org-agenda-custom-commands
;;          '(("w" "Whole Agenda no matter status"
;;             ((todo "TODO|IN-PROGRESS|DONE|CANCELLED")))))

;;         (org-super-agenda-groups
;;          '((:auto-map
;;             (lambda (item)
;;               (let ((ts (assoc "TIMESTAMP" (org-entry-properties))))
;;                 (if ts
;;                     (let* ((date-parts (org-parse-time-string (cdr ts)))
;;                            (month (nth 4 date-parts)) (year (nth 5 date-parts)))
;;                       (format-time-string "%B %Y" (encode-time 1 1 1 1 month year)))
;;                   ;; else
;;                   "W/o timestamp")))))))
;;     (org-agenda nil "w")))

;; HACK my work around to get `org-refile' to inherit tags.
(defun my-inherit-tags ()
  "Current heading to inherit parents tags"
  (interactive)
  (let ((inherited-tags (org-get-tags) ))
    (org-back-to-heading)
    (org-set-tags inherited-tags)))
(advice-add #'org-refile :before #'my-inherit-tags)
