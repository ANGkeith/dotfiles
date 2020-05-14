;;; ~/.doom.d/my-functions.el -*- lexical-binding: t; -*-

(define-globalized-minor-mode global-fci-mode fci-mode turn-on-fci-mode)

;; taken from https://emacs.stackexchange.com/questions/7558/how-to-collapse-undo-history/7560#7560
(defun my-undo-collapse-begin (marker)
  "Mark the beginning of a collapsible undo block.
This must be followed with a call to undo-collapse-end with a marker
eq to this one."
  (push marker buffer-undo-list))

(defun my-undo-collapse-end (marker)
  "Collapse undo history until a matching marker."
  (cond
   ((eq (car buffer-undo-list) marker)
    (setq buffer-undo-list (cdr buffer-undo-list)))
   (t
    (let ((l buffer-undo-list))
      (while (not (eq (cadr l) marker))
        (cond
         ((null (cdr l))
          (error "my-undo-collapse-end with no matching marker"))
         ((null (cadr l))
          (setf (cdr l) (cddr l)))
         (t (setq l (cdr l)))))
      ;; remove the marker
      (setf (cdr l) (cddr l))))))

(defmacro my-with-undo-collapse (&rest body)
  "Execute body, then collapse any resulting undo boundaries."
  (declare (indent 0))
  (let ((marker (list 'apply 'identity nil))                                    ; build a fresh list
        (buffer-var (make-symbol "buffer")))
    `(let ((,buffer-var (current-buffer)))
       (unwind-protect
           (progn
             (my-undo-collapse-begin ',marker)
             ,@body)
         (with-current-buffer ,buffer-var
           (my-undo-collapse-end ',marker))))))

;; taken from https://emacs.stackexchange.com/questions/31454/evil-mode-how-to-run-evil-indent-on-the-text-ive-just-pasted
(defun my-paste-and-indent-after ()
  (interactive)
  (save-excursion
    (my-with-undo-collapse
      (evil-paste-after 1)
      (evil-indent (evil-get-marker ?\[) (evil-get-marker ?\])))))
(defun my-paste-and-indent-before ()
  (interactive)
  (save-excursion
    (my-with-undo-collapse
      (evil-paste-before 1)
      (evil-indent (evil-get-marker ?\[) (evil-get-marker ?\])))))

(defun my-git-gutter+-revert-hunks ()
  "Revert hunk at point. If region is active, revert all hunks within the region. Without prompt"
  (interactive)
  (let* ((diffinfos (git-gutter+-selected-diffinfos))
         (one-diffinfo-p (= 1 (length diffinfos))))
    (dolist (diffinfo (nreverse diffinfos))
      (git-gutter+-do-revert-hunk diffinfo))))

;; for C-S-t
(defvar my-killed-file-list nil
  "List of recently killed files.")
(defun add-file-to-killed-file-list ()
  "If buffer is associated with a file name, add that file to the
`my-killed-file-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name my-killed-file-list)))
(add-hook 'kill-buffer-hook #'add-file-to-killed-file-list)
(defun my-reopen-killed-file ()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (when my-killed-file-list
    (find-file (pop my-killed-file-list))))

(defun my-evaluate-nearest-function()
  "Search backward for the first function and evaluates it with ophint"
  (interactive)
  (save-excursion
    (evil-backward-section-begin)
    (let ((beg (point)))
      (evil-jump-item)
      (let ((end (+ 1 (point))))
        (+eval/region beg end)
        (evil-goggles--show-overlay beg end 'evil-goggles-delete-face evil-goggles-duration)))))

(defun my-visual-select-whole-buffer(pos)
  (interactive "d")
  (better-jumper-set-jump pos)                                                  ; add to jump list so that i can jump back
  (evil-visual-select 1 (point-max)))

(defun my-generate-dotfile-github-link ()
  "Grabs the first word in the line and generate a github link to the commit,
assuming that the first word is the commit-hash"
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (msg (string-trim
               (buffer-substring-no-properties beg end)))
         (msg-list (split-string msg " "))
         (commit-hash (nth 0 msg-list)))
    (let ((owner "ANGkeith")
          (project "dotfiles"))
      (kill-whole-line)
      (insert (concat
               "https://github.com/" owner "/" project "/commit/" commit-hash "\n")))))

(defun my-magit-from-dotfile-github-link ()
  "Launch a magit buffer for showing the commit diffs by assuming that the last
path of a url is the commit-hash"
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (msg (string-trim
               (buffer-substring-no-properties beg end)))
         (msg-list (split-string msg "/"))
         (commit-hash (last msg-list 1)))
    (find-file-at-point (concat (getenv "DOOMDIR") "/config.el"))
    (magit-show-commit (substring (format "%s" commit-hash) 1 -1))))

(defun my-append-semicolon ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ";")))
