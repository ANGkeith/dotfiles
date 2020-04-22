;;; ~/.doom.d/my-functions.el -*- lexical-binding: t; -*-

(define-globalized-minor-mode global-fci-mode fci-mode turn-on-fci-mode)

(defun alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))


;; taken from https://emacs.stackexchange.com/questions/7558/how-to-collapse-undo-history/7560#7560
(defun undo-collapse-begin (marker)
  "Mark the beginning of a collapsible undo block.
This must be followed with a call to undo-collapse-end with a marker
eq to this one."
  (push marker buffer-undo-list))

(defun undo-collapse-end (marker)
  "Collapse undo history until a matching marker."
  (cond
   ((eq (car buffer-undo-list) marker)
    (setq buffer-undo-list (cdr buffer-undo-list)))
   (t
    (let ((l buffer-undo-list))
      (while (not (eq (cadr l) marker))
        (cond
         ((null (cdr l))
          (error "undo-collapse-end with no matching marker"))
         ((null (cadr l))
          (setf (cdr l) (cddr l)))
         (t (setq l (cdr l)))))
      ;; remove the marker
      (setf (cdr l) (cddr l))))))

(defmacro with-undo-collapse (&rest body)
  "Execute body, then collapse any resulting undo boundaries."
  (declare (indent 0))
  (let ((marker (list 'apply 'identity nil)) ; build a fresh list
        (buffer-var (make-symbol "buffer")))
    `(let ((,buffer-var (current-buffer)))
       (unwind-protect
           (progn
             (undo-collapse-begin ',marker)
             ,@body)
         (with-current-buffer ,buffer-var
           (undo-collapse-end ',marker))))))

;; taken from https://emacs.stackexchange.com/questions/31454/evil-mode-how-to-run-evil-indent-on-the-text-ive-just-pasted
(defun paste-and-indent-after ()
  (interactive)
  (with-undo-collapse
    (evil-paste-after 1)
    (evil-indent (evil-get-marker ?\[) (evil-get-marker ?\]))))
(defun paste-and-indent-before ()
  (interactive)
  (with-undo-collapse
    (evil-paste-before 1)
    (evil-indent (evil-get-marker ?\[) (evil-get-marker ?\]))))

(defun my-git-gutter+-revert-hunks ()
  "Revert hunk at point. If region is active, revert all hunks within the region. Without prompt"
  (interactive)
  (let* ((diffinfos (git-gutter+-selected-diffinfos))
         (one-diffinfo-p (= 1 (length diffinfos))))
    (dolist (diffinfo (nreverse diffinfos))
      (git-gutter+-do-revert-hunk diffinfo))))

(defun my-symbol-overlay-mode ()
  (unless (memq major-mode
                (list 'neotree-mode))
    (symbol-overlay-mode)))

(defun +doom-themes--neotree-no-fringes ()
  (set-window-fringes neo-global--window 3 0))
(defun +neo-buffer--insert-root-entry (node)
  (neo-buffer--node-list-set nil node)
  (cond ((eq neo-cwd-line-style 'button)
         (neo-path--insert-header-buttonized node))
        (t
         (neo-buffer--insert-with-face (neo-path--shorten node (window-body-width))
                                       'neo-root-dir-face)))
  (neo-buffer--newline-and-begin)
  (when neo-show-updir-line
    (neo-buffer--insert-fold-symbol 'close node)
    (insert-button ".."
                   'action '(lambda (x) (neotree-change-root))
                   'follow-link t
                   'face neo-dir-link-face
                   'neo-full-path (neo-path--updir node))
    (neo-buffer--newline-and-begin)))

(defvar killed-file-list nil
  "List of recently killed files.")
(defun add-file-to-killed-file-list ()
  "If buffer is associated with a file name, add that file to the
`killed-file-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name killed-file-list)))
(add-hook 'kill-buffer-hook #'add-file-to-killed-file-list)
(defun reopen-killed-file ()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (when killed-file-list
    (find-file (pop killed-file-list))))
