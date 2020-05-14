;;; ~/dotfiles/stow/emacs/.config/doom/+transient.el -*- lexical-binding: t; -*-

(define-transient-command neotree-dispatch ()
  ["Navigation                Actions"
   [("C-j"     "Next sibling"      neotree-select-next-sibling-node)
    ("C-k"     "Previous Siblings" neotree-select-previous-sibling-node)
    ("h  "     "Parent"            +neotree/collapse-or-up)]
   [("x"       "Remove"            neotree-delete-node)
    ("y"       "Copy"              neotree-copy-node)
    ("D"       "Create Directory"  make-directory)]
   [("C    "   "Collapse All"      neotree-collapse-all)
    ("r    "   "Rename or Move"    neotree-rename-node)
    ("C-SPC"   "Preview"           neotree-quick-look)
    ]])

(define-transient-command ivy-dispatch ()
  ["Navigation            Actions                                         Modifiers                         Select                                         Exit                             Export"
   [("C-j  "     "Down"                               next-line)
    ("C-k  "     "Up"                                 previous-line)
    ("C-S-j"     "Scroll down"                        scroll-up-command)
    ("C-S-k"     "Scroll up"                          scroll-down-command)]
   [("C-a"       "Beginning of line"                  evil-beginning-of-line)
    ("C-b"       "Back one character"                 evil-backward-char)
    ("C-r"       "History"                            counsel-minibuffer-history)
    ("C-d"       "Delete history (only after C-r)"    ivy-reverse-i-search-kill)]
   [("C-u"       "Clear line"                         evil-delete-back-to-indentation)
    ("C-v"       "Paste"                              yank)
    ("C-w"       "Back one word"                      ivy-backward-kill-word)
    ("C-z"       "Undo"                               undo)] ;(λ! (ignore-errors (call-interactively #'undo)));
   [("M-i"       "hydra-ivy/body"                     hydra-ivy/body)
    ("C-o"       "Choose what you want to do"         ivy-dispatching-done)]
   [("C-l     "  "Alt select candidate"               ivy-alt-done)
    ("C-RET   "  "Select candidate in another window" +ivy/git-grep-other-window-action)
    ("C-SPC   "  "Preview candidate"                  ivy-call-and-recenter)
    ([return]    "Select candidate"                   ivy-done)]
   [([escape]    "Abort recursive edit"               abort-recursive-edit)
    ("C-g     "  "Abort"                              keyboard-escape-quit)]
   [("C-c C-o "  "Output ivy result to buffer"        ivy-occur)
    ("C-c C-e "  "Output ivy result to buffer"        +ivy/woccur)]])
(advice-add 'ivy-dispatch :before
            (lambda ()
              (interactive)
              (setq-local transient-display-buffer-action
                          '(display-buffer-in-side-window bottom))))

;; TODO add transient for org agenda
