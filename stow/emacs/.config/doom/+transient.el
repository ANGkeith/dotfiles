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

;; TODO add ivy-dispatch
