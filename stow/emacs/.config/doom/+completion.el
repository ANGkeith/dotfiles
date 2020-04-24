;;; ~/dotfiles/stow/emacs/.config/doom/+completion.el -*- lexical-binding: t; -*-

;; company
(after! company
  (define-key! company-active-map
    "RET"        #'company-complete-selection
    [return]     #'company-complete-selection
    "TAB"        #'company-select-next
    [tab]        #'company-select-next
    [backtab]    #'company-select-previous
    (kbd "jk")   #'company-complete-selection
    "C-n"        #'company-select-next
    "C-p"        #'company-select-previous)
  (map! :i "M-c" #'company-dabbrev))

;; company-box
(after! company-box
  (setq
   company-box-max-candidates 100
   company-box-icons-alist 'company-box-icons-all-the-icons
   company-box-icons-all-the-icons
   (let ((all-the-icons-scale-factor 0.8))
     `(
       (Unknown       . ,(all-the-icons-octicon    "file-text"))
       (Text          . ,(all-the-icons-faicon     "file-text-o"))
       (Method        . ,(all-the-icons-faicon     "cube"))
       (Function      . ,(all-the-icons-faicon     "cube"))
       (Constructor   . ,(all-the-icons-faicon     "cube"))
       (Field         . ,(all-the-icons-faicon     "tag"))
       (Variable      . ,(all-the-icons-faicon     "tag"))
       (Class         . ,(all-the-icons-faicon     "cog"))
       (Interface     . ,(all-the-icons-faicon     "cogs"))
       (Module        . ,(all-the-icons-alltheicon "less"))
       (Property      . ,(all-the-icons-faicon     "wrench"))
       (Unit          . ,(all-the-icons-faicon     "tag"))
       (Value         . ,(all-the-icons-faicon     "tag"))
       (Enum          . ,(all-the-icons-faicon     "file-text-o"))
       (Keyword       . ,(all-the-icons-material   "format_align_center"))
       (Snippet       . ,(all-the-icons-material   "content_paste"))
       (Color         . ,(all-the-icons-material   "palette"))
       (File          . ,(all-the-icons-faicon     "file"))
       (Reference     . ,(all-the-icons-faicon     "tag"))
       (Folder        . ,(all-the-icons-faicon     "folder"))
       (EnumMember    . ,(all-the-icons-faicon     "tag"))
       (Constant      . ,(all-the-icons-faicon     "tag"))
       (Struct        . ,(all-the-icons-faicon     "cog"))
       (Event         . ,(all-the-icons-faicon     "bolt"))
       (Operator      . ,(all-the-icons-faicon     "tag"))
       (TypeParameter . ,(all-the-icons-faicon     "cog"))
       (Template      . ,(all-the-icons-octicon    "file-code"))
       (ElispFunction . ,(all-the-icons-material   "functions"                :face 'all-the-icons-red))
       (ElispVariable . ,(all-the-icons-material   "check_circle"             :face 'all-the-icons-blue))
       (ElispFeature  . ,(all-the-icons-material   "stars"                    :face 'all-the-icons-orange))
       (ElispFace     . ,(all-the-icons-material   "format_paint"             :face 'all-the-icons-pink))))))

;; ivy
(after! ivy
  (map! :in "M-p" #'counsel-yank-pop)
  (ivy-configure  #'counsel-yank-pop
    :height 10
    :format-fn #'counsel--yank-pop-format-function))
