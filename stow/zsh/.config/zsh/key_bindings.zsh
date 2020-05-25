#!/usr/bin/env zsh
# Enable Vi mode in terminal. (Default: bindkey -e)

if [ ! -z $INSIDE_EMACS ]; then
    bindkey -e
    bindkey -M emacs "^R" fzf-history-widget
    bindkey -M emacs '^Z' fg-bg
    bindkey -M emacs '^G' jump-to-file-dir
    bindkey -M emacs '^H' jump-to-file-dir

else
    # In insert mode, type use <c-v> follow by a key to check the keycode

    # Inherit some useful emac mode commands
    bindkey -M viins "^E" end-of-line
    bindkey -M viins "^A" beginning-of-line
    bindkey -M viins "^J" down-line-or-history
    bindkey -M viins "^K" up-line-or-history

    # fzf
    bindkey -M viins -r "^R"
    bindkey -M viins "^R" fzf-history-widget

    bindkey -M viins "^[OM" accept-line                                                      # fix issue of pressing keying shift+<cr> will output M
    bindkey -M viins '^Z' fg-bg
    bindkey -M viins -r '^G'
    bindkey -M viins '^G' jump-to-file-dir

    bindkey -M vicmd 'gg' beginning-of-line
    bindkey -M vicmd 'G'  end-of-line

    bindkey -M viins -r '^H'
    bindkey -M viins '^H' fzf-cd..

fi
