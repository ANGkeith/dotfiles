#!/usr/bin/env zsh
# Enable Vi mode in terminal. (Default: bindkey -e)
bindkey -v

# In insert mode, type use <c-v> follow by a key to check the keycode

# Inherit some useful emac mode commands
bindkey "^E" end-of-line
bindkey "^A" beginning-of-line
bindkey "^P" up-line-or-history
bindkey "^N" down-line-or-history

# fzf
bindkey -r "^R"
bindkey "^R" fzf-history-widget
bindkey "^T" fzf-file-widget
bindkey "^I" fzf-completion

# fix issue of pressing keying shift+<cr> will output M
bindkey "^[OM" accept-line

# history-substring-search from oh-my-zsh plugin library
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down


# C-Z a toggle behaviour
function fg-bg() {
  if [[ $#BUFFER -eq 0 ]]; then
    fg
  else
    zle push-input
  fi
}
zle -N fg-bg
bindkey '^Z' fg-bg

