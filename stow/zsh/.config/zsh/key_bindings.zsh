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

# fix issue of pressing keying shift+<cr> will output M
bindkey "^[OM" accept-line

# C-Z a toggle behaviour
fg-bg() {
  if [[ $#BUFFER -eq 0 ]]; then
    fg
  else
    zle push-input
  fi
}

# shell function make into a widget
zle -N fg-bg
bindkey '^Z' fg-bg

jump_to_file_dir() {
   file_path="${LBUFFER}$(__fsel)"
   cd $(dirname $file_path)
   zle fzf-redraw-prompt
}
zle -N jump_to_file_dir
bindkey -r '^G'
bindkey '^G' jump_to_file_dir

# cd.. Use fzf to cd into one of its parent/ancenstor dir
cd..() {
  local declare dirs=()
  get_parent_dirs() {
    if [[ -d "${1}" ]]; then dirs+=("$1"); else return; fi
    if [[ "${1}" == '/' ]]; then
      for _dir in "${dirs[@]}"; do echo $_dir; done
    else
      get_parent_dirs $(dirname "$1")
    fi
  }
  local DIR=$(get_parent_dirs $(realpath "${1:-$PWD}") | fzf-tmux --tac)
  cd "$DIR"
}
zle -N cd..
bindkey -r '^H'
bindkey '^H' cd..
