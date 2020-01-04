#!/usr/bin/env zsh
# vim:fdm=marker

# configurations {{{
alias bashrc='vim ~/.bashrc'
alias i3rc='vim ~/.config/i3/config'
alias sourcez="source $ZDOTDIR/.zshrc"
alias tmuxrc='vim ~/.tmux.conf'
alias vimrc='vim ~/.vimrc'
alias zshrc="vim $ZDOTDIR/.zshrc"
# }}}

# docker {{{
alias dockersrm='docker rm -f $(docker ps -aq)'
# }}}

# git {{{
alias ga="git add"
alias gb="git branch"
alias gc="git commit -m"
alias gcl="git clone"
alias gca="git commit --no-edit --amend"
alias gco="git checkout"
alias gcoh="git checkout HEAD"
function gdh() { git diff HEAD $1; }
alias gl="git pull"
alias gitll='git log --graph --pretty=format:'"'"'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an> %Creset'"'"'% --abbrev-commit --date=relative'
alias glrb="git pull --rebase"
alias gp="git push"
alias gpf="git push --force"
alias grb="git rebase"
alias gs="git status -sb"
function gst() {
    if [[ ! -z $1 ]]; then
        git stash push -m $1
    else
        git stash push
    fi
}
alias gsta="git stash apply"
alias gstl="git stash list"
alias gstp="git stash pop"
alias gsts="git stash show -v"
# }}}

# navigations {{{
# cd. - Use fzf to search for a file and cd into the directory of that file
cd.() {
   local file
   local dir
   file=$(fzf +m -q "$1" --preview-window 'right:60%' --preview 'bat --color=always --style=header,grid --line-range :300 {}' ) && dir=$(dirname "$file") && cd "$dir"
}
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
# }}}

# network {{{
alias ipconfig='dig +short myip.opendns.com @resolver1.opendns.com'
# }}}

# tmux {{{
# auto launch tmux
function t() {
    if command -v tmux &> /dev/null && [ -z "$TMUX" ]; then
        tmux attach -t default || tmux new -s default
    fi
}
# }}}

# vim {{{
alias vf='nvim $(fzf)'
alias vim='nvim'
# }}}

# awk {{{
alias c1="awk '{print \$1}'"
alias c2="awk '{print \$2}'"
alias c3="awk '{print \$3}'"
alias c4="awk '{print \$4}'"
alias c5="awk '{print \$5}'"
alias c6="awk '{print \$6}'"
alias c7="awk '{print \$7}'"
alias c8="awk '{print \$8}'"
alias c9="awk '{print \$9}'"
# }}}
function agr {
    ag -l "$1" | xargs perl -pi.bak -e "s/$1/$2/g"
}

