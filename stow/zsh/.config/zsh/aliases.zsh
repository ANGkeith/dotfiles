#!/usr/bin/env zsh
# vim:fdm=marker

# configurations {{{
alias bashrc="vim ~/.bashrc"
alias i3rc="vim $XDG_CONFIG_HOME/i3/config"
alias bspwmrc="vim $XDG_CONFIG_HOME/bspwm/bspwmrc"
alias sourcez="source $ZDOTDIR/.zshrc"
alias tmuxrc="vim $XDG_CONFIG_HOME/tmux/tmux.conf"
alias vimrc="vim $XDG_CONFIG_HOME/vim/vimrc"
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
alias gd="git diff"
function gdh() { git dsf HEAD $1; }
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
   file_path="${LBUFFER}$(__fsel)"
   cd $(dirname $file_path)
   zle fzf-redraw-prompt
   # file
   # local dir
   # file=$(fzf +m -q "$1" --preview-window 'right:60%' --preview 'bat --color=always --style=header,grid --line-range :300 {}' ) && dir=$(dirname "$file") && cd "$dir"
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
        tmux -f $XDG_CONFIG_HOME/tmux/tmux.conf attach -t default  || tmux -f $XDG_CONFIG_HOME/tmux/tmux.conf new -s default
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

agr() {
    if [[ -z $2 ]]; then
        echo "Please provide more arguments"
    else
        ag -0 -l "$1" | ARG_FROM="$1" ARG_TO="$2" xargs -r0 perl -pi -e 's/$ENV{ARG_FROM}/$ENV{ARG_TO}/g'
    fi
    # ag -l "$1" | xargs perl -pi.bak -e "s/$1/$2/g"
}


# color_helper {{{
ansi() {
    text="xYz"; # Some test text
    echo -e "\n                40m   41m   42m   43m   44m   45m   46m   47m";

    for FGs in '    m' '   1m' '  30m' '1;30m' '  31m' '1;31m' '  32m' \
            '1;32m' '  33m' '1;33m' '  34m' '1;34m' '  35m' '1;35m' \
            '  36m' '1;36m' '  37m' '1;37m'; do
        FG=${FGs// /}
        echo -en " $FGs \033[$FG  ${text}  ";
        for BG in 40m 41m 42m 43m 44m 45m 46m 47m; do
            echo -en "$EINS \033[$FG\033[${BG} ${text} \033[0m";
        done
        echo;
    done
    echo;
}

palette() {
    local colors; for n in {000..255}; do colors+=("%F{$n}$n%f"); done; print -cP $colors; 
}
# }}}
