#!/usr/bin/env zsh
# vim:fdm=marker

# configurations {{{
ealias bashrc="vim ~/.bashrc"
ealias sourcez="source $ZDOTDIR/.zshrc"
ealias tmuxrc="vim $XDG_CONFIG_HOME/tmux/tmux.conf"
ealias vimrc="vim $XDG_CONFIG_HOME/vim/vimrc"
ealias zshrc="vim $ZDOTDIR/.zshrc"
# }}}

# docker {{{
ealias dockersrm='docker rm -fv $(docker ps -aq)'
dex() {
    docker exec -ti "$1" bash
}
dexr() {
    docker exec --user root -ti "$1" bash
}
ddebug() {
    docker commit $1 docker-debug
    docker run -ti --entrypoint=sh docker-debug
}
ealias dcu='docker-compose up'
ealias dcub='docker-compose up --build'
ealias dls='docker container ls'

# }}}

# git {{{
ealias ga="git add"
ealias gaa="git add -A"
ealias gb="git branch"
ealias gc="git commit -m"
ealias gcl="git clone"
ealias gcp="git cherry-pick"
ealias gcpa="git cherry-pick --abort"
ealias gcpc="git cherry-pick --continue"
ealias gca="git commit --no-edit --amend"
ealias gco="git checkout"
ealias gcoh="git checkout HEAD"
ealias gd="git diff"
ealias gdh="git dsf HEAD"
ealias gl="git pull"
alias gitll='git log --graph --pretty=format:'"'"'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an> %Creset'"'"'% --abbrev-commit --date=relative'
ealias glrb="git pull --rebase"
ealias gp="git push"
ealias gpf="git push --force"
ealias gr="git reset"
ealias gr="git reset"
ealias grh="git reset --hard"
ealias grb="git rebase"
ealias grba="git rebase --abort"
ealias grbi="git reset -i"
ealias grbc="git rebase --continue"
ealias gs="git status -sb"
ealias gst="git stash push"
ealias gstn="git stash push -m"
ealias gsta="git stash apply"
ealias gstl="git stash list"
ealias gstp="git stash pop"
ealias gsts="git stash show -v"

# git filter-branch --force --index-filter \
#  "git rm --cached --ignore-unmatch PATH-TO-YOUR-FILE-WITH-SENSITIVE-DATA" \
#  --prune-empty --tag-name-filter cat -- --all
# }}}
#
# kubernetes {{{
    ealias k=kubectl
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
# }}}

# network {{{
ealias ipconfig='dig +short myip.opendns.com @resolver1.opendns.com'
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
ealias vf='nvim $(fzf)'
ealias vim='nvim'
ealias v='nvim'
# }}}

# awk {{{
ealias c1="awk '{print \$1}'"
ealias c2="awk '{print \$2}'"
ealias c3="awk '{print \$3}'"
ealias c4="awk '{print \$4}'"
ealias c5="awk '{print \$5}'"
ealias c6="awk '{print \$6}'"
ealias c7="awk '{print \$7}'"
ealias c8="awk '{print \$8}'"
ealias c9="awk '{print \$9}'"
# }}}

agr() {
    if [[ -z $2 ]]; then
        echo "Please provide more arguments"
    else
        ag --hidden -0 -l "$1" | ARG_FROM="$1" ARG_TO="$2" xargs -r0 perl -pi -e 's/$ENV{ARG_FROM}/$ENV{ARG_TO}/g'
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

get_temp() {
    paste <(cat /sys/class/thermal/thermal_zone*/type) <(cat /sys/class/thermal/thermal_zone*/temp) | column -s $'\t' -t | sed 's/\(.\)..$/.\1°C/' 
}

github() {
    # $1: is the name of repository
    # Usage: github sei40kr/fast-alias-tips-bin
    # Description: This will get from the latest release page the link
    curl -sL https://api.github.com/repos/"$1"/releases/latest | jq -r '.assets[].browser_download_url'
}

ealias k9="kill -9"
ealias grep="grep --color"

# ealias ls="exa -lahgb --icons"

# yarn {{{
    ealias ysd="yarn start:dev"
    ealias yt="yarn test"
    ealias ytu="yarn test"
    ealias yck="yarn code:check"
# }}}
# terraform {{{
    ealias tf="terraform"
    ealias tfi="terraform init"
    ealias tfa="terraform apply"
    ealias tfa!="terraform apply --auto-approve"
    ealias tfp="terraform plan"
    ealias tfd="terraform destroy"
    ealias tfd!="terraform destroy --auto-approve"
# }}}
#

function ngrok() {
  docker run -d --rm --net=host -e NGROK_PORT="$1" wernight/ngrok > /dev/null
  until curl --silent localhost:"${1}" > /dev/null; do
    echo "Waiting for ngrok to be ready"
    sleep 1
  done

  curl --silent http://localhost:4040/api/tunnels | jq --raw-output '.tunnels[0].public_url'
}

