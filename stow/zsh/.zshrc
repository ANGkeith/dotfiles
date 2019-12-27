# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="powerlevel10k/powerlevel10k"
POWERLEVEL9K_MODE="nerdfont-complete"
# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  # git

  # provides suggestion as I type based on history
  zsh-autosuggestions
  z
  zsh-syntax-highlighting
  fzf
  # provides function for searching history based on substring
  history-substring-search
)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias ohmyzsh="mate ~/.oh-my-zsh"
#


# ====================================================================== Custom

# allow change of directory by keying in only the directory path, use `setopt
# noautocd` and etc to reverse the changes
setopt  autocd autopushd

# set the respective extensions to open with vim
alias -s {yml,yaml,py,md,txt,vim}=vim
# ============================================================== Custom Exports
# make vim the default editor
export EDITOR=vim

# virtual env var
export WORKON_HOME=$HOME/.virtualenvs
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
export VIRTUALENVWRAPPER_VIRTUALENV=/usr/bin/virtualenv
# create .venv file in project directory
export PIPENV_VENV_IN_PROJECT=true
source /usr/bin/virtualenvwrapper.sh

# for easy reference to DOTFILE dir
export DOTFILE="$HOME/dotfiles/stow"

# add python `pip install --user` to path
export PATH=$HOME/.local/bin:$PATH

# Defer initialization of nvm until nvm, node or a node-dependent command is
# run. Ensure this block is only run once if .bashrc gets sourced multiple times
# by checking whether __init_nvm is a function.
if [ -s "$HOME/.nvm/nvm.sh" ]; then
  export NVM_DIR="$HOME/.nvm"
  export NVM_SOURCE="/usr/share/nvm" # The AUR package installs it here
  # https://www.growingwiththeweb.com/2018/01/slow-nvm-init.html
  # [ -s "$NVM_DIR/bash_completion" ] && . "$NVM_DIR/bash_completion"
  # nvim depends on npm
  declare -a __node_commands=('nvm' 'node' 'npm' 'yarn' 'gulp' 'grunt' 'webpack' 'nvim' )
  function __init_nvm() {
    for i in "${__node_commands[@]}"; do unalias $i; done
    [ -s "$NVM_SOURCE/nvm.sh" ] && . "$NVM_SOURCE/nvm.sh"  # Load NVM
    unset __node_commands
    unset -f __init_nvm
  }
  for i in "${__node_commands[@]}"; do alias $i='__init_nvm && '$i; done
fi

# fzf configurations
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'
# fzf to use rg instead of find
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_OPTS="--preview-window 'right:60%' --preview 'bat --color=always --style=header,grid --line-range :300 {}'"


# cheat
export CHEAT_HIGHLIGHT=magenta

# ============================================================== Custom Aliases
# utils
alias pycharm='/opt/pycharm-2019.2.3/bin/pycharm.sh & disown $!'
alias pycharmf='/opt/pycharm-2019.2.3/bin/pycharm.sh . & disown $!'

alias copy='xclip -sel clipboard'
alias untar='tar -zxvf'
alias ipconfig='dig +short myip.opendns.com @resolver1.opendns.com'
alias gitll='git log --graph --pretty=format:'"'"'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an> %Creset'"'"'% --abbrev-commit --date=relative'
alias dockersrm='docker rm -f $(docker ps -aq)'
alias startdb='docker run --rm   --name pg-docker -e POSTGRES_PASSWORD=docker -d -p 55432:5432 -v $HOME/docker/volumes/postgres:/var/lib/postgresql/data  postgres'

# config files
alias bashrc='vim ~/.bashrc'
alias i3rc='vim ~/.config/i3/config'
alias vimrc='vim ~/.vimrc'
alias tmuxrc='vim ~/.tmux.conf'
alias zshrc="vim ~/.zshrc"

# pyenv path
# git alias
alias ga="git add"
alias gb="git branch"
alias gc="git commit -m"
alias gcl="git clone"
alias gca="git commit --no-edit --amend"
alias gco="git checkout"
alias gcoh="git checkout HEAD"
alias gl="git pull"
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
function gdh() {
    git diff HEAD $1
}
alias vf='nvim $(fzf)'
alias vim='nvim'

# pyenv path
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

function t() {
    # auto launch tmux
    if command -v tmux &> /dev/null && [ -z "$TMUX" ]; then
        tmux attach -t default || tmux new -s default
    fi
}

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# enable true color
export TERM="xterm-256color"

neofetch

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

bindkey -v
set editing-mode vi
set blink-matching-paren on

# inherit some useful emac mode commands
bindkey "^E" end-of-line
bindkey "^A" beginning-of-line
bindkey "^P" up-line-or-history
bindkey "^N" down-line-or-history
bindkey "^R" fzf-history-widget
bindkey "^T" fzf-file-widget
bindkey "^[c" fzf-cd-widget
bindkey "^I" fzf-completion

bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# removes the delay when changing between modes
export KEYTIMEOUT=1

# Make CTRL-Z background things and unbackground them.
function fg-bg() {
  if [[ $#BUFFER -eq 0 ]]; then
    fg
  else
    zle push-input
  fi
}
zle -N fg-bg
bindkey '^Z' fg-bg
