#!/usr/bin/env zsh
# make vim the default editor
export EDITOR=nvim

# XDG base directory specification
# User-specific configurations
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

# virtual env var
export WORKON_HOME=$HOME/.virtualenvs
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
export VIRTUALENVWRAPPER_VIRTUALENV=/usr/bin/virtualenv
# create .venv file in project directory

# pipenv
export PIPENV_VENV_IN_PROJECT=true

# for easy reference to DOTFILE dir
export DOTFILE="$HOME/dotfiles/stow"

# add python `pip install --user` to path
export PATH=$HOME/.local/bin:$PATH

# Nvm
export NVM_DIR="$HOME/.nvm"
export NVM_SOURCE="/usr/share/nvm" # The AUR package installs it here

# fzf
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_OPTS="--preview-window 'right:60%' --preview 'bat --color=always --style=header,grid --line-range :300 {}'"

# cheat
export CHEAT_HIGHLIGHT=magenta

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"


# enable true color
export TERM="xterm-256color"

# zplug will look at the file in this variable
export ZPLUG_LOADFILE=$XDG_CONFIG_HOME/zsh/plugins.zsh

# used by plugins/fzf
export FZF_BASE=~/.local/lib/fzf

# Better transition for changing of vim mode in terminal
export KEYTIMEOUT=1

# Change default location of .zsh_history
export HISTFILE=$XDG_DATA_HOME/zsh/.zsh_history

export VIMINIT='source $MYVIMRC'
export MYVIMRC="$XDG_CONFIG_HOME/vim/vimrc"
