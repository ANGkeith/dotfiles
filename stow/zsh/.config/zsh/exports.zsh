#!/usr/bin/env zsh
# make vim the default editor
export EDITOR=nvim

# XDG base directory specification
# User-specific configurations
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

# virtual env var
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
export NVM_SOURCE="/usr/share/nvm" # The AUR package installs it here

# fzf
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_OPTS="--preview-window 'right:60%' --preview 'bat --color=always --style=header,grid --line-range :300 {}'"

# cheat
export CHEAT_HIGHLIGHT=magenta

# pyenv
export PATH="$PYENV_ROOT/bin:$PATH"

# enable true color
export TERM="xterm-256color"

# Better transition for changing of vim mode in terminal
export KEYTIMEOUT=1

export FAST_ALIAS_TIPS_PREFIX="\x1b[33;40m ﯦ  \x1b[0m $(tput bold)"

# Use nvim for navigating man page instead of less
export MANPAGER="/bin/sh -c \"col -b | nvim -c 'set ft=man ts=8 nomod nolist rnu noma' -\""

# history settings
export HISTSIZE=9999999999
export SAVEHIST=$HISTSIZE
export HISTORY_IGNORE="(ls|cd|pwd|exit|cd|\ls --color=tty -la|\ls --color=tty|nvim|gitk)"

# stderred
export LD_PRELOAD="/usr/lib/libstderred.so${LD_PRELOAD:+:$LD_PRELOAD}"
# export STDERRED_BLACKLIST="^(bash|test.*)$"

# Respect XDG {{{

    # cargo
    export CARGO_HOME="$XDG_DATA_HOME"/cargo

    # zsh
    export HISTFILE=$XDG_DATA_HOME/zsh/zsh_history

    # vim
    export VIMINIT='source $MYVIMRC'
    export MYVIMRC="$XDG_CONFIG_HOME/vim/vimrc"

    # less
    # disable less history
    export LESSHISTFILE=-

    # ncurses
    export TERMINFO="$XDG_DATA_HOME"/terminfo
    export TERMINFO_DIRS="$XDG_DATA_HOME"/terminfo:/usr/share/terminfo

    # npm
    export NPM_CONFIG_USERCONFIG=$XDG_CONFIG_HOME/npm/npmrc
    export NVM_DIR="$XDG_DATA_HOME"/nvm
    export NODE_REPL_HISTORY="$XDG_DATA_HOME"/node_repl_history

    # nvidia-settings --config="$XDG_CONFIG_HOME"/nvidia/settings


    # python
    export PYLINTHOME="$XDG_CACHE_HOME"/pylint
    export IPYTHONDIR="$XDG_CONFIG_HOME"/jupyter
    export JUPYTER_CONFIG_DIR="$XDG_CONFIG_HOME"/jupyter

    # virtualenv
    export WORKON_HOME="$XDG_DATA_HOME"/virtualenvs

    # pyenv
    export PYENV_ROOT="$XDG_DATA_HOME"/pyenv

    # wget
    export WGETRC="$XDG_CONFIG_HOME/wgetrc"

    # # xinit
    # export XINITRC="$XDG_CONFIG_HOME/X11/xinitrc"
    # export XSERVERRC="$XDG_CONFIG_HOME/X11/xserverrc"

    # z.lua
    export _ZL_DATA="$XDG_DATA_HOME/z"

    # zplugin
    declare -A ZPLGM
    export ZPLGM[BIN_DIR]="$HOME/.local/bin/zplugin"
    export ZPLGM[HOME_DIR]="$XDG_DATA_HOME/zplugin"
    export ZPLGM[PLUGINS_DIR]="$XDG_DATA_HOME/zplugin/plugins"
    export ZPLGM[SNIPPETS_DIR]="$XDG_DATA_HOME/zplugin/snippets"
    export ZPLGM[ZCOMPDUMP_PATH]="$XDG_CACHE_HOME/zcompdump"

# }}}

# vim: foldmethod=marker
