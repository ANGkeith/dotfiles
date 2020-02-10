#!/usr/bin/env zsh
if [[ ! -f $HOME/.local/lib/zinit/zinit.zsh ]];then
    # Install the zplug
    echo "zinit not found. Attempting to install."
    git clone https://github.com/zdharma/zinit $HOME/.local/lib/zinit
fi

if [[ ! -f $HISTFILE ]];then
    mkdir -p $(dirname $HISTFILE)
    touch $HISTFILE
fi
