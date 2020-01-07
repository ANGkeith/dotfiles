#!/usr/bin/env zsh
if [[ ! -f $HOME/.local/lib/zplugin/zplugin.zsh ]];then
    # Install the zplug
    echo "zplugin not found. Attempting to install."
    git clone https://github.com/zdharma/zplugin $HOME/.local/lib/zplugin
fi

if [[ ! -f $HISTFILE ]];then
    mkdir -p $(dirname $HISTFILE)
    touch $HISTFILE
fi
