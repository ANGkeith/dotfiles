#!/usr/bin/env zsh
if [[ ! -f $HISTFILE ]];then
    mkdir -p $(dirname $HISTFILE)
    touch $HISTFILE
fi

if [[ ! -d "$XDG_CONFIG_HOME"/zgen ]];then
    # Install the zplug
    echo "zinit not found. Attempting to install."
    git clone https://github.com/tarjoilija/zgen.git "$XDG_CONFIG_HOME"/zgen
fi
