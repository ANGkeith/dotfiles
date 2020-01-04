#!/usr/bin/env zsh
if [[ ! -d ~/.zplug ]];then
    # Install the zplug
    echo "zplug plugin manager not found. Attempting to install."
    curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh
else
    echo "[$(basename $0):$LINENO]: This section can be commented out. zplug is already installed"
fi


if [[ ! -d $XDG_DATA_HOME/zsh ]]; then
    echo "$XDG_DATA_HOME/zsh does not exists. Creating directory"
    mkdir $XDG_DATA_HOME/zsh
else
    echo "[$(basename $0):$LINENO]: This section can be commented out. $XDG_DATA_HOME/zsh already exists"
fi
