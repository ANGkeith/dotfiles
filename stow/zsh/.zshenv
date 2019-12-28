#******************************************************************************
#
#  .zshenv
#
#  initial setup file for both interactive and noninteractive zsh
#
#  Read config sequence (except /etc/*)
#
#   login shell
#     $ZDOTDIR/.zshenv
#     $ZDOTDIR/.zprofile
#     $ZDOTDIR/.zshrc
#     $ZDOTDIRA/.zlogin
#
#   interactive zsh
#     $ZDOTDIR/.zshenv
#     $ZDOTDIR/.zshrc
#
#   shell scripts
#     $ZDOTDIR/.zshenv
#
#   remoteley noninteractive zsh (e.x ssh hostname command)
#     $ZDOTDIR/.zshenv
#
#   logout:
#     $ZDOTDIR/.zlogout
#     /etc/zlogout
#
#******************************************************************************

if [[ -z "$XDG_CONFIG_HOME" ]]
then
    export XDG_CONFIG_HOME="$HOME/.config"
fi

if [[ -d "$XDG_CONFIG_HOME/zsh" ]]
then
    export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
fi

