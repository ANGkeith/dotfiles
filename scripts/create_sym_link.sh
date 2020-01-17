#!/usr/bin/env bash
set -o errexit
set -o pipefail
set -o nounset

script_root="$(cd $(dirname $BASH_SOURCE[0]) && pwd -P)"
project_root=$(cd $(dirname ${script_root}) && pwd -P)

mkdir -p ~/.local/bin
mkdir -p ~/.local/etc
mkdir -p ~/.local/lib
mkdir -p ~/.local/share
# so that stow will now sym link the entire `cheat` but only `personal`
mkdir -p ~/.local/share/cheat
cd ${project_root}/stow
stow -v -S -t ~ * --adopt

### don't sym link
rm ~/.config/konsolerc
# dont use variable
cp $HOME/dotfiles/stow/konsole/.config/konsolerc $HOME/.config/konsolerc
git checkout HEAD $HOME/dotfiles/stow/konsole/.config/konsolerc

