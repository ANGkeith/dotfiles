#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail

# bashrc
cp ~/.bashrc ./.bashrc
cp ~/.zshrc ./.zshrc
cp ~/.p10k.zsh ./.p10k.zsh

# i3
cp -r ~/.config/i3 ./
cp -r ~/.config/rofi ./

cp ~/.fehbg ./.fehbg
cp ~/Pictures/wallpaper.jpg ./
cp ~/.compton.conf ./

# konsole
cp ~/.config/konsolerc ./konsole
cp -r ~/.local/share/konsole ./konsole

# ranger
cp -r ~/.config/ranger ./

# vim
cp -r ~/.vim/autoload ./vim/
cp -r ~/.vim/swapfiles ./vim/
cp -r ~/.vim/colors ./vim/
cp ~/.vimrc ./.vimrc

# tmux
cp ~/.tmux.conf ./.tmux.conf

# cheat
cp ~/.cheat/* ./.cheat
