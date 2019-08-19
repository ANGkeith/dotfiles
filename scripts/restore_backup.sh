#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail

# bashrc
cp ./.bashrc ~/.bashrc
cp ./.zshrc ~/.zshrc
cp ./.p10k.zsh ~/.p10k.zsh

# i3
cp -r ./i3 ~/.config/
cp -r ./rofi ~/.config/
cp ./.fehbg ~/.fehbg
cp wallpaper.jpg ~/Pictures/wallpaper.jpg
cp .compton.conf ~/.compton.conf

# konsole
cp ./konsole/konsolerc ~/.config/
cp -r ./konsole/konsole ~/.local/share

# vim
cp -r ./vim/* ~/.vim
cp ./.vimrc ~/.vimrc

# tmux
cp ./.tmux.conf ~/.tmux.conf
