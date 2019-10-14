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

# ranger
cp -r ./ranger ~/.config/

# vim
cp -r ./vim/* ~/.vim
cp ./.vimrc ~/.vimrc

# python
cp ./.isort.cfg ~/.isort.cfg

# tmux
cp ./.tmux.conf ~/.tmux.conf

# cheat
mkdir -p ~/.cheat
cp .cheat/* ~/.cheat

# ctags
cp .ctags ~/.ctags

# global gitignore files
cp .gitignore_global ~/.gitignore_global
git config --global core.excludesfile ~/.gitignore_global