#!/bin/bash

# bashrc
cp ./.bashrc ~/.bashrc
cp ./.zshrc ~/.zshrc
cp ./.p10k.zsh ~/.p10k.zsh

# i3
cp -r ./i3 ~/.config/
cp -r ./rofi ~/.config/
sudo cp ./scripts/i3-persist.sh /usr/bin/i3-persist
cp ./.fehbg ~/.fehbg
cp wallpaper.jpg ~/Pictures/wallpaper.jpg
cp .compton.conf ~/.compton.conf

# termite
cp -r ./termite ~/.config/

# vim
cp -r ./vim ~/.vim
cp ./.vimrc ~/.vimrc

# tmux
cp ./.tmux.conf ~/.tmux.conf
