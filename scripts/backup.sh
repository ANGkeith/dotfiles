#!/bin/bash

# bashrc
cp ~/.bashrc ./bashrc/bashrc

mkdir -p ~/.config/i3
# i3
cp -r ~/.config/i3 ./
cp -r ~/.config/rofi ./

cp ~/.fehbg ./.fehbg
cp ~/Pictures/wallpaper.jpg ./
cp ~/.compton.conf ./

# termite
cp -r ~/.config/termite ./

# vim
cp -r ~/.vim/autoload ./vim/
cp -r ~/.vim/swapfiles ./vim/
cp ~/.vimrc ./vim/vimrc

cp -r ~/.config/keith_envs ./




