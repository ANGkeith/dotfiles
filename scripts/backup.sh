#!/bin/bash

# bashrc
cp ~/.bashrc ./bashrc/bashrc

mkdir -p ~/.config/i3
# i3
cp -r ~/.config/i3 ./
cp -r ~/.config/rofi ./

# vim
cp -r ~/.vim/autoload ./vim/
cp -r ~/.vim/swapfiles ./vim/
cp ~/.vimrc ./vim/vimrc

git add *

