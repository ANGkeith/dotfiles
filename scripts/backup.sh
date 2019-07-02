#!/bin/bash

# bashrc
cp ~/.bashrc ./bashrc/bashrc

# i3
cp ~/.config/i3/config ./i3/config

# vim
cp -r ~/.vim/autoload ./vim/
cp -r ~/.vim/colors ./vim/  
cp -r ~/.vim/swapfiles ./vim/
cp ~/.vimrc ./vim/vimrc

git add *


