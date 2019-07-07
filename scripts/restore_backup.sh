#!/bin/bash
git pull
# bashrc
cp ./bashrc/bashrc ~/.bashrc

# i3
cp -r ./i3/ ~/.config/
cp -r ./rofi ~/.config/

# vim
cp -r ./vim/* ~/.vim
cp ./vim/vimrc ~/.vimrc

cp -r ./keith_envs ~/.config/

