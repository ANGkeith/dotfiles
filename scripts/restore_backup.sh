#!/bin/bash

# bashrc
cp ./.bashrc ~/.bashrc

# i3
cp -r ./i3 ~/.config/
cp -r ./rofi ~/.config/
sudo cp ./i3-persist.sh /usr/bin/i3-persist
cp ./.fehbg ~/.fehbg
cp wallpaper.jpg ~/Pictures/wallpaper.jpg
cp .compton.conf ~/.compton.conf

# termite
cp -r ./termite ~/.config/

# powerline config
cp -r ./powerline ~/.config


# vim
cp -r ./vim ~/.vim
cp ./.vimrc ~/.vimrc
