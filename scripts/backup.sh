#!/bin/bash
cp ~/.bashrc .
cp ~/.config/i3/config .
cp -r ~/.vim/autoload . 
cp -r ~/.vim/colors .  
cp -r ~/.vim/swapfiles .

git add -a
git commit -m "update"


