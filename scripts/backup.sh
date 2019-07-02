#!/bin/bash
echo Working directory = $(pwd)

cp ~/.bashrc .
cp ~/.config/i3/config .
cp -r ~/.vim .  

git add *
git commit -m "update"
git push
echo Done


