#!/bin/bash
sudo apt install -y vim vim-gnome i3 kdiff3 xclip git gitk wget curl make build-essential \
libssl-dev tmux silversearch-ag

git clone https://github.com/pyenv/pyenv.git ~/.pyenv

sudo apt install -y python3-pip

pip3 install --user pipenv
pip3 install virtualenv
pip3 install virtualenvwrapper

pip3 install powerline-status

# install docker
sudo apt install -y docker.io
sudo systemctl start docker
sudo systemctl enable docker
sudo groupadd docker
sudo usermod -aG docker $USER
newgrp docker
