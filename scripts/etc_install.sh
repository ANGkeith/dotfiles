#!/bin/bash
sudo apt install -y vim i3 kdiff3 xclip git gitk wget curl make build-essential \
libssl-dev

sudo apt install -y python3-pip

pip3 install --user pipenv
pip3 install virtualenv
pip3 install virtualenvwrapper

git clone https://github.com/pyenv/pyenv.git ~/.pyenv
