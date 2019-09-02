#!/bin/bash

python -m pip install cheat
cd /tmp
sudo git clone https://github.com/cheat/cheat.git
cd cheat
cd cheatsheets
sudo mkdir /usr/share/cheat
sudo cp -r * /usr/share/cheat



