#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail
sudo apt update
sudo apt install -y vim vim-gnome i3 kdiff3 xclip git gitk wget curl make build-essential \
libssl-dev tmux silversearch-ag

git clone https://github.com/pyenv/pyenv.git ~/.pyenv

sudo apt install -y python3-pip

pip3 install --user pipenv
pip3 install virtualenv
pip3 install --user virtualenvwrapper

# install docker
sudo apt install -y docker.io
sudo systemctl start docker
sudo systemctl enable docker
sudo groupadd docker
sudo usermod -aG docker $USER
newgrp docker
sudo apt install docker-compose




# $ sudo mkdir /etc/systemd/system/docker.service.d/ 

# Create file `/etc/systemd/system/docker.service.d/startup_options.conf`
# > # /etc/systemd/system/docker.service.d/override.conf
# > [Service]
# > ExecStart=
# > ExecStart=/usr/bin/dockerd -H fd:// -H tcp://0.0.0.0:2376

# $ sudo systemctl daemon-reload
# $ sudo systemctl restart docker.service



