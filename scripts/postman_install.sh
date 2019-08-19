#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail

# dependencies for postman
sudo apt install -y libgconf2-4

wget https://dl.pstmn.io/download/latest/linux64 -O postman.tar.gz

sudo tar -xzf postman.tar.gz -C /opt
rm postman.tar.gz
sudo ln -s /opt/Postman/Postman /usr/bin/postman

