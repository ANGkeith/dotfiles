#!/bin/bash
# install dependencies
sudo apt install -y g++ libgtk-3-dev gtk-doc-tools gnutls-bin \
    valac intltool libpcre2-dev libglib3.0-cil-dev libgnutls28-dev \
    libgirepository1.0-dev libxml2-utils gperf build-essential

# install vte-ng
git clone https://github.com/thestinger/vte-ng.git
echo export LIBRARY_PATH="/usr/include/gtk-3.0:$LIBRARY_PATH"
cd vte-ng && ./autogen.sh && make && sudo make install
cd ..
sudo rm -r vte-ng/

# install termite
git clone --recursive https://github.com/thestinger/termite.git
cd termite && make && sudo make install
sudo ldconfig
sudo mkdir -p /lib/terminfo/x
sudo ln -s /usr/local/share/terminfo/x/xterm-termite /lib/terminfo/x/xterm-termite
sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator /usr/local/bin/termite 60
cd ..
sudo rm -r termite
pip install powerline-status
