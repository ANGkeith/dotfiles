#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail

# add i3-persist script
sudo apt install -y jq
sudo cp ./scripts/i3_persist.sh /usr/bin/i3-persist

sudo apt install -y libxcb1-dev libxcb-keysyms1-dev libpango1.0-dev libxcb-util0-dev libxcb-icccm4-dev libyajl-dev libstartup-notification0-dev libxcb-randr0-dev libev-dev libxcb-cursor-dev libxcb-xinerama0-dev libxcb-xkb-dev libxkbcommon-dev libxkbcommon-x11-dev autoconf xutils-dev libtool libxcb-shape0-dev cargo

cd /tmp
git clone https://github.com/Airblader/xcb-util-xrm
cd xcb-util-xrm
git submodule update --init
./autogen.sh --prefix=/usr
make
sudo make install

cd /tmp
git clone https://www.github.com/Airblader/i3 i3-gaps
cd i3-gaps
git checkout gaps && git pull
autoreconf --force --install
rm -rf build
mkdir build
cd build
../configure --prefix=/usr --sysconfdir=/etc
make
sudo make install

# Replacement for dmenu
sudo apt install -y rofi

# Used for auto locking terminal
sudo apt install -y feh xautolock

# install font
sudo apt install -y fonts-font-awesome libdbus-1-dev fonts-powerline powerline

# status bar 
cd /tmp
git clone https://github.com/greshake/i3status-rust
cd i3status-rust && cargo build --release
sudo cp target/release/i3status-rs /usr/bin/i3status-rs

# volume manager
sudo apt install -y pavucontrol

# show cpu temperatures
sudo apt install -y lm-sensors

# install i3-gaps
sudo add-apt-repository -y ppa:kgilmer/speed-ricer
sudo apt-get update
sudo apt install -y i3-gaps


