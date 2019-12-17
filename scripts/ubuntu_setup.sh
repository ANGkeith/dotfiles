#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail

script_root="$(cd $(dirname $BASH_SOURCE[0]) && pwd -P)"
project_root=$(cd $(dirname ${script_root}) && pwd -P)

sudo apt update
sudo apt install -y vim vim-gnome kdiff3 xclip git gitk wget curl make build-essential \
libssl-dev tmux silversearcher-ag konsole exuberant-ctags ctags

sudo update-alternatives --set x-terminal-emulator /usr/bin/konsole

# install python
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
    # to allow pycharm integration with docker
    sudo mkdir /etc/systemd/system/docker.service.d/
    sudo touch /etc/systemd/system/docker.service.d/startup_options.conf
    echo "# /etc/systemd/system/docker.service.d/override.conf
    [Service]
    ExecStart=
    ExecStart=/usr/bin/dockerd -H fd:// -H tcp://0.0.0.0:2375" |
        sudo tee /etc/systemd/system/docker.service.d/startup_options.conf
    sudo systemctl daemon-reload
    sudo systemctl restart docker.service

# install zsh
    sudo apt install -y zsh
    sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
    git clone https://github.com/romkatv/powerlevel10k.git $ZSH_CUSTOM/themes/powerlevel10k
    # zsh-syntax-highlighting
    git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
    # zsh-autosuggestions
    git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

# Install i3
    # Install chrome
    wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
    sudo dpkg -i google-chrome-stable_current_amd64.deb
    rm google-chrome-stable_current_amd64.deb

    # Sscreenlocker
    sudo apt install -y libev-dev libxcb-composite0 libxcb-composite0-dev
        \ libxcb-xinerama0 libxcb-randr0 libxcb-xinerama0-dev libxcb-xkb-dev
        \ libxcb-image0-dev libxcb-util-dev libxkbcommon-x11-dev
        \ libjpeg-turbo8-dev libpam0g-dev
    cd /tmp
    git clone https://github.com/PandorasFox/i3lock-color.git
    cd i3lock-color
    autoreconf --force --install
    rm -rf build/
    mkdir -p build && cd build/
    ../configure \
    --prefix=/usr \
    --sysconfdir=/etc \
    --disable-sanitizers
    make
    # the make will most likely trigger some err flag
    sudo cp i3lock /usr/bin
    cd /tmp
    git clone https://github.com/pavanjadhaw/betterlockscreen
    cd betterlockscreen
    sudo cp betterlockscreen /usr/bin/

    # i3-persist dependency
    sudo apt install -y jq

    # Status Bar
    sudo apt install -y libxcb1-dev libxcb-keysyms1-dev libpango1.0-dev
        \ libxcb-util0-dev libxcb-icccm4-dev libyajl-dev
        \ libstartup-notification0-dev libxcb-randr0-dev
        \ libev-dev libxcb-cursor-dev libxcb-xinerama0-dev libxcb-xkb-dev
        \ libxkbcommon-dev libxkbcommon-x11-dev autoconf xutils-dev libtool
        \ libxcb-shape0-dev cargo
    cd /tmp
    git clone https://github.com/greshake/i3status-rust
    cd i3status-rust && cargo build --release
    sudo cp target/release/i3status-rs /usr/bin/i3status-rs


    # i3_cycle_windows dependency
    sudo apt install -y libjson-perl
    # Screenshot
    sudo apt install -y flameshot

    # install i3-gaps
    sudo add-apt-repository -y ppa:kgilmer/speed-ricer
    sudo apt-get update
    sudo apt install -y i3-gaps
    # should be no use
    # cd /tmp
    # git clone https://github.com/Airblader/xcb-util-xrm
    # cd xcb-util-xrm
    # git submodule update --init
    # ./autogen.sh --prefix=/usr
    # make
    # sudo make install
    # cd /tmp
    # git clone https://www.github.com/Airblader/i3 i3-gaps
    # cd i3-gaps
    # git checkout gaps && git pull
    # autoreconf --force --install
    # rm -rf build
    # mkdir build
    # cd build
    # ../configure --prefix=/usr --sysconfdir=/etc
    # make
    # sudo make install

    # Replacement for dmenu
    sudo apt install -y rofi

    # Used for auto locking terminal
    sudo apt install -y feh xautolock

    # install font
    sudo apt install -y fonts-font-awesome libdbus-1-dev fonts-powerline powerline

    # volume manager
    sudo apt install -y pavucontrol

    # show cpu temperatures
    sudo apt install -y lm-sensors

# Postman
    sudo apt install -y libgconf2-4
    wget https://dl.pstmn.io/download/latest/linux64 -O postman.tar.gz
    sudo tar -xzf postman.tar.gz -C /opt
    rm postman.tar.gz
    suPo ln -s /opt/Postman/Postman /usr/bin/postman

# Cheat
    python -m pip install cheat
    cd /tmp
    sudo git clone https://github.com/cheat/cheat.git
    cd cheat
    cd cheatsheets
    sudo mkdir /usr/share/cheat
    sudo cp -r * /usr/share/cheat

# RE tools
    # disassembly tools
    # gdp-peda
    git clone https://github.com/longld/peda.git ~/peda
    echo "set disassembly-flavor intel" >> ~/.gdbinit
    echo "source ~/peda/peda.py" >> ~/.gdbinit
    # radare2
    sudo apt install -y radare2

# python -m pip install grip
# python -m pip install termcolor

# # ranger dependency
# sudo apt install -y w3m-img
# python -m pip install ranger-fm

# # jupyter
# mkvirtualenv jupyter
# pip install jupyterlab
# pip install jupyter-nbextensions-configurator
# jupyter nbextensions_configurator enable --user
# mkdir -p $(jupyter --data-dir)/nbextensions/
# git clone https://github.com/lambdalisue/jupyter-vim-binding $(jupyter --data-dir)/nbextensions/vim_binding
# deactivate
# pip install --user mypy black autopep8 isort flake8

## Selenium driver (FireFox)
    wget https://github.com/mozilla/geckodriver/releases/download/v0.23.0/geckodriver-v0.23.0-linux64.tar.gz
    sudo sh -c 'tar -x geckodriver -zf geckodriver-v0.23.0-linux64.tar.gz -O > /usr/local/bin/geckodriver'
    sudo chmod +x /usr/local/bin/geckodriver
    rm geckodriver-v0.23.0-linux64.tar.gz


