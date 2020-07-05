#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail
set -o xtrace

script_root="$(cd $(dirname $BASH_SOURCE[0]) && pwd -P)"
# shellcheck disable=SC2034
project_root=$(cd $(dirname ${script_root}) && pwd -P)

sudo apt update

sudo apt install -y xcape

sudo apt install -y moreutils # to use the sponge binaries

# sshd
sudo apt install -y openssh-server

sudo apt install -y libssl-dev
sudo apt install -y wget curl net-tools

# terminal
sudo apt install -y konsole tmux
sudo update-alternatives --set x-terminal-emulator /usr/bin/konsole

# search
sudo apt install ripgrep silversearcher-ag
sudo apt install -y fd-find
sudo ln -s /bin/fdfind /bin/fd

# text editor
sudo apt install -y keychain
sudo apt install -y aspell aspell-en
sudo apt install -y libsqlite3-dev sqlite3

command -v emacs || $(
        git clone -b emacs-27 git://git.sv.gnu.org/emacs.git /tmp/emacs
        cd /tmp/emacs
        sudo apt build-dep emacs
        ./autogen.sh
        ./configure --with-mailutils --with-modules --prefix="${HOME}/.local/lib/emacs"
        git clone --depth 1 https://github.com/hlissner/doom-emacs "$XDG_CONFIG_HOME"/emacs
        "$XDG_CONFIG_HOME"/emacs/bin/doom install
    )

sudo apt install -y exuberant-ctags ctags
sudo apt install -y neovim
# NOTE https://bugs.launchpad.net/ubuntu/+source/rust-bat/+bug/1868517
# sudo apt install -y bat # previewing fzf

# stderred
[[ ! -f /usr/lib/libstderred.so ]] && $(
        sudo apt -y install build-essential cmake
        git clone git://github.com/sickill/stderred.git /tmp/stderred
        cd /tmp/stderred
        make
        sudo mv /tmp/stderred/build/libstderred.so /usr/lib/libstderred.so
    )

# install python
    command -v pyenv || git clone https://github.com/pyenv/pyenv.git "$PYENV_ROOT"
    sudo apt install -y python3-pip
    pip3 install --user pipenv
    pip3 install virtualenv
    pip3 install --user virtualenvwrapper

# python packages

# install docker
    sudo apt install -y docker.io
    sudo systemctl start docker
    sudo systemctl enable docker
    sudo usermod -aG docker "$USER"
    sudo apt -y install docker-compose

# install zsh
    sudo apt install -y zsh
    sudo apt install -y fzf

#     # volume manager
    sudo apt install -y pavucontrol

# Cheat
    # install cheat binary from github page directly instead
    [[ -d "$XDG_DATA_HOME"/cheat/community ]] || git clone https://github.com/cheat/cheatsheets.git "$XDG_DATA_HOME"/cheat/community

# fixes keychron keyboard
    echo "options hid_apple fnmode=0" | sudo tee /etc/modprobe.d/hid_apple.conf

    sudo snap install postman

# fonts
    for font in "SourceCodePro/Black/complete/Sauce Code Pro Black Nerd Font Complete.ttf" \
        "SourceCodePro/Black-Italic/complete/Sauce Code Pro Black Italic Nerd Font Complete.ttf" \
        "SourceCodePro/Medium-Italic/complete/Sauce Code Pro Medium Italic Nerd Font Complete.ttf" \
        "SourceCodePro/Medium/complete/Sauce Code Pro Medium Nerd Font Complete.ttf" \
        "SourceCodePro/Semibold-Italic/complete/Sauce Code Pro Semibold Italic Nerd Font Complete.ttf" \
        "SourceCodePro/Semibold/complete/Sauce Code Pro Semibold Nerd Font Complete.ttf" \
        "SourceCodePro/Bold/complete/Sauce Code Pro Bold Nerd Font Complete.ttf" \
        "SourceCodePro/Bold-Italic/complete/Sauce Code Pro Bold Italic Nerd Font Complete.ttf" \
        "SourceCodePro/Italic/complete/Sauce Code Pro Italic Nerd Font Complete.ttf" \
        "SourceCodePro/Regular/complete/Sauce Code Pro Nerd Font Complete.ttf" \
        "Meslo/S/Regular/complete/Meslo LG S Regular Nerd Font Complete.ttf"; do
        wget --directory-prefix ~/.local/share/fonts "https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/${font}";
    done;
    sudo apt install fonts-symbola

# zoom
    command -v zoom || $(
        wget -O /tmp/zoom.deb https://zoom.us/client/latest/zoom_amd64.deb
        cd /tmp/
        sudo apt install -y zoom.deb
    )

# nodejs
    command -v nvm || $(
            git clone https://github.com/nvm-sh/nvm.git /tmp/nvm
            cd /tmp/nvm
            git checkout `git describe --abbrev=0 --tags --match "v[0-9]*" $(git rev-list --tags --max-count=1)`
            chmod +x /tmp/nvm/nvm.sh
            sudo mv /tmp/nvm/nvm.sh /usr/share/nvm/nvm.sh
        )
    NVM_SOURCE=/usr/share/nvm
    [ -s "$NVM_SOURCE"/nvm.sh ] && . "$NVM_SOURCE"/nvm.sh  # Load NVM
    nvm install "$NODE_DEFAULT_VERSION"
    nvm alias default "$NODE_DEFAULT_VERSION"
    nvm use default
# yarn
    sudo curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
    sudo sh -c "echo 'deb https://dl.yarnpkg.com/debian/ stable main' >> /etc/apt/sources.list"
    sudo apt update
    sudo apt --no-install-recommends install yarn



sudo apt install -y gimp inkscape
sudo apt install -y mpv

# linters
sudo apt install -y shellcheck
sudo apt install -y tidy
npm install -g eslint eslint-config-airbnb prettier babel-eslint import-js
# install hadolint

# lsp
pip python-language-server
npm install -g dockerfile-language-server-nodejs
npm install -g typescript-language-server
npm install -g bash-language-server

sudo add-apt-repository ppa:lyzardking/ubuntu-make
sudo apt update
sudo apt install ubuntu-make
umake web firefox-dev
sudo apt -y install flameshot


# kde
sudo apt install -y kde-plasma-desktop
sudo apt install -y plasma-widgets-addons
sudo apt install -y plasma-nm
sudo apt install -y latte-dock
