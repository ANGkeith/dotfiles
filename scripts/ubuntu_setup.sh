#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail
set -o xtrace

script_root="$(cd $(dirname $BASH_SOURCE[0]) && pwd -P)"
# shellcheck disable=SC2034
project_root=$(cd $(dirname ${script_root}) && pwd -P)

XDG_CONFIG_HOME="$HOME/.config"
XDG_CACHE_HOME="$HOME/.cache"
XDG_DATA_HOME="$HOME/.local/share"
PYENV_ROOT="$HOME/.local/lib/pyenv"

sudo apt update

sudo apt install -y xcape

sudo apt install -y moreutils # to use the sponge binaries

# sshd
sudo apt install -y openssh-server

sudo apt install -y libssl-dev
sudo apt install -y wget curl net-tools

# install zsh
    sudo apt install -y jq
    sudo apt install -y zsh
    sudo apt install -y fzf

# terminal
sudo apt install -y konsole tmux
[[ -d "$XDG_CONFIG_HOME"/tmux/plugins/tpm ]] || git clone https://github.com/tmux-plugins/tpm "${XDG_CONFIG_HOME}"/tmux/plugins/tpm
sudo update-alternatives --set x-terminal-emulator /usr/bin/konsole

# search
sudo apt install ripgrep silversearcher-ag
sudo apt install -y fd-find
[[ -L /bin/fd ]] || sudo ln -s /bin/fdfind /bin/fd

# text editor
sudo apt install -y keychain
sudo apt install -y aspell aspell-en
sudo apt install -y libsqlite3-dev sqlite3

command -v emacs || $(
        [[ -d /tmp/emacs ]] || git clone -b emacs-27 git://git.sv.gnu.org/emacs.git /tmp/emacs;
        cd /tmp/emacs;
        sudo apt-get build-dep emacs;
        ./autogen.sh
        ./configure --with-mailutils --with-modules --prefix="${HOME}/.local/lib/emacs"
        make
        make install
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
    command -v pyenv || [[ -d $PYENV_ROOT ]] || git clone https://github.com/pyenv/pyenv.git "$PYENV_ROOT"
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

# install virtualbox
    sudo apt install -y virtualbox

# install kubernetes
    command -v kubectl || $(
	    curl -o /tmp/kubectl -L "https://storage.googleapis.com/kubernetes-release/release/$(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)/bin/linux/amd64/kubectl" \
	    && chmod +x /tmp/kubectl
	    sudo mv /tmp/kubectl /usr/local/bin/kubectl
    )
    command -v minikube || $(
    	curl -o /tmp/minikube -L https://storage.googleapis.com/minikube/releases/latest/minikube-linux-amd64 \
    	&& chmod +x /tmp/minikube\
    	sudo install /tmp/minikube /usr/local/bin/
    )

# volume manager
    sudo apt install -y pavucontrol

# Cheat
    # install cheat binary from github page directly instead
    [[ -d "$XDG_DATA_HOME"/cheat/community ]] || git clone https://github.com/cheat/cheatsheets.git "$XDG_DATA_HOME"/cheat/community

# fixes keychron keyboard
    echo "options hid_apple fnmode=0" | sudo tee /etc/modprobe.d/hid_apple.conf

    sudo snap install postman

# emoji font checkout https://christitus.com/emoji/
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
    sudo apt-get install -y fonts-symbola
    sudo apt-get install -y xfonts-unifont ttf-unifont

# zoom
    command -v zoom || $(
        wget -O /tmp/zoom.deb https://zoom.us/client/latest/zoom_amd64.deb
        cd /tmp/
        sudo apt install -y zoom.deb
    )

# nodejs
    [[ -f /usr/share/nvm/nvm.sh ]]  || $(
            git clone https://github.com/nvm-sh/nvm.git /tmp/nvm
            cd /tmp/nvm
            git checkout `git describe --abbrev=0 --tags --match "v[0-9]*" $(git rev-list --tags --max-count=1)`
            chmod +x /tmp/nvm/nvm.sh
            sudo mkdir -p /usr/share/nvm
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

# lsp
pip3 install python-language-server
npm install -g dockerfile-language-server-nodejs
npm install -g typescript-language-server
npm install -g bash-language-server

sudo add-apt-repository ppa:lyzardking/ubuntu-make
sudo apt update
sudo apt -y install flameshot


# kde
sudo apt install -y kde-plasma-desktop
sudo apt install -y plasma-widgets-addons
sudo apt install -y plasma-nm
sudo apt install -y latte-dock

sudo apt install -y imwheel # use to remap mouse scroll speed

function install_terraform() {
    curl -fsSL https://apt.releases.hashicorp.com/gpg | sudo apt-key add -
    sudo apt-add-repository "deb [arch=amd64] https://apt.releases.hashicorp.com $(lsb_release -cs) main"
    sudo apt-get update && sudo apt-get install terraform
}

if ! command -v terraform > /dev/null; then
    install_terraform
fi

function install_aws() {
    curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "/tmp/awscliv2.zip"
    unzip /tmp/awscliv2.zip -d /tmp
    sudo /tmp/aws/install
}

if ! command -v aws > /dev/null; then
    install_aws
fi

function install_gcloud() {
    echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list
    sudo apt-get install apt-transport-https ca-certificates gnupg
    curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key --keyring /usr/share/keyrings/cloud.google.gpg add -
    sudo apt-get update && sudo apt-get install google-cloud-sdk
}

if ! command -v gcloud > /dev/null; then
    install_gcloud
fi

# requires user intervention
[[ -d "$XDG_CONFIG_HOME"/emacs/bin ]] || git clone --depth 1 https://github.com/hlissner/doom-emacs "$XDG_CONFIG_HOME"/emacs
"$XDG_CONFIG_HOME"/emacs/bin/doom install
sudo apt install ubuntu-make
umake web firefox-dev

