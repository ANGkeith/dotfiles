#!/usr/bin/env bash

set -o pipefail
set -o nounset
set -o errexit
set -o xtrace

SCRIPT_DIRECTORY=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
# shellcheck disable=SC2034
PROJECT_ROOT=$(cd "$(dirname "${SCRIPT_DIRECTORY}")" && pwd -P)

yay-install-maybe() {
   which ${2:-$1} || yay -S $1 --noconfirm --needed
}

sudo pacman -S xorg-server xorg-xinit xorg-xhost --noconfirm --needed

# Install git and dependency for gitk
sudo pacman -S git tk --noconfirm --needed

# Standard folder
    mkdir -p "$HOME"/Pictures "$HOME"/Documents "$HOME"/Desktop "$HOME"/Downloads

    sudo pacman -Syu

    sudo pacman -S lua --noconfirm --needed
    sudo pacman -S xorg-xmodmap --noconfirm --needed
    yay-install-maybe xcape

# Install yay
    command -v yay ||
        $(cd /tmp
          git clone https://aur.archlinux.org/yay.git
          cd yay
          makepkg -si
        )

# utils
    # file compression
    sudo pacman -S zip unzip --noconfirm --needed
    # bluetooth
    sudo pacman -S bluez bluez-utils --noconfirm --needed
    sudo systemctl enable bluetooth.service

    sudo pacman -S xclip --noconfirm --needed
    sudo pacman -S ctags --noconfirm --needed

    sudo pacman -S network-manager-applet --noconfirm --needed

    sudo pacman -S light --noconfirm --needed
    # file converter
    sudo pacman -S texlive-most --noconfirm --needed

    # use for managing window nodes
    sudo pacman -S xdo --noconfirm --needed
    sudo pacman -S xdotool --noconfirm --needed

    sudo pacman -S exa --noconfirm --needed # better ls
    sudo pacman -S fd --noconfirm --needed # better find

    sudo pacman -S jq --noconfirm --needed

    # for debugging
    sudo pacman -S peek --noconfirm --needed
    yay-install-maybe screenkey

# networking
    sudo pacman -S netcat --noconfirm --needed
    sudo pacman -S wget --noconfirm --needed
    sudo pacman -S nmap --noconfirm --needed
    sudo pacman -S lsof --noconfirm --needed

    # dig
    sudo pacman -S bind-tools --noconfirm --needed

# terminal and multiplexer
    sudo pacman -S tmux konsole --noconfirm --needed

# Searching tool
    sudo pacman -S ripgrep --noconfirm --needed
    sudo pacman -S the_silver_searcher --noconfirm --needed

# File manager
    sudo pacman -S nautilus ranger dolphin --noconfirm --needed
    # yay -S nautilus-dropbox
    yay-install-maybe dropbox

# Install zsh
    sudo pacman -S zsh --noconfirm --needed
    yay-install-maybe stderred-git stderred

# Desktop applications
    # kde asthetics
    sudo pacman -S latte-dock --noconfirm --needed
    sudo pacman -S plasma5-applets-active-window-control --noconfirm --needed
    yay-install-maybe sierrabreeze-kwin-decoration-git
    sudo pacman -S kvantum-qt5 --noconfirm --needed
    yay-install-maybe mcmojave-kde-theme-git

# Volume manager
    sudo pacman -S pavucontrol pulseaudio pulseaudio-alsa --noconfirm --needed
    # required to play sound over bluetooth
    sudo pacman -S pulseaudio-bluetooth --noconfirm --needed

# Show cpu temperatures (lm_sensors)
    sudo pacman -S i2c-tools lm_sensors --noconfirm --needed

# Dependencies for screenshot
    sudo pacman -S flameshot --noconfirm --needed

# Internet Browser
    sudo pacman -S firefox-developer-edition --noconfirm --needed

# Display Compositor
    sudo pacman -S picom --noconfirm --needed

# Window Manager
    sudo pacman -S plasma --noconfirm --needed
    # used to check the keycode using `xev`
    sudo pacman -S xorg-xev --noconfirm --needed
# gtk themes
    # create filepath for configurations
    mkdir -p ~/.config/gtk-1.0
    mkdir -p ~/.config/gtk-2.0

    sudo pacman -S lxappearance --noconfirm --needed

# development
    # install docker
        sudo pacman -S docker docker-compose --noconfirm --needed
        # sudo systemctl start docker
        # sudo systemctl enable docker
        # sudo usermod -aG docker "$USER"

    # python
        sudo pacman -S pyenv --noconfirm --needed
        sudo pacman -S python-pytest --noconfirm --needed
        sudo pacman -S python-pip --noconfirm --needed
        sudo pacman -S python-pipenv --noconfirm --needed
        sudo pacman -S python-virtualenv --noconfirm --needed
        sudo pacman -S python-virtualenvwrapper --noconfirm --needed

        # for vim-coc integration
        sudo pacman -S python-pynvim python-jedi --noconfirm --needed

    # nodejs
        yay-install-maybe nvm

        NVM_SOURCE=/usr/share/nvm
        [ -s "$NVM_SOURCE"/nvm.sh ] && . "$NVM_SOURCE"/nvm.sh  # Load NVM
        nvm install "$NODE_DEFAULT_VERSION"
        nvm alias default "$NODE_DEFAULT_VERSION"
        nvm use default

        # To upgrade nvm use the following:
        # nvm install $NODE_DEFAULT_VERSION --reinstall-packages-from=node
        # nvm alias default <version>

    # ??
        sudo pacman -S perl-json --noconfirm --needed

    # Text editor
        # neovim
        sudo pacman -S neovim python-pynvim --noconfirm --needed
        sudo npm install -g neovim

        # used for previewing
        sudo pacman -S bat --noconfirm --needed

        # emacs
        which emacs || $(
                yay -S emacs27-git --noconfirm --needed
                git clone --depth 1 https://github.com/hlissner/doom-emacs "$XDG_CONFIG_HOME"/emacs
                "$XDG_CONFIG_HOME"/emacs/bin/doom install
                ln -s "$XDG_CONFIG_HOME"/emacs/bin/doom ~/.local/bin/doom
            )

        yay-install-maybe visual-studio-code-bin code

        # Dependency
        yay-install-maybe python-epc
        yay-install-maybe python-importmagic

        # Spell checker for emacs
        sudo pacman -S aspell aspell-en --noconfirm --needed

        # lsp
        sudo pacman -S python-language-server --noconfirm --needed
        yay-install-maybe typescript-language-server-bin
        yay-install-maybe dockerfile-language-server-bin
        yay-install-maybe bash-language-server


        # linters

            # python
            sudo pacman -S python-black mypy python-pylint python-isort autopep8 --noconfirm --needed
            # auto remove unused imports
            yay-install-maybe python-autoflake

            # bash
            yay-install-maybe shellcheck-static shellcheck

            # js
            sudo npm install -g import-js
            sudo npm install -g eslint-config-airbnb
            sudo npm install -g install-peerdeps
            yay-install-maybe babel-eslint
            sudo pacman -S eslint prettier --noconfirm --needed
            sudo pacman -S tidy --noconfirm --needed


# maintanence
    sudo pacman -S cronie --noconfirm --needed
    sudo systemctl enable cronie
    yay-install-maybe timeshift
    sudo pacman -S pacman-contrib --noconfirm --needed # to install paccache

# fonts
    # to resolve nerd-fonts-complete error
    sudo mkdir -p /usr/share/fonts/TTF
    sudo mkdir -p /usr/share/fonts/OTF

    # zsh tmux themes
        sudo pacman -S powerline-fonts --noconfirm --needed

    # install random chinese fonts for internet browser
        sudo pacman -S adobe-source-han-sans-cn-fonts --noconfirm --needed
        sudo pacman -S adobe-source-han-sans-tw-fonts --noconfirm --needed
        sudo pacman -S adobe-source-han-serif-cn-fonts --noconfirm --needed
        sudo pacman -S adobe-source-han-serif-tw-fonts --noconfirm --needed
        sudo pacman -S adobe-source-han-sans-otc-fonts --noconfirm --needed

    # emacs fallback unicode glyph fonts
        yay-install-maybe ttf-symbola-infinality

    # Fira Code
        wget --directory-prefix ~/.local/share/fonts https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/FiraCode/Regular/complete/Fira%20Code%20Regular%20Nerd%20Font%20Complete.ttf

# keyrings
    sudo pacman -S gnome-keyring seahorse --noconfirm --needed
    sudo pacman -S keychain --noconfirm --needed
    yay-install-maybe bitwarden-bin
    yay-install-maybe bitwarden-cli

# bloat
    sudo pacman -S neofetch --noconfirm --needed
    yay-install-maybe python-grip

# photoshop
    sudo pacman -S gimp --noconfirm --needed
# cheat
    yay-install-maybe cheat-git
    git clone https://github.com/cheat/cheatsheets.git ~/.local/share/cheat/community

# fixes keychron keyboard
    echo "options hid_apple fnmode=0" | sudo tee /etc/modprobe.d/hid_apple.conf

# video client
    sudo pacman -S mpv --noconfirm --needed

# softwares
    yay-install-maybe zoom
    yay-install-maybe team

# binaries
    yay-install-maybe pciutils

# reboot
