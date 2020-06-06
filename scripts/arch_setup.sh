#!/usr/bin/env bash

set -o pipefail
set -o nounset
set -o errexit
set -o xtrace

SCRIPT_DIRECTORY=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
# shellcheck disable=SC2034
PROJECT_ROOT=$(cd "$(dirname "${SCRIPT_DIRECTORY}")" && pwd -P)

sudo pacman -S xorg-server xorg-xinit xorg-xhost --noconfirm --needed

# Install git and dependency for gitk
sudo pacman -S git tk --noconfirm --needed

# Standard folder
    mkdir -p "$HOME"/Pictures "$HOME"/Documents "$HOME"/Desktop "$HOME"/Downloads

    sudo pacman -Syu

    sudo pacman -S lua --noconfirm --needed
    sudo pacman -S xorg-xmodmap --noconfirm --needed
    yay -S xcape --noconfirm --needed

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
    yay -S screenkey --noconfirm --needed

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
    yay -S dropbox --noconfirm --needed

# Install zsh
    sudo pacman -S zsh --noconfirm --needed
    yay -S stderred-git --noconfirm --needed

# Desktop applications
    # kde asthetics
    sudo pacman -S latte-dock --noconfirm --needed
    sudo pacman -S plasma5-applets-active-window-control --noconfirm --needed
    yay -S sierrabreeze-kwin-decoration-git --noconfirm --needed
    sudo pacman -S kvantum-qt5 --noconfirm --needed
    yay -S mcmojave-kde-theme-git --noconfirm --needed

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
    yay -S polybar --noconfirm --needed
# gtk themes
    # create filepath for configurations
    mkdir -p ~/.config/gtk-1.0
    mkdir -p ~/.config/gtk-2.0

    sudo pacman -S lxappearance --noconfirm --needed

# development
    # install docker
        sudo pacman -S docker docker-compose --noconfirm --needed
        sudo systemctl start docker
        sudo systemctl enable docker
        sudo usermod -aG docker "$USER"

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
        yay -S nvm --noconfirm --needed

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
        yay -S emacs27-git --noconfirm --needed
        git clone --depth 1 https://github.com/hlissner/doom-emacs "$XDG_CONFIG_HOME"/emacs
        "$XDG_CONFIG_HOME"/emacs/bin/doom install 
        ln -s "$XDG_CONFIG_HOME"/emacs/bin/doom ~/.local/bin/doom

        yay -S visual-studio-code-bin --noconfirm --needed

        # Dependency
        yay -S python-epc python-importmagic --noconfirm --needed

        # Spell checker for emacs
        sudo pacman -S aspell aspell-en --noconfirm --needed

        # lsp
        sudo pacman -S python-language-server --noconfirm --needed
        yay -S typescript-language-server-bin dockerfile-language-server-bin bash-language-server --noconfirm --needed


        # linters

            # python
            sudo pacman -S python-black mypy python-pylint python-isort autopep8 --noconfirm --needed
            # auto remove unused imports
            yay -S python-autoflake --noconfirm --needed

            # bash
            yay -S shellcheck-static --noconfirm --needed

            # js
            sudo npm install -g import-js
            sudo npm install -g eslint-config-airbnb
            sudo npm install -g install-peerdeps
            yay -S babel-eslint --noconfirm --needed
            sudo pacman -S eslint prettier --noconfirm --needed
            sudo pacman -S tidy --noconfirm --needed


# maintanence
    sudo pacman -S cronie --noconfirm --needed
    sudo systemctl enable cronie
    yay -S timeshift --noconfirm --needed
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

    # p10k
        
        # TODO has to be ran manually
        # cd ~/Downloads
        # yay --getpkgbuild nerd-fonts-complete
        # cd nerd-fonts-complete
        # wget -O nerd-fonts-2.1.0.tar.gz https://github.com/ryanoasis/nerd-fonts/archive/v2.1.0.tar.gz
        # makepkg -sci BUILDDIR=.

    # polybar
        yay -S ttf-material-design-icons --noconfirm --needed
        yay -S ttf-font-awesome-4 --noconfirm --needed

    # emacs fallback unicode glyph fonts
        yay -S ttf-symbola-infinality --noconfirm --needed

    # Fira Code
        wget --directory-prefix ~/.local/share/fonts https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/FiraCode/Regular/complete/Fira%20Code%20Regular%20Nerd%20Font%20Complete.ttf

# keyrings
    sudo pacman -S gnome-keyring seahorse --noconfirm --needed
    sudo pacman -S keychain --noconfirm --needed
    yay -S bitwarden-bin bitwarden-cli --noconfirm --needed

# mail client
    yay -S mu --noconfirm --needed
    # imap
    sudo pacman -S isync --noconfirm --needed
    mkdir -p ~/.local/share/mail/school
    sudo pacman -S thunderbird --noconfirm --needed

# bloat
    sudo pacman -S neofetch --noconfirm --needed
    yay -S python-grip --noconfirm --needed

# photoshop
    sudo pacman -S gimp --noconfirm --needed
# cheat
    yay -S cheat-git --noconfirm --needed
    git clone https://github.com/cheat/cheatsheets.git ~/.local/share/cheat/community

# fixes keychron keyboard
    echo "options hid_apple fnmode=0" | sudo tee /etc/modprobe.d/hid_apple.conf

# video client
    sudo pacman -S mpv --noconfirm --needed

# softwares
    yay -S zoom --noconfirm --needed
    yay -S team --noconfirm --needed

# binaries
    yay -S pciutils --noconfirm --needed # lspci

reboot
