#!/usr/bin/env bash

set -o pipefail
set -o nounset
set -o errexit
set -o xtrace

SCRIPT_DIRECTORY=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
# shellcheck disable=SC2034
PROJECT_ROOT=$(cd "$(dirname "${SCRIPT_DIRECTORY}")" && pwd -P)

sudo pacman -S xorg-server xorg-xinit xorg-xhost --noconfirm

# Install git and dependency for gitk
sudo pacman -S git tk --noconfirm

# Standard folder
    mkdir -p "$HOME"/Pictures "$HOME"/Documents "$HOME"/Desktop
    sudo pacman -Syu

    sudo pacman -S lua --noconfirm
    sudo pacman -S xorg-xmodmap --noconfirm
    
# Install yay
    cd /tmp
    if [[ ! -d /tmp/yay  ]]; then
        git clone https://aur.archlinux.org/yay.git
        cd yay
        makepkg -si
    fi

# utils
    # file compression
    sudo pacman -S zip unzip --noconfirm
    # bluetooth
    sudo pacman -S bluez bluez-utils --noconfirm
    sudo systemctl enable bluetooth.service

    sudo pacman -S xclip --noconfirm
    sudo pacman -S ctags --noconfirm

    sudo pacman -S network-manager-applet --noconfirm

    sudo pacman -S light --noconfirm
    # file converter
    sudo pacman -S texlive-most --no-confirm

    # use for managing window nodes
    sudo pacman -S xdo --no-confirm
    sudo pacman -S xdotool --no-confirm

    sudo pacman -S exa --noconfirm # better ls
    sudo pacman -S fd --noconfirm # better find

    # for debugging
    sudo pacman -S peek --noconfirm
    yay -S screenkey --noconfirm

# networking
    sudo pacman -S netcat --noconfirm
    sudo pacman -S wget --noconfirm
    sudo pacman -S nmap --noconfirm
    sudo pacman -S lsof --noconfirm

    # dig
    sudo pacman -S bind-tools --noconfirm

# terminal and multiplexer
    sudo pacman -S tmux konsole --noconfirm

# Searching tool
    sudo pacman -S ripgrep --noconfirm
    sudo pacman -S the_silver_searcher --noconfirm

# File manager
    sudo pacman -S nautilus ranger dolphin --noconfirm
    yay -S nautilus-dropbox dropbox --noconfirm

# Install zsh
    sudo pacman -S zsh --noconfirm
    yay -S stderred-git --noconfirm

# Desktop applications
    sudo pacman -S sddm --noconfirm
    sudo systemctl enable sddm.service

    # kde asthetics
    sudo pacman -S latte-dock --noconfirm
    sudo pacman -S plasma5-applets-active-window-control --noconfirm
    yay -S sierrabreeze-kwin-decoration-git --noconfirm
    sudo pacman -S kvantum-q --noconfirm
    yay -S mcmojave-kde-theme-git --noconfirm

# Volume manager
    sudo pacman -S pavucontrol pulseaudio pulseaudio-alsa --noconfirm
    # required to play sound over bluetooth
    sudo pacman -S pulseaudio-bluetooth --noconfirm

# Show cpu temperatures (lm-sensors)
    sudo pacman -S i2c-tools --noconfirm

# Dependencies for screenshot
    sudo pacman -S flameshot --noconfirm

# Internet Browser
    yay -S brave-bin --noconfirm
    sudo pacman -Ss firefox-developer-edition --noconfirm

# Display Compositor
    sudo pacman -S picom --noconfirm

# Window Manager
    sudo pacman -S plasma --noconfirm
    # used to check the keycode using `xev`
    sudo pacman -S xorg-xev --noconfirm
    yay -S polybar --noconfirm
# gtk themes
    # create filepath for configurations
    mkdir -p ~/.config/gtk-1.0
    mkdir -p ~/.config/gtk-2.0

    sudo pacman -S lxappearance

# notification daemon
    sudo pacman -S dunst --noconfirm

# development
    # install docker
        sudo pacman -S docker docker-compose --noconfirm
        sudo systemctl start docker
        sudo systemctl enable docker
        sudo usermod -aG docker "$USER"

    # python
        sudo pacman -S pyenv --noconfirm
        sudo pacman -S python-pytest --noconfirm
        sudo pacman -S python-pip --noconfirm
        sudo pacman -S python-pipenv --noconfirm
        sudo pacman -S python-virtualenv --noconfirm
        sudo pacman -S python-virtualenvwrapper --noconfirm

        # for vim-coc integration
        sudo pacman -S python-pynvim python-jedi --noconfirm

    # nodejs
        yay -S nvm --noconfirm

        NVM_SOURCE=/usr/share/nvm
        [ -s "$NVM_SOURCE"/nvm.sh ] && . "$NVM_SOURCE"/nvm.sh  # Load NVM
        nvm install "$NODE_DEFAULT_VERSION"
        nvm alias default "$NODE_DEFAULT_VERSION"
        nvm use default

        # To upgrade nvm use the following:
        # nvm install $NODE_DEFAULT_VERSION --reinstall-packages-from=node
        # nvm alias default <version>
        # Remember to update hardcode path in `vimrc` and `.spacemacs`

    # ??
        sudo pacman -S perl-json --noconfirm

    # Text editor
        # neovim
        sudo pacman -S neovim python-pynvim --noconfirm
        npm install -g neovim

        # used for previewing sudo pacman -S bat --noconfirm

        # emacs
        yay -S emacs27--git --noconfirm
        git clone --branch develop https://github.com/syl20bnr/spacemacs ~/.emacs.d

        # Dependency
        yay -S python-epc python-importmagic --noconfirm

        # Spell checker for emacs
        sudo pacman -S aspell aspell-en --noconfirm

        # lsp
        sudo pacman -S python-language-server --noconfirm
        npm install -g dockerfile-language-server-nodejs
        npm install -g bash-language-server


        # linters

            # python
            sudo pacman -S python-black mypy python-pylint python-isort autopep8 --noconfirm
            # auto remove unused imports
            yay -S python-autoflake --noconfirm

            # bash
            yay -S shellcheck-static --noconfirm

            npm install -g eslint import-js


# maintanence
    yay -S timeshift --noconfirm
    sudo systemctl enable --now cronie.service
    yay -S paccache --noconfirm

# fonts
    # to resolve nerd-fonts-complete error
    sudo mkdir -p /usr/share/fonts/TTF
    sudo mkdir -p /usr/share/fonts/OTF

    # zsh tmux themes
        sudo pacman -S powerline-fonts --noconfirm

    # install random chinese fonts for chromium
        sudo pacman -S adobe-source-han-sans-cn-fonts --noconfirm
        sudo pacman -S adobe-source-han-sans-tw-fonts --noconfirm
        sudo pacman -S adobe-source-han-serif-cn-fonts --noconfirm
        sudo pacman -S adobe-source-han-serif-tw-fonts --noconfirm
        sudo pacman -S adobe-source-han-sans-otc-fonts --noconfirm

    # p10k
        yay -S nerd-fonts-complete --noconfirm

    # polybar
        yay -S ttf-material-design-icons --noconfirm
        yay -S ttf-font-awesome-4 --noconfirm

    # emacs fallback unicode glyph fonts
        yay -S ttf-symbola-infinality

    # Fira Code
        wget --directory-prefix ~/.local/share/fonts https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/FiraCode/Regular/complete/Fira%20Code%20Regular%20Nerd%20Font%20Complete.ttf

# keyrings
    sudo pacman -S gnome-keyring seahorse --noconfirm
    sudo pacman -S keychain --noconfirm
    yay -S bitwarden-bin bitwarden-cli --noconfirm

# mail client
    yay -S mu --noconfirm
    # imap
    sudo pacman -S isync --noconfirm
    mkdir -p ~/.local/share/mail/school
    sudo pacman -S thunderbird --noconfirm

# bloat
    sudo pacman -S neofetch --noconfirm
    yay -S python-grip --nonconfirm

# photoshop
    sudo pacman -S gimp --noconfirm

# cheat
    yay -S cheat-git --noconfirm
    git clone https://github.com/cheat/cheatsheets.git ~/.local/share/cheat/community

# fixes keychron keyboard
    echo "options hid_apple fnmode=0" | sudo tee /etc/modprobe.d/hid_apple.conf

# video client
    sudo pacman -S mpv --noconfirm


# conference
    yay -S zoom --noconfirm


reboot
