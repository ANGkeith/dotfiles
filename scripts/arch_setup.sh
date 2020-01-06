#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail

script_root="$(cd $(dirname $BASH_SOURCE[0]) && pwd -P)"
project_root=$(cd $(dirname ${script_root}) && pwd -P)

# Standard folders
    mkdir -p $HOME/Pictures $HOME/Documents $HOME/Desktop

source $XDG_CONFIG_HOME/zsh/exports.zsh

# utils
    # file compression
    sudo pacman -S zip unzip --noconfirm
    # bluetooth
    sudo pacman -S bluez-utils --noconfirm
    sudo systemctl enable bluetooth.service

    sudo pacman -S xclip --noconfirm

    sudo pacman -S ctags --noconfirm

    sudo pacman -S network-manager-applet --noconfirm

    sudo pacman -S light --noconfirm

# networking
    sudo pacman -S netcat --noconfirm

# terminal and multiplexer
    sudo pacman -S tmux konsole --noconfirm

# Install yay
    cd /tmp
    git clone https://aur.archlinux.org/yay.git
    cd yay
    makepkg -si

# Searching tool
    sudo pacman -S ripgrep --noconfirm
    sudo pacman -S the_silver_searcher --noconfirm

# Text editor
    # uncomment out zsh setup script
    sed -i 's/# source $ZDOTDIR\/setup.zsh/source $ZDOTDIR\/setup.zsh/g' $XDG_CONFIG_HOME/zsh/.zshrc

    # vim plugin manager
    curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    # neovim
    sudo pacman -S neovim python-pynvim --noconfirm
    # used for previewing
    sudo pacman -S bat --noconfirm



# Install git and dependency for gitk
    sudo pacman -S git tk --noconfirm

# Install zsh
    sudo pacman -S zsh --noconfirm
    # Font for theme
        yay -S nerd-fonts-complete

# Backup DE
    # pacman -S sddm plasma --noconfirm

# i3
    pacman -S i3 --noconfirm
    # Install fonts
        sudo pacman -S powerline-fonts --noconfirm
    # Install dependency for `i3_cycle_windows` script
        sudo pacman -S perl-json --noconfirm
    # Volume manager
        sudo pacman -S pavucontrol --noconfirm
    # Show cpu temperatures
        sudo pacman -S lm-sensors --noconfirm
    # Dependencies for screenshot
        sudo pacman -S flameshot --noconfirm
    # Replacement for dmenu
        # sudo pacman -S rofi --nocofirm
    # Screen locker
        sudo pacman -S feh xautolock --noconfirm
    # Status_bar
        yay -S i3status-rust
        # i3status-rust required ttf-font-awesome-4
        yay -S ttf-font-awesome-4
    # Screenlocker
        yay -S betterlockscreen
        # Generate cache for betterlockscreen
        PATH_TO_WALLPAPER="$XDG_CONFIG_HOME"/wallpaper/wallpaper.jpg
        if [ -e ${PATH_TO_WALLPAPER} ]; then
            betterlockscreen -u ${PATH_TO_WALLPAPER}
        else
            echo "The path `${PATH_TO_WALLPAPER}` does not exist. "
        fi
    # Internet Browser
        sudo pacman -S chromium --noconfirm
        # install random chinese fonts lol
        sudo pacman -S adobe-source-han-sans-cn-fonts --noconfirm
        sudo pacman -S adobe-source-han-sans-tw-fonts --noconfirm
        sudo pacman -S adobe-source-han-serif-cn-fonts --noconfirm
        sudo pacman -S adobe-source-han-serif-tw-fonts --noconfirm
        sudo pacman -S adobe-source-han-sans-otc-fonts --noconfirm
    # Display Compositor
        sudo pacman -S picom --noconfirm

# bspwm
    sudo pacman -S bspwm sxhkd --noconfirm
     

# development
    # install docker
        sudo pacman -S docker docker-compose --noconfirm
        sudo systemctl start docker
        sudo systemctl enable docker
        sudo usermod -aG docker $USER
        newgrp docker
        # to allow pycharm integration with docker
            # sudo mkdir /etc/systemd/system/docker.service.d/
            # sudo touch /etc/systemd/system/docker.service.d/startup_options.conf
            # echo "# /etc/systemd/system/docker.service.d/override.conf
            # [Service]
            # ExecStart=/usr/bin/dockerd -H fd:// -H tcp://0.0.0.0:2375" |
            #     sudo tee /etc/systemd/system/docker.service.d/startup_options.conf
            # sudo systemctl daemon-reload
            # sudo systemctl restart docker.service

    # python
        sudo pacman -S pyenv --noconfirm
        sudo pacman -S python-pip --noconfirm
        sudo pacman -S python-pipenv --noconfirm
        sudo pacman -S python-virtualenv --noconfirm
        sudo pacman -S python-virtualenvwrapper --noconfirm

        # linters
        sudo pacman -S python-black mypy python-pylint python-isort autopep8 --noconfirm

        # for vim-coc integration
        sudo pacman -S python-pynvim python-jedi --noconfirm


    # nodejs
        yay -S nvm

# maintanence
    yay -S timeshift

    # depenency for nvim coc plugin
    source $ZDOTDIR/.zshrc
    nvm install 10.18.0
    nvm alias default 10.18.0

# bloat
    sudo pacman -S neofetch --noconfirm
    sudo pacman -S conky-lua-nvidia --noconfirm
