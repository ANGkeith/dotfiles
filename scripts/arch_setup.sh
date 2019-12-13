#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail

script_root="$(cd $(dirname $BASH_SOURCE[0]) && pwd -P)"
project_root=$(cd $(dirname ${script_root}) && pwd -P)

# Standard folders
    mkdir -p $HOME/Pictures $HOME/Documents $HOME/Desktop

# Install yay
    cd /tmp
    git clone https://aur.archlinux.org/yay.git
    cd yay
    makepkg -si

# Text editor
    sudo pacman -S vim the_silver_searcher xclip --noconfirm
    sudo pacman -S vim .--noconfirm
    curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Install git and dependency for gitk
    sudo pacman -S git tk --noconfirm

# Install zsh
    sudo pacman -S zsh --noconfirm
    # Ohmyzsh
        sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
    # Themes
        git clone https://github.com/romkatv/powerlevel10k.git $ZSH_CUSTOM/themes/powerlevel10k
    # Font for theme
        yay -S nerd-fonts-complete
    # Plugins zsh-syntax-highlighting
        git clone https://github.com/zsh-users/zsh-syntax-highlighting.git \
            ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
    # Plugins zsh-autosuggestions
        git clone https://github.com/zsh-users/zsh-autosuggestions \
            ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

# Backup DE
    # pacman -S sddm plasma --noconfirm

# i3
    pacman -S i3 --noconfirm
    # Install fonts
        sudo pacman -S powerline-fonts --noconfirm
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
        PATH_TO_WALLPAPER="$HOME/.config/wallpaper.jpg"
        if [ -e ${PATH_TO_WALLPAPER} ]; then
            betterlockscreen -u ${PATH_TO_WALLPAPER}
        else
            echo "The path `${PATH_TO_WALLPAPER}` does not exist. "
        fi
    # Internet Browser
        sudo pacman -S chromium --noconfirm
    # Display Compositor
        sudo pacman -S compton --noconfirm

# install docker
    sudo pacman -S docker docker-compose --noconfirm
    sudo systemctl start docker
    sudo systemctl enable docker
    sudo usermod -aG docker $USER
    newgrp docker
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

