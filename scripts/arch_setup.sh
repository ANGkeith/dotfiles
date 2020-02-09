#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail
set -o xtrace


script_root="$(cd $(dirname $BASH_SOURCE[0]) && pwd -P)"
project_root=$(cd $(dirname ${script_root}) && pwd -P)

sudo pacman -S xorg-server xorg-xinit xorg-xhost --noconfirm

# Standard folder
    mkdir -p $HOME/Pictures $HOME/Documents $HOME/Desktop
    sudo pacman -Syu

    sudo pacman -S lua --noconfirm
    sudo pacman -S xorg-xmodmap --noconfirm

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

# networking
    sudo pacman -S netcat --noconfirm
    sudo pacman -S wget --noconfirm
    sudo pacman -S nmap --noconfirm
    sudo pacman -S lsof --noconfirm

    # dig
    sudo pacman -S bind-tools --noconfirm

# terminal and multiplexer
    sudo pacman -S tmux konsole --noconfirm

# Install yay
    cd /tmp
    if [[ ! -d /tmp/yay  ]]; then
        git clone https://aur.archlinux.org/yay.git
        cd yay
        makepkg -si
    fi

# Searching tool
    sudo pacman -S ripgrep --noconfirm
    sudo pacman -S the_silver_searcher --noconfirm

# Text editor
    # vim plugin manager
    # curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    #     https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    # neovim
    sudo pacman -S neovim python-pynvim --noconfirm

    # used for previewing
    sudo pacman -S bat --noconfirm

    sudo pacman -S emacs --noconfirm

# Spell checker for emacs
    sudo pacman -S aspell aspell-en --noconfirm

# File manager
    sudo pacman -S nautilus ranger --noconfirm

# Install git and dependency for gitk
    sudo pacman -S git tk --noconfirm

# Install zsh
    sudo pacman -S zsh --noconfirm
    yay -S stderred-git --noconfirm

# Desktop applications
    sudo pacman -S sddm --noconfirm
    sudo systemctl enable sddm.service

    # sddm themes
    yay -S sddm-theme-sugar-candy --noconfirm

    sudo sed -i 's/\(AccentColor=\)".*"/\1"#5fafaf"/g' /usr/share/sddm/themes/Sugar-Candy/theme.conf
    sudo sed -i 's/\(ForceHideCompletePassword=\)".*"/\1"true"/g' /usr/share/sddm/themes/Sugar-Candy/theme.conf
    sudo sed -i 's/\(Font=\)".*"/\1"Hack Nerd Font"/g' /usr/share/sddm/themes/Sugar-Candy/theme.conf
    sudo sed -i 's/\(FontSize=\)".*"/\1"14"/g' /usr/share/sddm/themes/Sugar-Candy/theme.conf
    sudo sed -i 's/\(HourFormat=\)".*"/\1"\\nHH:mm"/g' /usr/share/sddm/themes/Sugar-Candy/theme.conf
    sudo sed -i 's/\(DateFormat=\)".*"/\1"dddd, d MMMM"/g' /usr/share/sddm/themes/Sugar-Candy/theme.conf
    # Larger welcome text
    sudo sed -i 's/\(config.HeaderText.*\)\*.*/\1* 5 : 0/g' /usr/share/sddm/themes/Sugar-Candy/Components/Clock.qml

    sudo cp ~/.config/wallpaper/wallpaper.jpg /usr/share/sddm/themes/Sugar-Candy/Backgrounds/Mountain.jpg

# use the theme and set path for Xauthority
echo "[Theme]
Current=Sugar-Candy" | sudo tee /etc/sddm.conf

# Volume manager
    sudo pacman -S pavucontrol pulseaudio pulseaudio-alsa --noconfirm
    # required to play sound over bluetooth
    sudo pacman -S pulseaudio-bluetooth --noconfirm


# Show cpu temperatures (lm-sensors)
    sudo pacman -S i2c-tools --noconfirm

# Dependencies for screenshot
    sudo pacman -S flameshot --noconfirm

# For spawning menus
    sudo pacman -S rofi --noconfirm

# Screen locker
    sudo pacman -S feh xautolock --noconfirm
    yay -S betterlockscreen --noconfirm
    # Generate cache for betterlockscreen
    PATH_TO_WALLPAPER=~/.config/wallpaper/wallpaper.jpg
    if [ -e ${PATH_TO_WALLPAPER} ]; then
        betterlockscreen -u ${PATH_TO_WALLPAPER}
    else
        echo "The path `${PATH_TO_WALLPAPER}` does not exist. "
    fi

# Internet Browser
    sudo pacman -S chromium --noconfirm

# Display Compositor
    sudo pacman -S picom --noconfirm

# Window Manager
    sudo pacman -S bspwm sxhkd --noconfirm
    yay -S polybar --noconfirm

# notification daemon
    sudo pacman -S dunst --noconfirm


# development
    # install docker
        sudo pacman -S docker docker-compose --noconfirm
        sudo systemctl start docker
        sudo systemctl enable docker
        sudo usermod -aG docker $USER

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
        sudo pacman -S python-pytest --noconfirm
        sudo pacman -S python-pip --noconfirm
        sudo pacman -S python-pipenv --noconfirm
        sudo pacman -S python-virtualenv --noconfirm
        sudo pacman -S python-virtualenvwrapper --noconfirm

        # linters
        sudo pacman -S python-black mypy python-pylint python-isort autopep8 --noconfirm

        # for vim-coc integration
        sudo pacman -S python-pynvim python-jedi --noconfirm


    # nodejs
        yay -S nvm --noconfirm

    # ??
        sudo pacman -S perl-json --noconfirm

# maintanence
    yay -S timeshift --noconfirm
    sudo systemctl enable --now cronie.service

    NVM_SOURCE=/usr/share/nvm
    [ -s "$NVM_SOURCE/nvm.sh" ] && . "$NVM_SOURCE/nvm.sh"  # Load NVM
    # depenency for nvim coc plugin
    nvm install 10.18.0
    # nvm alias default 10.18.0

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

    # Fira Code
        wget --directory-prefix ~/.local/share/fonts https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/FiraCode/Regular/complete/Fira%20Code%20Regular%20Nerd%20Font%20Complete.ttf


# bloat
    sudo pacman -S neofetch --noconfirm
    sudo pacman -S keychain --noconfirm
    yay -S python-grip --nonconfirm

# photoshop
    sudo pacman -S gimp --noconfirm

# cheat
    git clone https://github.com/cheat/cheatsheets.git ~/.local/share/cheat/community

reboot

