#!/bin/bash
set -eou pipefail

script_root="$(cd $(dirname $BASH_SOURCE[0]) && pwd -P)"
project_root=$(cd $(dirname ${script_root}) && pwd -P)

# install developer tools
# xcode-select --install

# install package manager
# /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

defaults write -g ApplePressAndHoldEnabled -bool false
defaults write com.apple.Dock autohide-delay -float 0.0001; killall Dock

brew install \
    fluxcd/tap/flux \
    fzf \
    `# for gitk` \
    git-gui \
    go \
    helm \
    jq \
    k3d \
    kubectl \
    nvim \
    nvm \
    ripgrep \
    stow \
    tmux \
    wget

brew install --cask \
    `# password manager` \
    bitwarden \
    docker \
    `# screenshot tool` \
    flameshot \
    font-source-code-pro \
    google-chrome \
    iterm2 \
    `#key remapping tool` \
    karabiner-elements \
    `# snapping window shorcuts` \
    rectangle \
    slack \
    telegram \
    virtualbox \
    visual-studio-code\
    whatsapp \
    zoom

# Install patched terminal font
for font in "SourceCodePro/Black/complete/Sauce Code Pro Black Nerd Font Complete.ttf" \
    "SourceCodePro/Black-Italic/complete/Sauce Code Pro Black Italic Nerd Font Complete.ttf" \
    "SourceCodePro/Medium-Italic/complete/Sauce Code Pro Medium Italic Nerd Font Complete.ttf" \
    "SourceCodePro/Medium/complete/Sauce Code Pro Medium Nerd Font Complete.ttf" \
    "SourceCodePro/Semibold-Italic/complete/Sauce Code Pro Semibold Italic Nerd Font Complete.ttf" \
    "SourceCodePro/Semibold/complete/Sauce Code Pro Semibold Nerd Font Complete.ttf" \
    "SourceCodePro/Bold/complete/Sauce Code Pro Bold Nerd Font Complete.ttf" \
    "SourceCodePro/Bold-Italic/complete/Sauce Code Pro Bold Italic Nerd Font Complete.ttf" \
    "SourceCodePro/Italic/complete/Sauce Code Pro Italic Nerd Font Complete.ttf" \
    "SourceCodePro/Regular/complete/Sauce Code Pro Nerd Font Complete.ttf"; do
    wget --directory-prefix ~/Library/Fonts "https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/${font}";
done;
