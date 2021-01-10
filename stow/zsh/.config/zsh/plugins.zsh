# load zgen
source "$ZGEN_DIR"/zgen.zsh
# if the init scipt doesn't exist
if ! zgen saved; then
    success_message() {
        printf "\033[32m %s\033[0m\n" "$1"
    }
    echo "Creating a zgen save"

    # asthetics
    zgen load romkatv/powerlevel10k powerlevel10k
    # nicer ls colors
    zgen oh-my-zsh lib/theme-and-appearance.zsh

    # misc
    zgen oh-my-zsh plugins/z
    zgen load junegunn/fzf shell/key-bindings.zsh
    zgen oh-my-zsh plugins/extract
    zgen oh-my-zsh lib/clipboard.zsh
    zgen load zsh-users/zsh-history-substring-search
    zgen load hlissner/zsh-autopair
    zgen load zdharma/zsh-diff-so-fancy
    zgen load zigius/expand-ealias.plugin.zsh

    zgen load johanhaleby/kubetail

    # install def-matcher binary which zsh-fast-alias-tips requires
    if [ ! -f ~/.local/bin/def-matcher ]; then
        echo "Installing def-matcher binary ..."
        install_def_matcher_bin() {
            cd /tmp
            download_link=$(curl -sL https://api.github.com/repos/sei40kr/fast-alias-tips-bin/releases/latest | jq -r '.assets[].browser_download_url' | grep linux)
            wget -qO def-matcher.tar.gz "$download_link"
            tar -xzf def-matcher.tar.gz
            rm def-matcher.tar.gz
            chmod +X def-matcher
            mv def-matcher ~/.local/bin
        }
        $(install_def_matcher_bin)
        success_message "Installed def-matcher binary"
    fi
    zgen load sei40kr/zsh-fast-alias-tips

    zgen load zsh-users/zsh-autosuggestions
    zgen load zsh-users/zsh-syntax-highlighting

    # add widgets
    # completions
    zgen oh-my-zsh plugins/docker
    zgen oh-my-zsh plugins/minikube
    zgen oh-my-zsh plugins/kubectl
    zgen load zsh-users/zsh-completions src

    success_message "save all to init script"

    zgen load softmoth/zsh-vim-mode
    zgen save
fi
