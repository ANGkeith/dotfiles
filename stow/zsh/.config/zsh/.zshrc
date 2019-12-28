source $ZDOTDIR/exports.zsh
[[ -f $ZDOTDIR/themes/p10k.zsh ]] && source $ZDOTDIR/themes/p10k.zsh

source $ZDOTDIR/setup.zsh

source ~/.zplug/init.zsh

zplug load

# Automate installation of plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

source $ZDOTDIR/aliases.zsh
source $ZDOTDIR/key_bindings.zsh
source $ZDOTDIR/lazy_load.zsh
