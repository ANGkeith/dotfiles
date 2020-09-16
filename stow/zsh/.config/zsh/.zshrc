source "$ZDOTDIR"/exports.zsh
source "$ZDOTDIR"/setup.zsh
source "$ZDOTDIR"/plugins.zsh

source "$ZDOTDIR"/locked_plugins/oh-my-zsh/plugins/vi-mode/vi-mode.plugin.zsh
source "$ZDOTDIR"/locked_plugins/zsh-system-clipboard/zsh-system-clipboard.zsh

source "$ZDOTDIR"/themes/p10k.zsh
source "$ZDOTDIR"/my_widget.zsh
source "$ZDOTDIR"/key_bindings.zsh
source "$ZDOTDIR"/aliases.zsh;
source "$ZDOTDIR"/options.zsh;
source "$ZDOTDIR"/lazy_load.zsh

# Check startup time
# for i in $(seq 1 10); do time /bin/zsh -i -c exit; done;
# enable-fzf-tab
complete -C $(which terraform) terraform

