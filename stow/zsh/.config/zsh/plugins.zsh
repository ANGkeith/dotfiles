source $HOME/.local/lib/zinit/zinit.zsh
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# easy way for ls colors
zinit snippet OMZ::lib/theme-and-appearance.zsh

# Themes
zinit ice depth=1 atload'!source $ZDOTDIR/themes/p10k.zsh' lucid nocd
zinit load romkatv/powerlevel10k

# zinit extensions
zinit load zinit-zsh/z-a-bin-gem-node

# Turbo MODE {{{

    # Used by OMZ vi-mode
    zinit wait lucid for OMZ::lib/clipboard.zsh

    # TODO: waiting for PR to be merge. https://github.com/ohmyzsh/ohmyzsh/pull/8004
    zinit ice wait lucid
    zinit snippet 'https://github.com/erydo/oh-my-zsh/blob/vi-mode/plugins/vi-mode/vi-mode.plugin.zsh'

    # yank to clipboard in vim mode
    zinit ice wait lucid
    zinit snippet "https://github.com/kutsan/zsh-system-clipboard/blob/master/zsh-system-clipboard.zsh"

    # add widgets
    zinit wait lucid
    zinit snippet "https://github.com/junegunn/fzf/blob/master/shell/key-bindings.zsh"

    # expand alias
    zinit wait lucid for OMZ::plugins/globalias/globalias.plugin.zsh

    # install additional completions
    zinit ice wait lucid blockf atpull'zinit creinstall -q .'
    zinit load zsh-users/zsh-completions

    zinit ice wait as"completion" lucid
    zinit snippet OMZ::plugins/docker/_docker

    zinit wait lucid for atload"_zsh_autosuggest_start" zsh-users/zsh-autosuggestions

    # zinit ice wait lucid
    # zinit snippet OMZ::plugins/z/z.sh
    if [[ -z $(which lua) ]]; then
        echo "lua executable missing"
    else
        zinit ice as"null" atload'eval "$(lua z.lua --init zsh enhanced once fzf)"'
        zinit load https://github.com/skywind3000/z.lua
    fi

    zinit wait lucid for hlissner/zsh-autopair

    zinit ice wait lucid atinit"zpcompinit; zpcdreplay"
    zinit load zdharma/fast-syntax-highlighting

    # Keep this below, so that this can override plugins default
    zinit ice wait lucid atload" \
        source $ZDOTDIR/key_bindings.zsh; \
        source $ZDOTDIR/aliases.zsh; \
        source $ZDOTDIR/options.zsh;"
    zinit load zdharma/null
# }}}

# Command {{{
    # prettier git diff, use `git dsf` to trigger
    zinit ice as"null" wait"2" lucid sbin"bin/git-dsf;bin/diff-so-fancy"
    zinit load zdharma/zsh-diff-so-fancy

    # easier to use `find`
    zinit ice cloneonly wait"2" lucid from"gh-r" mv"fd* -> fd" sbin"fd/fd"
    zinit load sharkdp/fd

    zinit ice cloneonly wait"2" lucid from"gh-r" mv"cheat* -> cheat" sbin"cheat"
    zinit load cheat/cheat

    # for raspberry, fzf need bpick *arm8*
    # zinit ice cloneonly bpick"*arm8*" from"gh-r" sbin"g:fzf -> fzf"

    zinit ice cloneonly from"gh-r" sbin"g:fzf -> fzf"
    zinit load junegunn/fzf-bin

    # shows aliases of a command
    zinit wait"2" lucid for \
        from'gh-r' sbin"def-matcher" sei40kr/fast-alias-tips-bin \
                                     sei40kr/zsh-fast-alias-tips
# }}}

# Loaded on-demand {{{
    # Least frequently used
    zinit trigger-load'!extract' for OMZ::plugins/extract/extract.plugin.zsh
# }}}

# vim: foldmethod=marker
