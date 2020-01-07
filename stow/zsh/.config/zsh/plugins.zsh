source $HOME/.local/lib/zplugin/zplugin.zsh
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin

# easy way for ls colors
zplugin snippet OMZ::lib/theme-and-appearance.zsh

# Themes
zplugin ice depth=1 atload'!source $ZDOTDIR/themes/p10k.zsh' lucid nocd
zplugin load romkatv/powerlevel10k

# zplugin extensions
zplugin load zplugin/z-a-bin-gem-node

# Turbo MODE {{{

    # Used by OMZ vi-mode
    zplugin wait lucid for OMZ::lib/clipboard.zsh

    # TODO: waiting for PR to be merge. https://github.com/ohmyzsh/ohmyzsh/pull/8004
    zplugin ice wait lucid
    zplugin snippet 'https://github.com/erydo/oh-my-zsh/blob/vi-mode/plugins/vi-mode/vi-mode.plugin.zsh'

    # yank to clipboard in vim mode
    zplugin ice wait lucid
    zplugin snippet "https://github.com/kutsan/zsh-system-clipboard/blob/master/zsh-system-clipboard.zsh"

    # add widgets
    zplugin wait lucid for OMZ::plugins/fzf/fzf.plugin.zsh

    # expand alias
    zplugin wait lucid for OMZ::plugins/globalias/globalias.plugin.zsh

    # install additional completions
    zplugin ice wait lucid blockf atpull'zplugin creinstall -q .'
    zplugin load zsh-users/zsh-completions

    zplugin ice wait as"completion" lucid
    zplugin snippet OMZ::plugins/docker/_docker

    zplugin wait lucid for atload"_zsh_autosuggest_start" zsh-users/zsh-autosuggestions

    # zplugin ice wait lucid
    # zplugin snippet OMZ::plugins/z/z.sh
    zplugin ice as"null" atload'eval "$(lua z.lua --init zsh enhanced once fzf)"'
    zplugin load https://github.com/skywind3000/z.lua

    zplugin wait lucid for hlissner/zsh-autopair

    zplugin ice wait lucid atinit"zpcompinit; zpcdreplay"
    zplugin load zdharma/fast-syntax-highlighting

    # Keep this below, so that this can override plugins default
    zplugin ice wait lucid atload" \
        source $ZDOTDIR/key_bindings.zsh; \
        source $ZDOTDIR/aliases.zsh; \
        source $ZDOTDIR/options.zsh;"
    zplugin load zdharma/null
# }}}

# Command {{{
    # prettier git diff, use `git dsf` to trigger
    zplugin ice as"null" wait"2" lucid sbin"bin/git-dsf;bin/diff-so-fancy"
    zplugin load zdharma/zsh-diff-so-fancy

    # easier to use `find`
    zplugin ice as"null" wait"2" lucid from"gh-r" mv"fd* -> fd" sbin"fd/fd"
    zplugin load sharkdp/fd

    # shows aliases of a command
    zplugin wait"2" lucid for \
        from'gh-r' sbin"def-matcher" sei40kr/fast-alias-tips-bin \
                                     sei40kr/zsh-fast-alias-tips
# }}}

# Loaded on-demand {{{
    # Least frequently used
    zplugin trigger-load'!extract' for OMZ::plugins/extract/extract.plugin.zsh
# }}}

# vim: foldmethod=marker
