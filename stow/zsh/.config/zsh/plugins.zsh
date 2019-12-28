#!/usr/bin/env zsh
# Allows zplug to update itself
zplug "zplug/zplug", hook-build:"zplug --self-manage"

# TODO: move .zcompdump to $XDG_DATA_HOME/zsh once the following issue is
# resolved "https://github.com/ohmyzsh/ohmyzsh/issues/7332"
zplug "robbyrussell/oh-my-zsh", use:"lib/{clipboard,completion,directories,history,termsupport,git,grep,key-bindings,theme-and-appearance,prompt_info_functions,compfix,nvm}.zsh"

# Provides aliases for extracting archive files
zplug "plugins/extract", from:oh-my-zsh

zplug "plugins/z", from:oh-my-zsh

#   # provides suggestion as I type based on history
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "plugins/history-substring-search", from:oh-my-zsh, defer:3
# TODO: waiting for PR to be merge. https://github.com/ohmyzsh/ohmyzsh/pull/8004
# zplug "plugins/vi-mode", from:oh-my-zsh
zplug "erydo/oh-my-zsh", use:"plugins/vi-mode/vi-mode.plugin.zsh", at:vi-mode
zplug "plugins/fzf", from:oh-my-zsh

zplug "kutsan/zsh-system-clipboard"
zplug "romkatv/powerlevel10k", as:theme, depth:1

# return 0 # in case zplug adds plugs ignore them
