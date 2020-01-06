# https://www.growingwiththeweb.com/2018/01/slow-nvm-init.html
# Defer initialization of nvm until nvm, node or a node-dependent command is
# run. Ensure this block is only run once if .bashrc gets sourced multiple times
# by checking whether __init_nvm is a function.
if [ -s "$NVM_SOURCE/nvm.sh" ] && [ ! "$(type -w __init_nvm)" = "__init_nvm: function" ]; then
  # nvim depends on npm
  declare -a __node_commands=('nvm' 'node' 'npm' 'yarn' 'gulp' 'grunt' 'webpack' 'nvim' )
  function __init_nvm() {
    for i in "${__node_commands[@]}"; do unalias $i; done
    [ -s "$NVM_SOURCE/nvm.sh" ] && . "$NVM_SOURCE/nvm.sh"  # Load NVM
    unset __node_commands
    unset -f __init_nvm
  }
  for i in "${__node_commands[@]}"; do alias $i='__init_nvm && '$i; done
fi


# virtualenv
workon() {
    # Remove this function, subsequent calls will execute 'workon' directly
    unfunction "$0"
    source /usr/bin/virtualenvwrapper.sh
    workon "$@"
}

# pyenv
# enable shims and autocompletion
pyenv() {
    unfunction "$0"
    if command -v pyenv 1>/dev/null 2>&1; then
        eval "$(pyenv init -)"
    fi
}
