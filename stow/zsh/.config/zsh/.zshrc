source $ZDOTDIR/exports.zsh
source $ZDOTDIR/setup.zsh
source $ZDOTDIR/plugins.zsh
source $ZDOTDIR/lazy_load.zsh

# Check startup time
# for i in $(seq 1 10); do time /bin/zsh -i -c exit; done;
# TODO launch emacs using systemd instead
```
if [[ -z $(pgrep emacs) ]]; then
    . "/usr/share/nvm/nvm.sh" &> /dev/null
    emacs --daemon &> /dev/null
fi
```
