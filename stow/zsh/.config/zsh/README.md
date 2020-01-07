# ZSH

## File Structure

The following shows the order in which the files will be sourced:

* `.zshenv` - so far this is used to set the `ZDOTDIR` env variable, which is
  points to the directory that zsh looks for startup files (ie. `.zshrc`) from.
* `.zshrc` - main file to load the rest, exports system variables, extends path
* `exports.zsh` - contains all the environment variables
* `plugins.zsh` - contains all the list of plugins managed by `zplugin`
* `themes` - contains the themes
* `lazy_load.zsh` - contains non-essential slow functions that can be loaded on
  demand

## zplugins turbo mode

The following files will be ran after all the plugin has been loaded in the
'turbo' mode so that it can override plugins configurations

* `aliases.zsh` - contains command abbrievation. (This is sourced after the
plugins so aliases will not be override by the plugin)
* `key_bindings.zsh` - contains all the list of key bindings
demand. This is to reduce start time of zsh
* `options.zsh` - overriding some default/plugin options

