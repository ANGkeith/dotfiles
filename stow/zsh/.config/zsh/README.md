# ZSH

## File Structure

The following shows the order in which the files will be sourced:

* `.zshenv` - so far this is used to set the `ZDOTDIR` env variable, which is
  points to the directory that zsh looks for startup files (ie. `.zshrc`) from.
* `.zshrc` - main file to load the rest, exports system variables, extends path
* `exports.zsh` - contains all the environment variables
* `themes/p10k.zsh` - contains the themes
* `setup.zsh` - used to automatically install the dependencies. It will only be
  ran once
* `plugins.zsh` - contains all the list of plugins used by `zplug`
* `aliases.zsh` - contains command abbrievation. (This is sourced after the
  plugins so aliases will not be override by the plugin)
* `key_bindings.zsh` - contains all the list of key bindings
* `lazy_load.zsh` - contains non-essential slow functions that can be loaded on
  demand. This is to reduce start time of zsh

