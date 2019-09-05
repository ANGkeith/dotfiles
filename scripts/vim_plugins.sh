#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail

# containes the dependency requried by vim plugins

# vim-tagbar
sudo apt install -y exuberant-ctags
