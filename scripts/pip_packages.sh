#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail
set +o nounset

export WORKON_HOME=$HOME/.virtualenvs
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
export VIRTUALENVWRAPPER_VIRTUALENV=$HOME/.local/bin/virtualenv
source $HOME/.local/bin/virtualenvwrapper.sh

# Preview Github Md files
mkvirtualenv grip 
workon grip
python -m pip install grip
deactivate
