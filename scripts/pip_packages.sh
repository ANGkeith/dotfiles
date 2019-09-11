#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail
set +o nounset

python -m pip install grip
python -m pip install termcolor

# ranger dependency
sudo apt install -y w3m-img
python -m pip install ranger-fm

# jupyter
mkvirtualenv jupyter
pip install jupyterlab
pip install jupyter-nbextensions-configurator
mkdir -p $(jupyter --data-dir)/nbextensions/
git clone https://github.com/lambdalisue/jupyter-vim-binding $(jupyter --data-dir)/nbextensions/vim_binding
deactivate

