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
