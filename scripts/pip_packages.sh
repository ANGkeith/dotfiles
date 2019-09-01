#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail
set +o nounset

python -m pip install grip
python -m pip install termcolor
python -m pip install ranger-fm
