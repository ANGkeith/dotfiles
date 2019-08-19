#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail

./scripts/essential_install.sh
./scripts/postman_install.sh
./scripts/web_browser_install.sh
./scripts/i3_install.sh

