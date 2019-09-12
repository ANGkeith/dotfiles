#!/bin/bash
set -e

python3 << END
import pip
import subprocess
def import_or_install(packages):
    for package in packages:
        try:
            __import__(package)
        except ImportError:
            subprocess.check_call(["python3", "-m", "pip", "install", package])
import_or_install(["flake8", "mypy", "black", "isort", "autopep8"])
END

# colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NO_COLOR='\033[0m'

# subroutines
yell() {
  echo -e "$RED$0: $*$NO_COLOR" >&2
}

die() {
  yell "$*"
  exit 77
}

try() {
  "$@" || die "Error when running '$*'"
}

success() {
  echo -e "$GREEN${*:-success}$NO_COLOR"
}

warning() {
  echo -e "$YELLOW$*$NO_COLOR"
}

cont() {
  warning "Error occured but we can continue. Please re-check problems manually."
}

# static type check
echo "Running static type check..."
try mypy $1 --disallow-untyped-defs --ignore-missing-imports
success

# formatting
echo "Running formatter with black..."
try black \
    --line-length=80 \
    $1
success

# linting
echo "Running linter with flake8..."
try flake8 \
  $1 \
  --max-line-length=80
success

# isort
echo "Run import sorting with isort..."
import_heading_stdlib='Standard Library'
import_heading_firstparty='My Stuff'
try isort \
  --atomic \
  --recursive \
  --force-grid-wrap=0 \
  --line-width=80 \
  --apply \
  --trailing-comma \
  $1
success

# repeat static type check and linting
echo "Run mypy again..."
try mypy $1 --disallow-untyped-defs --ignore-missing-imports
success

echo "Running linter with flake8 again..."
try flake8 \
  $1 \
  --max-line-length=80
success

echo "Running linter with autopep8"
try autopep8 \
  $1 \
  -i \
  -r \
  --max-line-length=80
success


