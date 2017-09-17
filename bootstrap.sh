#!/usr/bin/env sh

set -e

DOTFILES_ROOT=$(pwd -P)

# Utility functions

# This is copied from holman/dotfiles with some modifications
install_dotfiles () {
  echo 'installing dotfiles'

  for src in $(find -H "$DOTFILES_ROOT" -maxdepth 2 -name '*.symlink' -not -path '*.git*')
  do
    dst="$HOME/.$(basename "${src%.*}")"
    ln -s "$src" "$dst"
  done
}

# Setup some initial applications

sudo apt update
sudo apt upgrade -y

sudo apt install rofi

# Symlink to home
install_dotfiles

echo "Bootstrap complete"
