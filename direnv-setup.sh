#!/bin/bash

if ! command -v direnv >/dev/null 2>&1; then
  if ! command -v nix >/dev/null 2>&1; then
    >&2 echo "Error: Nix is not installed"
    exit 1
  fi
  echo "Installing direnv using Nix..."
  nix-env -iA nixpkgs.direnv
fi

if [ ! -f ~/.bashrc ]; then
  touch ~/.bashrc
fi

if ! grep -q "eval \"\$(direnv hook bash)\"" ~/.bashrc; then
  echo "Hooking direnv into bash shell..."
  echo 'eval "$(direnv hook bash)"' >> ~/.bashrc
fi

echo "direnv is installed and hooked into your bash shell"