#!/usr/bin/env bash

set -e

indent() {
    sed 's/^/    /'
}

if [ "${JAMB_ENV_LOADED}" != "true" ]; then
  echo "Error: Jambhala environment not loaded. Run `direnv allow` and try again."
  exit 1
fi

cat << "EOF"
  ,____          (\=-,
  \    `'-.______/ /
  `-._.-"(       |
          \ '--\ |
            ^^   ^^
     ▄█    ▄████████   ▄▄▄▄███▄▄▄▄   ▀█████████▄     ▄█    █▄       ▄████████  ▄█          ▄████████
    ███   ███    ███ ▄██▀▀▀███▀▀▀██▄   ███    ███   ███    ███     ███    ███ ███         ███    ███
    ███   ███    ███ ███   ███   ███   ███    ███   ███    ███     ███    ███ ███         ███    ███
    ███   ███    ███ ███   ███   ███  ▄███▄▄▄██▀   ▄███▄▄▄▄███▄▄   ███    ███ ███         ███    ███
    ███ ▀███████████ ███   ███   ███ ▀▀███▀▀▀██▄  ▀▀███▀▀▀▀███▀  ▀███████████ ███       ▀███████████
    ███   ███    ███ ███   ███   ███   ███    ██▄   ███    ███     ███    ███ ███         ███    ███
    ███   ███    ███ ███   ███   ███   ███    ███   ███    ███     ███    ███ ███▌    ▄   ███    ███
█▄ ▄███   ███    █▀   ▀█   ███   █▀  ▄█████████▀    ███    █▀      ███    █▀  █████▄▄██   ███    █▀
▀▀▀▀▀▀                                                                        ▀
EOF

echo -e "Welcome to Jambhala Setup Wizard...\n"

just personalize

cabal update
cabal build .

echo ""
read -p "Install cardano-node/cardano-cli? [Y/n] " input
input=${input:-Y}
if [[ $input =~ ^[Yy]$ ]]; then
  install_node=true
else
  install_node=false
fi
if $install_node; then
  just install-cardano
else
  echo -e "\nRun \`j install-cardano\` at any time to install cardano-node/cardano-cli."
  echo -e "If you already installed the node/cli through other means, you can use your existing installation by configuring the environment variables in the \`.env\` file (see README for help).\n" | fold -s -w 80
fi

echo "Jambhala setup complete. Run \`j code\` to start coding!"