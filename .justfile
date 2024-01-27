# direnv allow
@allow:
  direnv allow

# direnv reload
@reload:
  direnv reload

# codium editor
@code DIRECTORY='.':
  if [ "{{ DIRECTORY }}" = "." ] || [ -d "{{ DIRECTORY }}" ]; then \
    if [ "${VIM_MODE}" = 'true' ]; then \
      codium {{ DIRECTORY }}; \
    else codium {{ DIRECTORY }} --disable-extension asvetliakov.vscode-neovim; \
    fi \
  else echo "Invalid directory: {{ DIRECTORY }}"; \
  fi

# jamb cli
@cli *OPTS:
  cabal run . -- {{ OPTS }} 2>&1 || true

# cabal repl
@repl:
  cabal repl

# print env vars
@vars:
  python3 jsetup-utils/check_env.py jambhala

direnv-bin := `which direnv`
hls-bin := `which haskell-language-server`

# generate .env file from template
@mk-env:
  mk-env.sh

# create direnv symlink
# @link-direnv:
#  if [ -n "{{ direnv-bin }}" ]; then \
#    ln -s -f "{{ direnv-bin }}" .vscode/direnv.link; \
#  else echo "direnv not found!"; \
#  fi

# create HLS symlink
@link-hls:
  if [ -n "{{ hls-bin }}" ]; then \
    ln -s -f "{{ hls-bin }}" .vscode/haskell-language-server.link; \
  else echo "haskell-language-server not found!"; \
  fi

hoogle-port := "8080"

# start hoogle server
@hoogle:
  echo "Starting Hoogle server @ http://localhost:{{ hoogle-port }}..."
  hoogle server --local -p {{ hoogle-port }} >/dev/null

haskell-sources := "$(find src j-cli -type f -name '*.hs')"
cabal-sources := "$(find . -maxdepth 1 -type f -name '*.cabal')"
fourmolu-exts := "-o -XQuasiQuotes -o -XTemplateHaskell -o -XTypeApplications -o -XImportQualifiedPost"

# format flake.nix, *.cabal, and *.hs files
@fmt:
  echo "Formatting..."
  nixpkgs-fmt flake.nix > /dev/null 2>&1
  cabal-fmt -i {{ cabal-sources }} > /dev/null 2>&1
  fourmolu {{ fourmolu-exts }} -m inplace {{ haskell-sources }} > /dev/null 2>&1
  echo "Formatting complete."

# run cardano-ez-installer
@install-cardano:
  python3 cardano-ez-installer/main.py

node-network-id := env_var('CARDANO_NODE_NETWORK_ID')
node-network := if node-network-id == "1" { "preprod" } else if node-network-id == "2" { "preview" } else { "mainnet" }
cardano-path := env_var('CARDANO_PATH')
network-path := cardano-path / node-network
socket-path := cardano-path / "node.socket"
config-path := network-path / "config"
node-port := "1337"

# run cardano-node
@node:
  cardano-node run \
  --topology {{ config-path }}/topology.json \
  --database-path {{ network-path }}/db \
  --socket-path {{ socket-path }} \
  --port {{ node-port }} \
  --config {{ config-path }}/config.json

# launch jambhalucid
@lucid:
  cd jambhalucid && if [ ! -d "node_modules" ]; then \
    pnpm install; \
  fi && pnpm dev

# clean nix store
@gc:
  nix-collect-garbage -d

# update Jambhala
@update *OPTS:
  jupdate.sh {{ OPTS }} jambhala

# personalize project
@personalize:
  personalize.sh jambhala

# show plutus-apps commit history (q to exit)
@pa-history:
  dir=`find dist-newstyle/src -type d -name 'plutus-ap_*' -print -quit`; \
  if [ -n "$dir" ]; then \
    cd "dist-newstyle/src/$(basename "$dir")"; \
    git fetch; \
    git log; 2>/dev/null || true; \
  else \
    echo "Error: plutus-apps directory not found"; \
  fi

# rebuild project
@rebuild:
  rm -rf .direnv
  rm -rf .cabal
  cabal clean
  cabal build

# reset .env files to defaults
@reset-env:
  rm .env
  rm cardano-cli-guru/.env
  direnv allow

# run setup wizard
@setup:
  setup.sh

# run Aiken tests, build, and write validator(s)
@aiken VALIDATOR='' *BUILD_FLAGS='':
  @cd jambhalaiken && \
  aiken check && \
  aiken build {{ BUILD_FLAGS }} && \
  if [ -n "{{ VALIDATOR }}" ]; then \
    python write.py -v {{ VALIDATOR }}; \
  else \
    python write.py; \
  fi