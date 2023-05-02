{
  description = "Jambhala: A Plutus Development Suite";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, CHaP }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            jambhala =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc8107";
                # This is used by `nix develop .` to open a shell for use with
                # `cabal`, `hlint` and `haskell-language-server`
                shell.tools = {
                  cabal = { };
                  hlint = "3.4.1";
                  haskell-language-server = "1.8.0.0";
                };
                # Non-Haskell applications required by Jambhala
                shell.buildInputs = [
                  pkgs.bashInteractive
                  pkgs.gnugrep
                  pkgs.neovim
                  pkgs.nixpkgs-fmt
                  pkgs.nix-prefetch-git
                  pkgs.nodejs
                  pkgs.nodePackages.pnpm
                  codiumWithExtensions
                ];
                inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        codiumWithExtensions = (pkgs.vscode-with-extensions.override {
          vscode = pkgs.vscodium;
          vscodeExtensions = with pkgs.vscode-extensions; [
            asvetliakov.vscode-neovim
            dracula-theme.theme-dracula
            haskell.haskell
            jnoortheen.nix-ide
            justusadam.language-haskell
            mkhl.direnv
          ];
        }
        );
        flake = pkgs.jambhala.flake { };
      in
      flake // {
        # Built by `nix build .`
        packages.default = flake.packages."jambhala:exe:jamb";
      });

  # nixConfig = {
  #   bash-prompt = "\\[\\e[0;92m\\][\\[\\e[0;92m\\]Jambhala:\\[\\e[0;92m\\]\\w\\[\\e[0;92m\\]]\\[\\e[0;92m\\]$ \\[\\e[0m\\]";
  #   extra-substituters = [
  #     "https://cache.iog.io"
  #     "https://cache.zw3rk.com"
  #   ];
  #   extra-trusted-public-keys = [
  #     "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
  #     "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
  #   ];
  #   allow-import-from-derivation = true;
  #   accept-flake-config = true;
  # };
}
