{
  description = "Jambhala: A Plutus Development Suite";

  inputs = {
    haskellNix.url  = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    CHaP = {
      url   = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, CHaP }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          jambhala =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = {};
                hlint = "3.4.1";
                haskell-language-server = "1.8.0.0";
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
                nix-prefetch-git
              ];
              inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.jambhala.flake {
      };
    in flake // {
      # Built by `nix build .`
      packages.default = flake.packages."jambhala:exe:jamb";
    });

  # nixConfig = {
  #   extra-trusted-substituters = [
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