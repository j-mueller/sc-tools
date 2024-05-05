{
  description = "sc-tools";
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    iohk-nix.url = "github:input-output-hk/iohk-nix";
  };
  outputs = inputs@{ nixpkgs, haskellNix, flake-parts, CHaP, iohk-nix, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      perSystem = { self', system, lib, config, pkgs, ... }:
        let
          overlays = [
            haskellNix.overlay
            iohk-nix.overlays.crypto
            iohk-nix.overlays.haskell-nix-crypto
            (final: prev: {
              sc-tools = final.haskell-nix.cabalProject' {
                compiler-nix-name = "ghc8107";
                src = lib.cleanSource ./.;
                shell = {
                  withHoogle = true;
                  tools = {
                    cabal = "latest";
                  };
                };
                inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
                sha256map = {
                  "https://github.com/j-mueller/annealing"."ea8e9ec8cf63796205372cedb265836cb8e526b2" = "sha256-9qC0s3lA7ndq4k4opY1niQ5XtmnovPqkUoFVtfI+9Ig=";
                  "https://github.com/j-mueller/csv"."b2f00d7819ff62cb80532adfbc28dbc1046d3d96" = "sha256-yFyu+eEZQjYRstMK0pyi+8Iaeg+QsXMYrwniTo/VS5E=";
                };
              };
            })
          ];
          pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
          flake = pkgs.sc-tools.flake { };
        in
        {
          devShells = flake.devShells;
        };
    };
}


