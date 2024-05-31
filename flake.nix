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
                compiler-nix-name = "ghc964";
                src = lib.cleanSource ./.;
                shell = {
                  withHoogle = true;
                  tools = {
                    cabal = "latest";
                    haskell-language-server = "latest";
                    ghcide = "latest";
                  };
                };
                inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
                sha256map = {
                  "https://github.com/albertodvp/plutarch-plutus"."1b2b5fb684e69e3d12319608278c1f6dea6e12af" = "sha256-xViZbZlq5Sw9rHakS9nwcQikCOfwZClSBL8UD+5eJJ4=";
                  "https://github.com/albertodvp/xsy-liqwid-libs"."26b37079439445b815b4bce86d80cf5dbf971f60" = "sha256-dUaqSBHXJFpzHkG6FBE8RES6drMELbALwcVCdg5S4sI=";
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


