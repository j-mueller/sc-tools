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
                    ghcid = "latest";
                  };
                };
                inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
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


