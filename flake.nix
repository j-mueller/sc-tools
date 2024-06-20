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
                sha256map = {
                  "https://github.com/Plutonomicon/plutarch-plutus"."fcdd2209433d8b8979e820dc4fa9aad5f202216d" = "sha256-gQwaYGIds5owHivXi+ktH7CGeBqoLBykVxyHZZiDUM4=";
                  "https://github.com/input-output-hk/xsy-liqwid-libs"."a799bcca72bcd133cc25a7b6841acb48b3885138" = "sha256-ibAtsyejc3SOUWKoyxSQ5r3jg9eOfyEHkpO+qTrwa2U=";
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


