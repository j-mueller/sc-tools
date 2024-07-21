{
  description = "sc-tools";
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
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
                compiler-nix-name = "ghc966";
                src = lib.cleanSource ./.;
                shell = {
                  withHoogle = true;
                  buildInputs = with pkgs; [
                    fd
                  ];
                  tools = {
                    cabal = "latest";
                    haskell-language-server = "latest";
                    ghcide = "latest";
                    ghcid = "latest";
                    stylish-haskell = "latest";
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
          packages = flake.packages;
          checks = {
            build-all = pkgs.runCommandCC "check all cabal.project pagackes"
              {
                buildInputs = builtins.attrValues flake.packages;
              }
              "mkdir $out";
          };
        };
    };
  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}


