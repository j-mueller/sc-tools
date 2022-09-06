{
  description = "cardano-convex/coin-selection";
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emanote = {
      url = "github:srid/emanote";
    };
    cardano-wallet = {
      url = "github:input-output-hk/cardano-wallet/v2022-08-16";
    };
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, iohkNix, emanote, cardano-wallet }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          csProject =
            final.haskell-nix.project' {
              src = ./.;
              index-state = "2022-05-12T00:00:00Z";
              compiler-nix-name = "ghc8107";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = {};
                hlint = {};
                # haskell-language-server = {};
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
                emanote.packages.${system}.default 
                cardano-wallet.packages.${system}.cardano-node
              ];
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              # shell.crossPlatforms = p: [p.ghcjs];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.csProject.flake {
        # This adds support for `nix build .#js-unknown-ghcjs:hello:exe:hello`
        # crossPlatforms = p: [p.ghcjs];
      };
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."convex-coin-selection:exe:convex-coin-selection";
    });
}