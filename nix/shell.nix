{ repoRoot, inputs, pkgs, lib, system }:

cabalProject:

let
  cardano-node = inputs.cardano-node.packages.cardano-node;
  cardano-cli = inputs.cardano-cli.legacyPackages.cardano-cli;
in
{
  name = "stablecoin-plutus";
  packages = [
    cardano-cli
    cardano-node
  ];

  env = {
    CARDANO_NODE = "${cardano-node}/bin/cardano-node";
    CARDANO_CLI = "${cardano-cli}/bin/cardano-cli";
  };

  preCommit = {
    # NOTE: when this attribute set changes, `.pre-commit-config.yaml` (which is a sym link to the nix store) changes.
    #       To maintain a the same hooks for both nix and non-nix environment you should update the `.pre-commit-config.yaml.nonix`
    #       (`cp .pre-commit-config.yaml .pre-commit-config.yaml.nonix`).
    #       This step is necessary because `.pre-commit-config.yaml` is ignored by git.
    # TODO To enable in a future PR
    # cabal-fmt.enable = true;
    stylish-haskell.enable = true;
    # nixpkgs-fmt.enable = true;
  };
}
