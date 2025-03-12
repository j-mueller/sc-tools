{
  description = "sc-tools";

  inputs = {
    iogx = {
      url = "github:input-output-hk/iogx";
      inputs.hackage.follows = "hackage";
      inputs.CHaP.follows = "CHaP";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    cardano-node = {
      url = "github:input-output-hk/cardano-node?ref=10.1.4";
    };

    cardano-cli = {
      url = "github:intersectmbo/cardano-cli?ref=cardano-cli-10.1.1.0";
    };
  };


  outputs = inputs: inputs.iogx.lib.mkFlake {
    inherit inputs;
    repoRoot = ./.;
    outputs = import ./nix/outputs.nix;
    systems = [ "x86_64-linux" ];
  };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://sc-tools.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "sc-tools.cachix.org-1:DY2+6v0HuMvoCt7wEqZTPqzZBcNk/Lexb72Vixz6n6I="
    ];
    allow-import-from-derivation = true;
  };
}
