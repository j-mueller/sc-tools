{ repoRoot, inputs, pkgs, lib, system }:

let
  sha256map = {
  };

  modules = [
    ({ config, ... }: {
      packages = {
        # TODO Fails because `cardano-cli`, and `cardano-node` is not on PATH
        # when running tests with Nix. I don't know how to include packages as
        # inputs when running tests (doesn't seem supported by Haskell.nix). The
        # alternative would be to allow env variables in complement to absolute
        # paths, and use preCheck to set the env variables.
        convex-devnet.doCheck = false;

        # Werror everything. This is a pain, see https://github.com/input-output-hk/haskell.nix/issues/519
        convex-base.ghcOptions = [ "-Werror" ];
        convex-coin-selection.ghcOptions = [ "-Werror" ];
        convex-blockfrost.ghcOptions = [ "-Werror" ];
        convex-devnet.ghcOptions = [ "-Werror" ];
        convex-mockchain.ghcOptions = [ "-Werror" ];
        convex-node-client.ghcOptions = [ "-Werror" ];
        convex-optics.ghcOptions = [ "-Werror" ];
        convex-wallet.ghcOptions = [ "-Werror" ];
      };
    })
  ];

  cabalProject = pkgs.haskell-nix.cabalProject' {
    inherit modules sha256map;
    src = ../.;
    name = "sc-tools";
    compiler-nix-name = "ghc966";
    inputMap = { "https://chap.intersectmbo.org/" = inputs.CHaP; };
    shell.withHoogle = false;
    flake.variants.profiled = {
      modules = [{ enableProfiling = true; enableLibraryProfiling = true; profilingDetail = "none"; }];
    };
  };


  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
  };

in

project
