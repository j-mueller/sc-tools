{ repoRoot, inputs, pkgs, lib, system }:

let
  sha256map = {
  };

  modules = [
    ({ config, ... }: {
      packages = {
        # TODO
        # convex-base.ghcOptions = [ "-Werror" ];
        # convex-coin-selection.ghcOptions = [ "-Werror" ];
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
