{ repoRoot, inputs, pkgs, lib, system }:
let
  project = repoRoot.nix.project;
in
[
  (
    project.flake
  )
  {
    devShells.default = project.variants.profiled.devShells.default;
  }
]
