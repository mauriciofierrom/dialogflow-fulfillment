let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.callPackage ./dialog-flow.nix { }
