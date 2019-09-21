let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.callPackage ./dialogflow-fulfillment.nix { }
