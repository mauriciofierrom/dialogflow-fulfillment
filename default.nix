{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc883" }:

let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskell.packages.${compiler}.callPackage ./dialogflow-fulfillment.nix { }
