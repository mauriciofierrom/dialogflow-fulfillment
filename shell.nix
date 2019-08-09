{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-pretty, base, bytestring
      , containers, directory, hspec, hspec-discover, stdenv, text
      , unordered-containers
      }:
      mkDerivation {
        pname = "dialogflow";
        version = "0.0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base bytestring containers text unordered-containers
        ];
        testHaskellDepends = [
          aeson aeson-pretty base bytestring containers directory hspec
          hspec-discover
        ];
        testToolDepends = [ hspec-discover ];
        homepage = "https://github.com/mauriciofierrom/dialogflow";
        description = "A Dialogflow library for Haskell";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
