{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, hspec, hspec-discover, stdenv
      , text
      }:
      mkDerivation {
        pname = "dialog-flow";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ aeson base text ];
        testHaskellDepends = [ base hspec hspec-discover ];
        testToolDepends = [ hspec-discover ];
        homepage = "https://github.com/mauriciofierrom/dialog-flow";
        description = "A Dialog Flow library for Haskell";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
