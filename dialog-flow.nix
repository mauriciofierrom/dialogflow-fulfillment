{ mkDerivation, aeson, base, containers, hspec, hspec-discover, stdenv
, text, unordered-containers
}:
mkDerivation {
  pname = "dialog-flow";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base containers text unordered-containers ];
  testHaskellDepends = [ aeson base containers hspec hspec-discover ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/mauriciofierrom/dialog-flow";
  description = "A Dialog Flow library for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
