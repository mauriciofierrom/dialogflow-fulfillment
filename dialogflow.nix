{ mkDerivation, aeson, aeson-pretty, base, bytestring, containers
, directory, hspec, hspec-discover, stdenv, text
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
}
