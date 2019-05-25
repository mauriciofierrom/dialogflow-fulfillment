{ mkDerivation, aeson, base, stdenv, text }:
mkDerivation {
  pname = "dialog-flow";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base text ];
  doHaddock = false;
  homepage = "https://github.com/mauriciofierrom/dialog-flow";
  description = "A Dialog Flow library for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
