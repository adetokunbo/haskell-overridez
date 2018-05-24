{ mkDerivation, aeson, aeson-casing, attoparsec, base, bytestring
, Cabal, exceptions, foldl, managed, neat-interpolation
, optparse-applicative, stdenv, system-fileio, system-filepath
, text, turtle
}:
mkDerivation {
  pname = "haskell-overridez";
  version = "0.10.0";
  src = ../../.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-casing attoparsec base bytestring Cabal exceptions
    foldl managed neat-interpolation optparse-applicative system-fileio
    system-filepath text turtle
  ];
  description = "Manage nix overrides for haskell packages";
  license = stdenv.lib.licenses.bsd3;
}
