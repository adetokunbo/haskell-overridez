{ all-cabal-hashes, cabal2nix, gnugrep, gnused, nix-prefetch-scripts, makeWrapper
, mkDerivation, aeson, aeson-casing, attoparsec, base, bytestring
, Cabal, exceptions, foldl, managed, neat-interpolation
, network-uri , optparse-applicative, stdenv, system-fileio
, system-filepath, text, turtle
}:
mkDerivation {
  pname = "haskell-overridez";
  version = "0.10.3.1";
  src = ../../.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-casing attoparsec base bytestring Cabal exceptions
    foldl managed neat-interpolation network-uri optparse-applicative
    system-fileio system-filepath text turtle
  ];
  description = "Manage nix overrides for haskell packages";
  license = stdenv.lib.licenses.bsd3;
  executableToolDepends = [ makeWrapper ];
  postInstall = ''
     wrapProgram "$out/bin/haskell-overridez" \
      --prefix PATH : ${stdenv.lib.makeBinPath ([ cabal2nix gnugrep gnused nix-prefetch-scripts ])} \
      --set HOME /homeless-shelter \
      --set HOZ_ALL_CABAL_HASHES ${all-cabal-hashes}
  '';
}
