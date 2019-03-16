{ all-cabal-hashes
, cabal2nix
, gnugrep
, gnused
, haskellPackages
, makeWrapper
, nix-prefetch-scripts
, stdenv
}:

stdenv.mkDerivation rec {
  name = "haskell-overridez";
  version = haskellPackages.haskell-overridez-exe.version;
  unpackPhase = "true";
  nativeBuildInputs = [ makeWrapper ];
  buildCommand = ''
    mkdir -p $out/bin
    makeWrapper ${haskellPackages.haskell-overridez-exe}/bin/haskell-overridez $out/bin/haskell-overridez \
      --prefix PATH : ${stdenv.lib.makeBinPath ([ cabal2nix gnugrep gnused nix-prefetch-scripts ])} \
      --set HOME /homeless-shelter \
      --set HOZ_ALL_CABAL_HASHES ${all-cabal-hashes}
  '';

  meta = haskellPackages.haskell-overridez-exe.meta;
}