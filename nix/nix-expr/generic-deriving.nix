{ mkDerivation, base, containers, ghc-prim, hspec, stdenv
, template-haskell
}:
mkDerivation {
  pname = "generic-deriving";
  version = "1.11.2";
  sha256 = "1y92q4dmbyc24hjjvq02474s9grwabxffn16y31gzaqhm0m0z5i9";
  libraryHaskellDepends = [
    base containers ghc-prim template-haskell
  ];
  testHaskellDepends = [ base hspec template-haskell ];
  homepage = "https://github.com/dreixel/generic-deriving";
  description = "Generic programming library for generalised deriving";
  license = stdenv.lib.licenses.bsd3;
}
