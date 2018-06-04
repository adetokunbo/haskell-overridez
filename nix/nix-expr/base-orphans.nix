{ mkDerivation, base, ghc-prim, hspec, QuickCheck, stdenv }:
mkDerivation {
  pname = "base-orphans";
  version = "0.6";
  sha256 = "03mdww5j0gwai7aqlx3m71ldmjcr99jzpkcclzjfclk6a6kjla67";
  libraryHaskellDepends = [ base ghc-prim ];
  testHaskellDepends = [ base hspec QuickCheck ];
  homepage = "https://github.com/haskell-compat/base-orphans#readme";
  description = "Backwards-compatible orphan instances for base";
  license = stdenv.lib.licenses.mit;
}
