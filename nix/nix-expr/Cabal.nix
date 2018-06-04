{ mkDerivation, array, base, base-compat, base-orphans, binary
, bytestring, containers, deepseq, Diff, directory, filepath
, integer-logarithms, mtl, optparse-applicative, parsec, pretty
, process, QuickCheck, stdenv, tagged, tar, tasty, tasty-golden
, tasty-hunit, tasty-quickcheck, text, time, transformers
, tree-diff, unix
}:
mkDerivation {
  pname = "Cabal";
  version = "2.2.0.1";
  sha256 = "0yqa6fm9jvr0ka6b1mf17bf43092dc1bai6mqyiwwwyz0h9k1d82";
  libraryHaskellDepends = [
    array base binary bytestring containers deepseq directory filepath
    mtl parsec pretty process text time transformers unix
  ];
  testHaskellDepends = [
    array base base-compat base-orphans bytestring containers deepseq
    Diff directory filepath integer-logarithms optparse-applicative
    pretty process QuickCheck tagged tar tasty tasty-golden tasty-hunit
    tasty-quickcheck text tree-diff
  ];
  doCheck = false;
  homepage = "http://www.haskell.org/cabal/";
  description = "A framework for packaging Haskell software";
  license = stdenv.lib.licenses.bsd3;
}
