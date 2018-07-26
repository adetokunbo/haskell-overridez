{ mkDerivation, base, bytestring, comonad, containers
, contravariant, criterion, fetchgit, hashable, mwc-random
, primitive, profunctors, semigroupoids, semigroups, stdenv, text
, transformers, unordered-containers, vector, vector-builder
}:
mkDerivation {
  pname = "foldl";
  version = "1.4.2";
  src = fetchgit {
    url = "https://github.com/Gabriel439/Haskell-Foldl-Library";
    sha256 = "1glp5qzd66rb23vvrfy2a09fbzxmj5s9cy51vsysy3w2wifnfkbr";
    rev = "96c4e00ad60270292c622327b60c08db36228ca3";
  };
  libraryHaskellDepends = [
    base bytestring comonad containers contravariant hashable
    mwc-random primitive profunctors semigroupoids semigroups text
    transformers unordered-containers vector vector-builder
  ];
  benchmarkHaskellDepends = [ base criterion ];
  description = "Composable, streaming, and efficient left folds";
  license = stdenv.lib.licenses.bsd3;
}
