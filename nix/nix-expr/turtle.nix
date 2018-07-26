{ mkDerivation, ansi-wl-pprint, async, base, bytestring, clock
, containers, criterion, directory, doctest, exceptions, fetchgit
, foldl, hostname, managed, optional-args, optparse-applicative
, process, semigroups, stdenv, stm, system-fileio, system-filepath
, temporary, text, time, transformers, unix, unix-compat
}:
mkDerivation {
  pname = "turtle";
  version = "1.5.10";
  src = fetchgit {
    url = "https://github.com/Gabriel439/Haskell-Turtle-Library";
    sha256 = "1xc7qj82m58ispgbxm09n3n2jj81kj8g1hpx9ggb1rb65isdzr7q";
    rev = "884201b98f47351aba458ca32cea4f5fe89465b2";
  };
  libraryHaskellDepends = [
    ansi-wl-pprint async base bytestring clock containers directory
    exceptions foldl hostname managed optional-args
    optparse-applicative process semigroups stm system-fileio
    system-filepath temporary text time transformers unix unix-compat
  ];
  testHaskellDepends = [ base doctest system-filepath temporary ];
  benchmarkHaskellDepends = [ base criterion text ];
  description = "Shell programming, Haskell-style";
  license = stdenv.lib.licenses.bsd3;
}
