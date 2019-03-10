{ # The defaulted revision of the pkgsMake repo
  pkgsMakeRev ? "fff811a6d5aa386e835168e5f7b3dfca81b5c910"

  # The SHA256 of the repo at pkgsMakeRev
, pkgsMakeSha256 ? "0mbvmmza58frrj4cqs9ky2bwgw3z6wkpb8d8k1911g11rapl7wgq"
}:
let
  pkgsMakePath = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "shajra";
    repo = "example-nix";
    rev = pkgsMakeRev;
    sha256 = pkgsMakeSha256;
  };
in
  import pkgsMakePath
