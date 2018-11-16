# Update this file to change the pinned version of nixpkgs. The contents of this
# file also determine the cache key on circle CI.

let
  nixpkgs = import ./nix/18_09.nix;
  pkgs = import nixpkgs;
in
  import ./. { inherit pkgs; debug=false; }
