{ debug ? false }:
let
  pkgs = import ./nix/17_09.nix;
in
  import ./. { inherit debug pkgs; }
