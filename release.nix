let
  pkgs = import ./nix/17_09.nix;
in
  import ./. { inherit pkgs; debug=false; }
