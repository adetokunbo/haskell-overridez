let
  nixpkgs = import ./nix/18_03.nix;
  pkgs = import nixpkgs;
in
  import ./. { inherit pkgs; debug=false; }
