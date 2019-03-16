{ debug ? false, pkgs ? import <nixpkgs> {} }:
let
  lib = import ./lib.nix { inherit debug pkgs; };
  build = import ./build.nix {};
in
 {
  inherit (build) haskell-overridez;
  inherit (lib) allIn combineAllIn nixExprIn gitJsonIn optionsIn;
 }
