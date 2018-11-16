{ debug ? false, pkgs ? import <nixpkgs> {} }:
let
  overridez = import ./lib.nix { inherit debug pkgs; };
  overlays = [
   (newPkgs: oldPkgs: {
     haskellPackages = oldPkgs.haskellPackages.override {
       overrides = overridez.allIn ./nix;
     };
   })
  ];
  nixpkgs = import ./nix/18_09.nix;
  pkgs = import nixpkgs { inherit overlays; };
in
 {
  inherit (pkgs.haskellPackages) haskell-overridez;
  inherit (overridez) allIn combineAllIn nixExprIn gitJsonIn optionsIn;
 }
