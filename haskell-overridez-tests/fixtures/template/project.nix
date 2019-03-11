{ pkgs ? (import <nixpkgs> {}) }:

let
  haskellPackages = pkgs.haskellPackages;
in rec {
  purojekuto-no-namae = haskellPackages.callCabal2nix "purojekuto-no-namae" ./. {};
}