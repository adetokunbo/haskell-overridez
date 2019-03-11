{ system ? builtins.currentSystem
, nixBase ? "18.09"
}:
let
  pkgsMake = import ./nix/fetchPkgsMake.nix {};
  nixVersion = import (./. + "/nix/${nixBase}.nix");
  nixpkgs = import ./nix/fetchNixPkgs.nix nixVersion;
  pkgsMakeArgs = {
    nixpkgsRev = nixVersion.rev;
    nixpkgsSha256 = nixVersion.sha256;
    haskellArgs = {
      envMoreTools = [
        pkgs.haskellPackages.apply-refact
        pkgs.haskellPackages.cabal2nix
        pkgs.haskellPackages.cabal-install
        pkgs.haskellPackages.ghcid
        pkgs.haskellPackages.hlint
        pkgs.haskellPackages.hoogle
        pkgs.haskellPackages.stylish-cabal
        pkgs.haskellPackages.stylish-haskell
      ];
    };
  };
  pkgs = import nixpkgs { inherit system; };

in

pkgsMake pkgsMakeArgs ({ call, lib, ... }: rec {
  haskell-overridez-exe = call.haskell.cabal2nix.app ./.;
  haskell-overridez = call.package ./nix/wrapper;
  haskell-overridez-tests = call.haskell.cabal2nix.lib ./haskell-overridez-tests;
})
