let
  overridez = import ./. {};
  overlays = [
   (newPkgs: oldPkgs: {
     haskellPackages = oldPkgs.haskellPackages.override {
       overrides = overridez.allIn ./nix;
     };
   })
  ];
  pkgs = import <nixpkgs> { inherit overlays; };
in
 { inherit (pkgs.haskellPackages) haskell-overridez; }
