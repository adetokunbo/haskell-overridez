let
  overridez = import ./nix/haskell-overridez.nix;
  overlays =
    let dropTestPkgs = haskellPackagesNew: haskellPackagesOld: {
            foldl = null;
            managed = null;
            optparse-applicative = null;
            turtle = null;
            nanabanme-no-purojekuto = haskellPackagesNew.callPackage ./nix/nanabanme-no-purojekuto.nix {};
          };
    in [
      (newPkgs: oldPkgs: = {
           haskellPackages = oldPkgs.haskellPackages.override {
             overrides = dropTestPkgs;
         };
      })
    ];
  pkgs = import <nixpkgs> { inherit overlays; };
in
  { nanabanme-no-purojekuto = pkgs.haskellPackages.nanabanme-no-purojekuto;
  }
