let
  overridez = import ./nix/haskell-overridez.nix;
  localhostKara = import ./nix/indirect_import.nix;
  overlays =
    let dropTestPkgs = haskellPackagesNew: haskellPackagesOld: {
            foldl = null;
            managed = null;
            optparse-applicative = null;
            turtle = null;
            yonbanme-no-yagai-purojekuto = haskellPackagesNew.callPackage ./nix/yonbanme-no-yagai-purojekuto.nix {};
          };
    in [
      (newPkgs: oldPkgs:
         let
           inherit (oldPkgs.lib) composeExtensions fold;
           composeExtensionsList = fold composeExtensions (_: _: {});
         in {
           haskellPackages = oldPkgs.haskellPackages.override {
             overrides = composeExtensionsList [dropTestPkgs (overridez.combineAllIn ./nix [localhostKara])];
         };
      })
    ];
  pkgs = import <nixpkgs> { inherit overlays; };
in
  { yonbanme-no-yagai-purojekuto = pkgs.haskellPackages.yonbanme-no-yagai-purojekuto;
  }
