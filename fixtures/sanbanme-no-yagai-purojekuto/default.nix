let
  overridez = import ./nix/haskell-overridez.nix;
  localhostKara = overridez.allIn ./nix/localhost/sanbanme-no-yagai-purojekuto;
  overlays =
    let dropTestPkgs = haskellPackagesNew: haskellPackagesOld: {
            foldl = null;
            managed = null;
            optparse-applicative = null;
            turtle = null;
            sanbanme-no-yagai-purojekuto = haskellPackagesNew.callPackage ./nix/sanbanme-no-yagai-purojekuto.nix {};
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
  { sanbanme-no-yagai-purojekuto = pkgs.haskellPackages.sanbanme-no-yagai-purojekuto;
  }
