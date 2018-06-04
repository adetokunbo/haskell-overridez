let
  overridez = import ./nix/haskell-overridez.nix;
  overlays =
    let dropTestPkgs = haskellPackagesNew: haskellPackagesOld: {
            foldl = null;
            managed = null;
            optparse-applicative = null;
            turtle = null;
            rokubanme-no-purojekuto = haskellPackagesNew.callPackage ./nix/rokubanme-no-purojekuto.nix {};
          };
    in [
      (newPkgs: oldPkgs:
         let
           inherit (oldPkgs.lib) composeExtensions fold;
           composeExtensionsList = fold composeExtensions (_: _: {});
         in {
           haskellPackages = oldPkgs.haskellPackages.override {
             overrides = composeExtensionsList [dropTestPkgs (overridez.allIn ./nix)];
         };
      })
    ];
  pkgs = import <nixpkgs> { inherit overlays; };
in
  { rokubanme-no-purojekuto = pkgs.haskellPackages.rokubanme-no-purojekuto;
  }
