let
  overridez = import ./nix/haskell-overridez.nix;
  githubKara = overridez.allIn ./nix/github.com/adetokunbo/example-fetched-haskell-overridez;
  overlays =
    let dropTestPkgs = haskellPackagesNew: haskellPackagesOld: {
            foldl = null;
            managed = null;
            optparse-applicative = null;
            turtle = null;
            ichibanme-no-yagai-purojekuto = haskellPackagesNew.callPackage ./nix/ichibanme-no-yagai-purojekuto.nix {};
          };
    in [
      (newPkgs: oldPkgs:
         let
           inherit (oldPkgs.lib) composeExtensions fold;
           composeExtensionsList = fold composeExtensions (_: _: {});
         in {
           haskellPackages = oldPkgs.haskellPackages.override {
             overrides = composeExtensionsList [dropTestPkgs (overridez.combineAllIn ./nix [githubKara])];
         };
      })
    ];
  pkgs = import <nixpkgs> { inherit overlays; };
in
  { ichibanme-no-yagai-purojekuto = pkgs.haskellPackages.ichibanme-no-yagai-purojekuto;
  }
