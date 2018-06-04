let
  overridez = import ./nix/haskell-overridez.nix;
  overlays =
    let dropTestPkgs = haskellPackagesNew: haskellPackagesOld: {
            foldl = null;
            managed = null;
            optparse-applicative = null;
            turtle = null;
            yonbanme-no-purojekuto = haskellPackagesNew.callPackage ./nix/yonbanme-no-purojekuto.nix {};
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
  { yonbanme-no-purojekuto = pkgs.haskellPackages.yonbanme-no-purojekuto;
  }
