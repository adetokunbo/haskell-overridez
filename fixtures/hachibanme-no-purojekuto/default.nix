let
  overridez = import ./nix/haskell-overridez.nix;
  jibunNo = import ./nix/indirect_import.nix;
  overlays =
    let dropTestPkgs = haskellPackagesNew: haskellPackagesOld: {
            foldl = null;
            managed = null;
            optparse-applicative = null;
            turtle = null;
            hachibanme-no-purojekuto = haskellPackagesNew.callPackage ./nix/hachibanme-no-purojekuto.nix {};
          };
    in [
      (newPkgs: oldPkgs:
         let
           inherit (oldPkgs.lib) composeExtensions fold;
           composeExtensionsList = fold composeExtensions (_: _: {});
         in {
           haskellPackages = oldPkgs.haskellPackages.override {
             overrides = composeExtensionsList [dropTestPkgs jibunNo];
         };
      })
    ];
  pkgs = import <nixpkgs> { inherit overlays; };
in
  { hachibanme-no-purojekuto = pkgs.haskellPackages.hachibanme-no-purojekuto;
  }
