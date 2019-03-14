let
  overridez = import ./lib.nix {};
  overlays =
    let dropTestPkgs = haskellPackagesNew: haskellPackagesOld: {
            foldl = null;
            managed = null;
            optparse-applicative = null;
            turtle = null;
            purojekuto-no-namae = haskellPackagesNew.callPackage ./purojekuto-no-namae.nix {};
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
  { purojekuto-no-namae = pkgs.haskellPackages.purojekuto-no-namae;
  }
