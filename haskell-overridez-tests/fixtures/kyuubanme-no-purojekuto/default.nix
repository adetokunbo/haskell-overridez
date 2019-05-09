let
  overridez = import ./lib.nix {};
  theOverrides = overridez.allIn ./nix;
  overlays =
    let dropTestPkgs = haskellPackagesNew: haskellPackagesOld: {
            logging-effect-extra-handler = null;
            kyuubanme-no-purojekuto = haskellPackagesNew.callPackage ./kyuubanme-no-purojekuto.nix {};
          };
    in [
      (newPkgs: oldPkgs:
         let
           inherit (oldPkgs.lib) composeExtensions fold;
           composeExtensionsList = fold composeExtensions (_: _: {});
         in {
           haskellPackages = oldPkgs.haskellPackages.override {
             overrides = composeExtensionsList [dropTestPkgs theOverrides];
         };
      })
    ];
  pkgs = import <nixpkgs> { inherit overlays; };
in
  { kyuubanme-no-purojekuto = pkgs.haskellPackages.kyuubanme-no-purojekuto;
  }
