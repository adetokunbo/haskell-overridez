let
  overridez = import ./nix/haskell-overridez.nix;
  overlays =
    let dropTestPkgs = haskellPackagesNew: haskellPackagesOld: {
            beam-core = null;
            kyuubanme-no-purojekuto = haskellPackagesNew.callPackage ./nix/kyuubanme-no-purojekuto.nix {};
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
  { kyuubanme-no-purojekuto = pkgs.haskellPackages.kyuubanme-no-purojekuto;
  }
