let
  overridez = import ./nix/haskell-overridez.nix;
  localhostKara = overridez.allIn ./nix/localhost/sanbanme-no-yagai-purojekuto;
  config = {
    packageOverrides = pkgs:
      let
        inherit (pkgs.lib) composeExtensions fold;
        composeExtensionsList = fold composeExtensions (_: _: {});
        dropTestPkgs = self: super: {
          foldl = null;
          managed = null;
          optparse-applicative = null;
          turtle = null;
          sanbanme-no-yagai-purojekuto = self.callPackage ./nix/sanbanme-no-yagai-purojekuto.nix {};
        };
      in {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = composeExtensionsList [dropTestPkgs (overridez.combineAllIn ./nix [localhostKara])];
        };
      };
  };
  pkgs = import <nixpkgs> { inherit config; };
in
  { sanbanme-no-yagai-purojekuto = pkgs.haskellPackages.sanbanme-no-yagai-purojekuto;
  }
