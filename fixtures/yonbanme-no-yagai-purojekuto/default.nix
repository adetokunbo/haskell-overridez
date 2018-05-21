let
  overridez = import ./nix/haskell-overridez.nix;
  localhostKara = import ./nix/indirect_import.nix;
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
          yonbanme-no-yagai-purojekuto = self.callPackage ./nix/yonbanme-no-yagai-purojekuto.nix {};
        };
      in {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = composeExtensionsList [dropTestPkgs (overridez.combineAllIn ./nix [localhostKara])];
        };
      };
  };
  pkgs = import <nixpkgs> { inherit config; };
in
  { yonbanme-no-yagai-purojekuto = pkgs.haskellPackages.yonbanme-no-yagai-purojekuto;
  }
