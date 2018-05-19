let
  overridez = import ./nix/haskell-overridez.nix;
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
          nanabanme-no-purojekuto = self.callPackage ./nix/nanabanme-no-purojekuto.nix {};
        };
      in {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = dropTestPkgs;
        };
      };
  };
  pkgs = import <nixpkgs> { inherit config; };
in
  { nanabanme-no-purojekuto = pkgs.haskellPackages.nanabanme-no-purojekuto;
  }
