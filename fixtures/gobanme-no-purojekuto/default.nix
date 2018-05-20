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
          gobanme-no-purojekuto = self.callPackage ./nix/gobanme-no-purojekuto.nix {};
        };
      in {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = composeExtensionsList [dropTestPkgs (overridez.allIn ./nix)];
        };
      };
  };
  pkgs = import <nixpkgs> { inherit config; };
in
  { gobanme-no-purojekuto = pkgs.haskellPackages.gobanme-no-purojekuto;
  }
