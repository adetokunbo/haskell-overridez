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
          yonbanme-no-purojekuto = self.callPackage ./nix/yonbanme-no-purojekuto.nix {};
        };
      in {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = composeExtensionsList [dropTestPkgs (overridez.allIn ./nix)];
        };
      };
  };
  pkgs = import <nixpkgs> { inherit config; };
in
  { yonbanme-no-purojekuto = pkgs.haskellPackages.yonbanme-no-purojekuto;
  }
