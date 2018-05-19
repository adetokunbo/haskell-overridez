let
  overridez = import ./nix/haskell-overridez.nix;
  githubKara = overridez.allIn ./nix/github.com/adetokunbo/example-fetched-haskell-overridez;
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
          ichibanme-no-yagai-purojekuto = self.callPackage ./nix/ichibanme-no-yagai-purojekuto.nix {};
        };
      in {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = composeExtensionsList [dropTestPkgs (overridez.combineAllIn ./nix [githubKara])];
        };
      };
  };
  pkgs = import <nixpkgs> { inherit config; };
in
  { ichibanme-no-yagai-purojekuto = pkgs.haskellPackages.ichibanme-no-yagai-purojekuto;
  }
