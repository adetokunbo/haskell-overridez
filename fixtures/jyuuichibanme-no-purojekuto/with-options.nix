let
  overridez = import ./nix/haskell-overridez.nix;
  jibunNo = import ./nix/indirect_import.nix;
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
          jyuuichibanme-no-purojekuto = self.callPackage ./nix/jyuuichibanme-no-purojekuto.nix {};
        };
      in {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = composeExtensionsList [dropTestPkgs jibunNo];
        };
      };
  };
  pkgs = import <nixpkgs> { inherit config; };
in
  { jyuuichibanme-no-purojekuto = pkgs.haskellPackages.jyuuichibanme-no-purojekuto;
  }
