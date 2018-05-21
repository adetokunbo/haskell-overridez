let
  overridez = import ./nix/haskell-overridez.nix;
  config = {
    packageOverrides = pkgs:
      let
        inherit (pkgs.lib) composeExtensions fold;
        composeExtensionsList = fold composeExtensions (_: _: {});
        dropTestPkgs = self: super: {
          beam-core = null;
          kyuubanme-no-purojekuto = self.callPackage ./nix/kyuubanme-no-purojekuto.nix {};
        };
      in {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = composeExtensionsList [dropTestPkgs (overridez.allIn ./nix)];
        };
      };
  };
  pkgs = import <nixpkgs> { inherit config; };
in
  { kyuubanme-no-purojekuto = pkgs.haskellPackages.kyuubanme-no-purojekuto;
  }
