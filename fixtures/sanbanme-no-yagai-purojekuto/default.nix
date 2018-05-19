let
  overridez = import ./nix/haskell-overridez.nix;
  githubKara = overridez.allIn ./nix/localhost/example-fetched-haskell-overridez;
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = overridez.combineAllIn ./nix [githubKara];
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };
in
  { sanbanme-no-yagai-purojekuto = pkgs.haskellPackages.sanbanme-no-yagai-purojekuto;
  }
