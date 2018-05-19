let
  overridez = import ./nix/haskell-overridez.nix;
  githubKara = overridez.allIn ./nix/github.com/adetokunbo/example-fetched-haskell-overridez;
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = overridez.combineAllIn ./nix [githubKara];
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };
in
  { ichibanme-no-yagai-purojekuto = pkgs.haskellPackages.ichibanme-no-yagai-purojekuto;
  }
