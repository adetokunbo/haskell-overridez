let
  overridez = import ./nix/haskell-overridez.nix;
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = overridez.allIn ./nix;
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };
in
  { yonbanme-no-purojekuto = pkgs.haskellPackages.yonbanme-no-purojekuto;
  }