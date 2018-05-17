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
  { gobanme-no-purojekuto = pkgs.haskellPackages.gobanme-no-purojekuto;
  }
