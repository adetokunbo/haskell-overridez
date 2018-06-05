{ debug ? false, pkgs ? import <nixpkgs> {} }:
with pkgs;
let inherit (pkgs.lib) composeExtensions fold foldr listToAttrs mapAttrs';
    inherit (builtins) fromJSON match pathExists readFile readDir replaceStrings toPath trace;

    composeExtensionsList = fold composeExtensions (_: _: {});
    isDir = path: pathExists (toPath (toString (path + "/.")));
    _trace = msg: result: if debug then (trace msg result) else result;
    overridezLib = import ./lib.nix { inherit debug pkgs; };
in
rec {

  inherit (overridezLib) allIn combineAllIn nixExprsIn gitJsonIn optionsIn;

  haskell-overridez = stdenvNoCC.mkDerivation {
    name = "haskell-overridez";

    nativeBuildInputs = [ makeWrapper ];

    unpackPhase = ":";

    installPhase = ''
      install -vD ${./haskell-overridez} $out/bin/$name;
      wrapProgram $out/bin/$name \
        --prefix PATH : ${stdenv.lib.makeBinPath ([ cabal2nix gnugrep gnused nix-prefetch-scripts ])} \
        --set HOME /homeless-shelter \
        --set HOZ_ALL_CABAL_HASHES ${all-cabal-hashes}
    '';

    preferLocalBuild = true;

    meta = with stdenv.lib; {
      description = "Script used to manage haskell overrides during development";
      license = licenses.bsd3;
      platforms = stdenv.lib.platforms.unix;
      maintainers = [{
        email = "tim.emiola@gmail.com";
        github = "adetokunbo";
        name = "Tim Emiola";
      }];
    };
  };
}
