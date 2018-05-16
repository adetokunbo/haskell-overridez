{ debug ? false, pkgs ? import <nixpkgs> {} }:
with pkgs;
let inherit (pkgs.lib) composeExtensions fold foldr listToAttrs mapAttrs';
    inherit (builtins) fromJSON match pathExists readFile readDir replaceStrings toPath trace;

    composeExtensionsList = fold composeExtensions (_: _: {});
    isDir = path: pathExists (toPath (toString (path + "/.")));
    _trace = msg: result: if debug then (trace msg result) else result;
in
rec {
  haskell-overridez = stdenvNoCC.mkDerivation {
    name = "haskell-overridez";

    nativeBuildInputs = [ makeWrapper ];

    unpackPhase = ":";

    installPhase = ''
      install -vD ${./haskell-overridez} $out/bin/$name;
      wrapProgram $out/bin/$name \
        --prefix PATH : ${stdenv.lib.makeBinPath ([ cabal2nix gnugrep gnused nix-prefetch-scripts ])} \
        --set HOME /homeless-shelter
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

  combineAllIn = rootDir: otherOverrides:
    let
      extDir = d: (toString rootDir) + "/" + d;
      allExtensions = [
        (nixExprIn (extDir "nix-expr"))
        (gitJsonIn (extDir "git-json"))
        (optionsIn (extDir "options"))
      ] ++ otherOverrides;
    in
      composeExtensionsList allExtensions;

  allIn = rootDir: combineAllIn rootDir [];

  nixExprIn = aDir: self: super:
    let
      mkOverride = f: self.callPackage (aDir + "/${f}") { };
      toPackage = f: _: _trace "found override (nix-expr): ${f}" rec {
        name  = builtins.replaceStrings [ ".nix" ] [ "" ] f;
        value = _trace ("using override (nix-expr): ${name}") (mkOverride f);
      };
    in
      if isDir aDir
      then mapAttrs' toPackage (readDir (toPath aDir))
      else _trace ("no overrides (nix-expr): directory not found ${aDir}") {};

  gitJsonIn = aDir: self: super:
    let
      inherit (pkgs) fetchFromGitHub;
      inherit (pkgs.lib) zipListsWith;

      toGithubAttrs = src:
        let ownerRepo = match "https://github.com/(.*)/(.*)\.git" src.url;
            zipNV = zipListsWith (fst: snd: {name = fst; value = snd; });
            ownerRepoAttrs = listToAttrs (zipNV ["owner" "repo"] ownerRepo);
        in { inherit (src) rev sha256; } // ownerRepoAttrs;

      applyFuncs = funcs: pkgName: foldr (g: a: g a) pkgName funcs;

      readDirOverrides = fetcher: toFetchAttrs: d:
        let filePath = n: d + "/${n}.json";
            loadFuncs = [fetcher toFetchAttrs fromJSON readFile filePath];
            mkOverride = n: self.callCabal2nix n (applyFuncs loadFuncs n) {};
            toPackage = file: _: _trace "found override (git-json): ${file}" rec {
              name  = replaceStrings [ ".json" ] [ "" ] file;
              value = _trace ("using override (git-json): ${name}") (mkOverride name);
            };
        in mapAttrs' toPackage (readDir d);

    in
      if isDir aDir
      then readDirOverrides fetchFromGitHub toGithubAttrs aDir
      else _trace ("no overrides (git-json): was not a dir ${aDir}") {};

  optionsIn = aDir: self: super:
    let
      inherit (builtins) attrValues filter intersectAttrs isList split;
      inherit (pkgs.lib) mapAttrs;

      supportedOptions = {
        doJailbreak = pkgs.haskell.lib.doJailbreak;
        dontCheck = pkgs.haskell.lib.dontCheck;
        dontHaddock = pkgs.haskell.lib.dontHaddock;
      };

      mkOverrides = option: names: _: super:
        let
          f = supportedOptions.${option};
          toPackage = name: _trace "found override (options): ${toString name}" {
            inherit name;
            value = _trace ("using override (options): ${toString name}") (f super.${name});
          };
        in
          _trace "found option ${option} on pkgs ${toString names}"
          listToAttrs (map toPackage names);

      readOptionsOverrides = d:
        let
          nonEmptyNonList = x: (!(isList x)) && x != "";
          availableOptions = intersectAttrs (readDir d) supportedOptions;
          lines = f: filter nonEmptyNonList (split "\n" (readFile (d + "/${f}")));
          namesPerOption = mapAttrs (opt: _: lines opt) availableOptions;
          optionsOverrides = attrValues (mapAttrs mkOverrides namesPerOption);
        in
          (composeExtensionsList optionsOverrides) self super;
    in
      if isDir aDir
      then readOptionsOverrides aDir
      else _trace ("no overrides (options): was not a dir ${aDir}") {};
}
