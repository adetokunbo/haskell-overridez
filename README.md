# haskell-overridez

__haskell-overridez__ is a script and library of nix functions that simplify the use of overrides while developing haskell projects with [nixpkgs](https://github.com/NixOS/nixpkgs).


## Installation

It's assumed that you have already [installed nix](https://nixos.wiki/wiki/Nix_Installation_Guide).

You can then install `haskell-overridez` using `nix-env`:

```bash

nix-env --install -f https://git.io/vpHEF  # the git.io short URL for longer link below

or

nix-env --install -f https://github.com/adetokunbo/haskell-overridez/archive/master.tar.gz

```

## Usage

Installation adds the command `haskell-overridez` to the nix environment.

It is a wrapper script around other tools whose output it writes to subdirectories of the development project.

E.g,

```bash
$ cd my-nix-project

# install an override using github json
$ haskell-overridez -g reflex-frp/reflex-dom-contrib

# install an override using cabal2nix
$ haskell-overridez https://github.com/tathougies/beam --subpath beam-core
```

There various options for managing the overrides; to see them all, you can read the usage:

```bash
$ haskell-overridez -h
haskell-overridez - manage nix overrides for haskell packages
...

```

### Project Layout

Given the example commands above, haskell-overridez creates a project with the following layout:

```
├── default.nix
│
├── nix (1)
│   │
│   ├── haskell-overridez.nix (2)
│   │
│   ├── nix-expr (3)
│   │   └── beam-core.nix
│   │
│   ├── git-json (3)
│   │   └── reflex-dom-contrib.json
```

 1. There is a `nix` subdirectory of the main project directory.
 2. There is a haskell-overridez.nix file that contains the nix expression used to load the accompanying nix expression library.
 3. There are subdirectories (`nix-expr`, `git-json`) that contain the output from the tools.
 4. The accompanying library functions use the contents of the subdirectories to generate a nix expression that combines all the overrides into a single nix overlay.

### Using the library functions

The library functions can be used from default.nix as follows:

```nix

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

...

```

Some overrides can't be specified using __haskell-overridez__ and will need to specified in other ways.  The two sets can be combined using `combineAllIn` instead of `allIn`:

```nix

let
  overridez = import ./nix/haskell-overridez.nix;
  myManualOverride = ...
  myImportedOverrides = import /from/some/nix/file.nix

  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = overridez.combineAllIn ./nix [myManualOverride, myImportedOverrides];
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };

in

...

```

### Using the library functions in reflex-project-skeleton projects

Projects developed using the [Reflex Platform](https://github.com/reflex-frp/reflex-platform) can benefit from adopting the layout in
[reflex-project-skeleton](https://github.com/ElvishJerricco/reflex-project-skeleton).  This allows them to share
haskell code between frontend and backend.  In these projects, `haskell-overridez` can be used as follows:

```nix

let pkgs = import <nixpkgs> {};
    overridez = import ./nix/haskell-overridez.nix;
in

{}:

(import ../../repos/reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };

  overrides = overridez.allIn ./nix;
})

```

## Contributions

Contributions are welcome! Please raise an [issue](https://github.com/adetokunbo/haskell-overridez/issues) to report any problems or [open a PR](https://github.com/adetokunbo/haskell-overridez/pulls) with fixes and improvements.

## Road Map

  1. Ask people to try it out to see if its useful (.. soonish)
  2. Iterate on its features to make more useful (.. going forward)
  3. (??) Merge it into [nixpkgs](https://github.com/NixOS/nixpkgs) (later, if people think that's a good idea)

## Credit

__haskell-overridez__ is inspired by the section on [Advanced Dependency](https://github.com/Gabriel439/haskell-nix/tree/master/project4) in [haskell-nix](https://github.com/Gabriel439/haskell-nix).
The idea is to turn the recommendations there into a tool that is hosted in the nix store.

## License
BSD-3 clause
