# haskell-overridez [![CircleCI](https://circleci.com/gh/adetokunbo/haskell-overridez.svg?style=svg)](https://circleci.com/gh/adetokunbo/haskell-overridez)

__haskell-overridez__ is a tool and library of nix functions that simplify the use of overrides while developing haskell projects with [nixpkgs](https://github.com/NixOS/nixpkgs).

## Inspiration

__haskell-overridez__ is inspired by the section on [Advanced Dependency Management](https://github.com/Gabriel439/haskell-nix/tree/master/project4) in [haskell-nix](https://github.com/Gabriel439/haskell-nix).
The idea is to turn the recommendations there into a tool that is itself installed into the nix environment.


## Installation

It's assumed that you have already [installed nix](https://nixos.wiki/wiki/Nix_Installation_Guide).

You can then install `haskell-overridez` using `nix-env`:

```bash

nix-env --install -f https://github.com/adetokunbo/haskell-overridez/archive/v0.10.3.0.tar.gz

```

## Basic usage

Installation adds the executable `haskell-overridez` to the nix environment.

It writes the output of the other tools it uses to subdirectories of the development project.

E.g,

```bash
$ cd my-nix-project

# install an override using github json
$ haskell-overridez -g reflex-frp/reflex-dom-contrib

# install an override using cabal2nix
$ haskell-overridez https://github.com/tathougies/beam -- --subpath beam-core
```

There are various options for managing the overrides; to see them all, you can read the help message:

```bash
$ haskell-overridez -h
haskell-overridez - manage nix overrides for haskell packages
...

```

### Project Layout

Given the previous example commands, `haskell-overridez` creates a project with the following layout:

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
 2. There is a `haskell-overridez.nix` file that contains the nix expression used to load the accompanying nix expression library.
 3. There are subdirectories (`nix-expr`, `git-json`) that contain the output from the tools.
 4. The accompanying library functions use the contents of the subdirectories to generate a nix expression that combines all the overrides into a single nix overlay.

### Using the library functions

The library functions can be used from `default.nix` by setting the `overlays` attribute.

```nix

let
  overridez = import ./nix/haskell-overridez.nix;
  overlays = [
    (newPkgs: oldPkgs: {
      haskellPackages = oldPkgs.haskellPackages.override {
        overrides = overridez.allIn ./nix;
      };
    })
  ];
  pkgs = import <nixpkgs> { inherit overlays; };

in
  {}

```

or by setting the `packageOverrides` attribute of the config element.

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
  {}

```

Some overrides can't be specified using the features of `haskell-overridez` and need to be specified directly. These direct overrides can be combined with the configured ones using `combineAllIn` instead of `allIn`:

```nix

let
  overridez = import ./nix/haskell-overridez.nix;
  myManualOverride = self: super: {};
  myImportedOverrides = import /from/some/nix/file.nix;

  overlays = [
    (newPkgs: oldPkgs: {
      haskellPackages = oldPkgs.haskellPackages.override {
        overrides = overridez.combineAllIn ./nix [myManualOverride myImportedOverrides];
      };
    })
  ];
  pkgs = import <nixpkgs> { inherit overlays; };

in
  {}


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

## Fetching shared configs

`haskell-overridez` has a `fetch` subcommand that makes it easy to share the overrides it manages.  As long as the override config files are saved in a git repository, they can be fetched for use in other projects.
`haskell-overridez fetch` copies the override configuration from a target git repo to a subdirectory of the current project's `nix` directory.

### Sharing public projects

- Use `haskell-overridez` to manage the `nix` overrides of a project
- Publish the project to an online git repo, ensuring that the `nix` folder is a top-level directory in the repo
- Use `haskell-overridez fetch <url-of-repo>` to clone the project's nix configs

#### Fetch configs from private git clones

To fetch from local private git repos, use a [file url][] to the git directory.

#### Examples

- [This example repo][] can have its configs fetched.
- See the integration test fixtures for examples of [remote fetching][] and [local fetching][], and for usage of [a remote-fetched config][] and of [a local-fetched config][].

[This example repo]: https://github.com/adetokunbo/example-fetched-haskell-overridez
[file url]: https://en.wikipedia.org/wiki/File_URI_scheme
[remote fetching]: https://github.com/adetokunbo/haskell-overridez/blob/8c18163683145f11fdf37b9ee85860452f2ea057/fixtures/ichibanme-no-yagai-purojekuto/setup_test.sh
[local fetching]: https://github.com/adetokunbo/haskell-overridez/blob/8c18163683145f11fdf37b9ee85860452f2ea057/fixtures/sanbanme-no-yagai-purojekuto/setup_test.sh
[a remote-fetched config]: https://github.com/adetokunbo/haskell-overridez/blob/8c18163683145f11fdf37b9ee85860452f2ea057/fixtures/ichibanme-no-yagai-purojekuto/default.nix
[a local-fetched config]: https://github.com/adetokunbo/haskell-overridez/blob/8c18163683145f11fdf37b9ee85860452f2ea057/fixtures/sanbanme-no-yagai-purojekuto/default.nix

## Contributions

Contributions are welcome! Please raise an [issue](https://github.com/adetokunbo/haskell-overridez/issues) to report any problems or [open a PR](https://github.com/adetokunbo/haskell-overridez/pulls) with fixes and improvements.

## Versioning

`haskell-overridez` uses [PVP][].  While it does not provide a library, the package provides an executable and nixpkg functions.  Its __public API__ is defined as the documentation provided by `haskell-overridez -h` and all the nixpkgs functions exported by `default.nix`.

[PVP]: http://pvp.haskell.org

### Updating after releases

Each 'version' tag (e.g, vN.N.N) in the repository is a release.  To update managed projects to a new release

- re-install `haskell-overridez`
- update the projects to use the releases's nixpkgs functions with `haskell-overridez -i`

#### Newer projects

These are installed using a versioned archive file, so updating is optional.    If you don't upgrade, any existing projects will be unaffected.

#### Older projects

These were installed with the original installation instructions that used an archive of the  master branch.  Unfortunately, after each new release, the hash of the master branch changes,  meaning that the hash specified in `/nix/haskell-overridez.nix` of these projects becomes incorrect, and derivations using the overrides will start to fail to load.  In these cases, you __must__ update `haskell-overridez` and the affected projects.


## Road Map

  1. Ask people to try it out to see if its useful ([reddit](https://www.reddit.com/r/haskell/comments/8k9g08/a_tool_that_helps_automate_the_installation_and/?ref=share&ref_source=link))
  2. Iterate on any proposed feature requests (.. ongoing)
  3. (??) Merge it into [nixpkgs](https://github.com/NixOS/nixpkgs) (later, if people think that's a good idea)

## License
BSD-3 clause
