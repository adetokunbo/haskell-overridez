#!/bin/bash
set -euo pipefail

setup_test() {
    set -x
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    pushd $this_dir > /dev/null
    _cleanup_nix_dir
    _use_haskell_overridez
    _add_this_project
    popd > /dev/null
    set +x
}

_cleanup_nix_dir() {
    [[ -d nix ]] && rm -fR nix
    mkdir -p nix
    (cat <<EOF
let
  pkgs = import <nixpkgs> {};
in
  import (../../..) { inherit pkgs; }
EOF
    ) > "nix/haskell-overridez.nix"
}

_add_this_project() {
    mkdir -p nix/nix-expr
    local nix_file=nix/nix-expr/yonbanme-no-purojekuto.nix
    cabal2nix . > $nix_file
    sed -i'' -e 's|src = ./.|src = ../../.|' $nix_file
}

_use_haskell_overridez() {
    HOZ_OPTS=dontCheck
    haskell-overridez cabal://optparse-applicative-0.14.2.0
    HOZ_OPTS=doJailbreak
    haskell-overridez cabal://turtle-1.5.0
    haskell-overridez cabal://foldl-1.3.7
    haskell-overridez cabal://managed-1.0.6
}

setup_test
