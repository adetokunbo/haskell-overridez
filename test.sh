#!/bin/bash
set -euo pipefail

test() {
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    fixture_dir=$this_dir/fixtures
    for d in $(ls $fixture_dir)
    do
        local test_dir=${fixture_dir}/${d}
        [[ -d $test_dir ]] && {
            # setup
            echo "test project: ${test_dir}"
            pushd $test_dir > /dev/null
            _prepare_nix_dir
            _add_current_project_to_nix
            ./setup_test.sh

            # test
            trap 'popd > /dev/null' INT TERM EXIT
            nix-build --no-out-link
            echo
            echo "OK: test project : ${test_dir}"
            echo

            # cleanup
            [[ -d nix ]] && rm -fR nix
            popd > /dev/null
            trap - INT TERM EXIT
        }
    done
}

_prepare_nix_dir() {
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

_add_current_project_to_nix() {
    cwd=$(pwd)
    mkdir -p nix/nix-expr
    local nix_file="nix/nix-expr/${cwd##*/}.nix"
    cabal2nix . > $nix_file
    sed -i'' -e 's|src = ./.|src = ../../.|' $nix_file
}

test
