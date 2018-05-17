#!/bin/bash
set -euo pipefail

test() {
    set -x
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    fixture_dir=$this_dir/fixtures
    for d in $(ls $fixture_dir)
    do
        local test_dir=${fixture_dir}/${d}
        [[ -d $test_dir ]] || continue

        pushd $test_dir > /dev/null
        # maybe skip
        [[ -f ./SKIP ]] && {
            local reason=$(cat ./SKIP)
            local cause="${reason:-'incomplete test'}"
            echo
            echo "SKIPPED: $test_dir: $cause"
            echo
            popd > /dev/null
            continue
        }
        _test_one_project $test_dir
    done
    set +x
}

_test_one_project() {
    local test_dir=${1-''}
    [[ -z $test_dir ]] && return 1;

    # setup
    trap 'popd > /dev/null' INT TERM EXIT

    echo
    echo "testing project: ${test_dir}"
    echo

    _prepare_nix_dir
    _add_current_project_to_nix
    ./setup_test.sh

    # test
    nix-build --no-out-link
    echo
    echo "OK: test project : ${test_dir}"
    echo

    # cleanup
    [[ -d nix ]] && rm -fR nix
    popd > /dev/null
    trap - INT TERM EXIT
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
    sed -i'.bak' -e 's|src = ./.|src = ../../.|' $nix_file
}

test
