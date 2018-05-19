#!/bin/bash
set -euo pipefail

test() {
    [[ -n ${HOZ_TEST_DEBUG:-''} ]] && set -x
    HOZ_TMP_DIR=$(mktemp -d)
    trap "rm -fR $HOZ_TMP_DIR" INT TERM EXIT
    trap 'echo "FAILED: $test_desc"; return 1' ERR

    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    local test_descs=()
    fixture_dir=$this_dir/fixtures
    for d in $(ls $fixture_dir)
    do
        local test_dir=${fixture_dir}/${d}
        [[ -d $test_dir ]] || continue

        pushd $test_dir > /dev/null
        # maybe skip
        [[ -f ./SKIP ]] && {
            local reason=$(cat ./SKIP)
            local cause="${reason:-'uncompleted test'}"
            echo
            echo "SKIPPED: $test_dir: $cause"
            echo
            popd > /dev/null
            continue
        }

        local test_desc="project: $test_dir"
        [[ -f ./DESC ]] && test_desc="$(cat DESC) (in $test_dir)"
        test_descs+=("$test_desc")
        echo
        echo "testing: ${test_desc}"
        _test_one_project $test_dir
        echo
        echo "OK: ${test_desc}"

        popd > /dev/null
    done

    echo
    echo "completed: ${#test_descs[@]} integration tests"
    for test_desc in "${test_descs[@]}"
    do
        echo "OK: ${test_desc}"
    done
    [[ -n ${HOZ_TEST_DEBUG:-''} ]] && set +x
}

_test_one_project() {
    local test_dir=${1-''}
    [[ -z $test_dir ]] && return 1;

    # setup
    _prepare_nix_dir
    _add_current_project_to_nix
    source setup_test.sh

    # test
    [[ -f test.sh ]] && source test.sh || nix-build --no-out-link

    # cleanup
    [[ -d nix ]] && rm -fR nix
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
    local cwd=$(pwd)
    local nix_file="nix/nix-expr/${cwd##*/}.nix"
    mkdir -p nix/nix-expr
    cabal2nix . > $nix_file
    sed -i'.bak' -e 's|src = ./.|src = ../../.|' $nix_file
}

test
