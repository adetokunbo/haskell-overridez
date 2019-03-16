#!/bin/bash
set -euo pipefail

test() {
    [[ -n ${HOZ_TEST_DEBUG:-''} ]] && set -x
    HOZ_TMP_DIR=$(mktemp -d)
    trap "rm -fR $HOZ_TMP_DIR" INT TERM EXIT

    # set up the HOZ_TEST_CMD to point at the haskell-overridez built by
    # the nix-build
    local test_descs=()
    local skipped_descs=()
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    pushd $this_dir
    nix-build --show-trace build.nix
    export HOZ_TEST_CMD=$(pwd)/result/bin/haskell-overridez
    popd

    fixture_dir=$this_dir/fixtures
    local test_dirs="$@"
    [[ $# == 0 ]] && test_dirs=$(ls $fixture_dir)
    for d in $test_dirs
    do
        local test_dir=${fixture_dir}/${d}
        [[ -d $test_dir ]] || continue

        pushd $test_dir > /dev/null
        # maybe skip
        [[ -f ./SKIP ]] && {
            local reason=$(cat ./SKIP)
            local cause="${reason:-'uncompleted test'}"
            local skipped_desc="$cause (in $test_dir)"
            skipped_descs+=("$skipped_desc")
            echo
            echo "SKIPPED: $skipped_desc"
            echo
            popd > /dev/null
            continue
        }

        local test_desc="project: $test_dir"
        [[ -f ./DESC ]] && test_desc="$(cat DESC) (in $test_dir)"
        test_descs+=("$test_desc")
        echo
        echo "testing: ${test_desc}"
        echo
        _test_one_project $test_dir
        echo
        echo "OK: ${test_desc}"

        popd > /dev/null
    done

    echo
    echo "tested haskell-overridez v$($HOZ_TEST_CMD -v | head -n 1)"
    echo "completed: ${#test_descs[@]} integration tests, skipped ${#skipped_descs[@]}"
    if (( ${#test_descs[@]} != 0 ))
    then
        for test_desc in "${test_descs[@]}"
        do
            echo "OK: ${test_desc}"
        done
    fi
    [[ -n ${HOZ_TEST_DEBUG:-''} ]] && set +x || return 0
}

_test_one_project() {
    local test_dir=${1-''}
    [[ -z $test_dir ]] && return 1;

    # setup
    _prepare_nix_dir
    _add_current_project_to_nix
    source setup_test.sh

    # test, defaulting 'nix-build'
    [[ -f test.sh ]] && source test.sh || nix-build --no-out-link --show-trace

    # cleanup if the debug flag is not set
    [[ -d nix ]] && [[ -z ${HOZ_TEST_DEBUG:-''} ]] && rm -fR nix || return 0
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
    local nix_file="./nix/${cwd##*/}.nix"
    cabal2nix . > $nix_file
    sed -i'.bak' -e 's|src = ./.|src = ../.|' $nix_file
}

echo "DEPRECATED: instead of test.sh, use 'nix-shell --run 'cabal new-test haskell-overridez-tests'"
# test "$@"
