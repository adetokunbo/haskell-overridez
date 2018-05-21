#!/bin/bash
set -euo pipefail

_check_is_different() {
    local search=${1:-''}
    local refs1=${2:-''}
    local refs2=${3:-''}
    [[ -z $refs2 ]] && return 1

    found1=$(grep -h $search $refs1)
    found2=$(grep -h $search $refs2)
    [[ $found1 == $found2 ]] && {
        echo
        printf "FAIL: expected runtime refs for $search to differ\n but '$found1' == '$found2'\n"
        return 1
    }
    echo
    printf "OK: runtime refs for $search differ:\n '$found1' != '$found2'\n"
}

test() {
    local cwd=$(pwd)
    local test_name="${cwd##*/}"

    # without options
    echo
    echo "instantiating the derivation without options"
    drv_wo_opts=$(nix-instantiate -A $test_name default.nix)
    echo "without options: derivation $drv_wo_opts"
    wo_opts=$(mktemp -q)
    nix-store -q --references $drv_wo_opts > $wo_opts
    cat $wo_opts

    # with options
    echo
    echo "instantiating the derivation with options"
    drv_with_opts=$(nix-instantiate -A $test_name with-options.nix)
    echo "without options: derivation $drv_with_opts"
    with_opts=$(mktemp -q)
    nix-store -q --references $drv_with_opts > $with_opts
    cat $with_opts

    # confirm the runtime references for the dependencies are different derivations
    _check_is_different "foldl" $wo_opts $with_opts
    _check_is_different "turtle" $wo_opts $with_opts
}

test
