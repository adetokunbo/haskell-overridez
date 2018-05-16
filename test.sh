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

test
