#!/bin/bash
set -euo pipefail

setup_test() {
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    pushd $this_dir > /dev/null

    $HOZ_TEST_CMD https://github.com/tathougies/beam --subpath beam-core

    popd > /dev/null
}

setup_test
