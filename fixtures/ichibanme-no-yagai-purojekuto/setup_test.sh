#!/bin/bash
set -euo pipefail

setup_test() {
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    pushd $this_dir > /dev/null

    $HOZ_TEST_CMD fetch https://github.com/adetokunbo/example-fetched-haskell-overridez

    popd > /dev/null
}

setup_test
