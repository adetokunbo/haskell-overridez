#!/bin/bash
set -euo pipefail

setup_test() {
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    pushd $this_dir > /dev/null

    $HOZ_TEST_CMD --flag-override DontCheck cabal://optparse-applicative-0.14.2.0
    $HOZ_TEST_CMD --flag-override DoJailbreak cabal://turtle-1.5.12
    $HOZ_TEST_CMD --flag-override DoJailbreak cabal://foldl-1.4.5
    $HOZ_TEST_CMD --flag-override DoJailbreak cabal://managed-1.0.6

    popd > /dev/null
}

setup_test
