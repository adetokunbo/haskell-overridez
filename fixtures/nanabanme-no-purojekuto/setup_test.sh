#!/bin/bash
set -euo pipefail

setup_test() {
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    pushd $this_dir > /dev/null

    HOZ_OPTS=dontCheck
    $HOZ_TEST_CMD cabal://optparse-applicative-0.14.2.0
    HOZ_OPTS=doJailbreak
    $HOZ_TEST_CMD cabal://turtle-1.5.8
    $HOZ_TEST_CMD cabal://foldl-1.3.7
    $HOZ_TEST_CMD cabal://managed-1.0.6

    popd > /dev/null
}

setup_test
