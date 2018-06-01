#!/bin/bash
set -euo pipefail

setup_test() {
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    pushd $this_dir > /dev/null

    HOZ_OPTS=dontCheck
    $HOZ_TEST_CMD https://github.com/pcapriotti/optparse-applicative
    HOZ_OPTS=doJailbreak
    $HOZ_TEST_CMD https://github.com/Gabriel439/Haskell-Turtle-Library
    $HOZ_TEST_CMD https://github.com/Gabriel439/Haskell-Foldl-Library
    $HOZ_TEST_CMD https://github.com/Gabriel439/Haskell-Managed-Library

    popd > /dev/null
}

setup_test
