#!/bin/bash
set -euo pipefail

setup_test() {
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    pushd $this_dir > /dev/null

    HOZ_OPTS=dontCheck

    # Using -g with foldl or optparse-applicative makes cabal2nix infinitely
    # recurse when converting the json to nix-expr
    $HOZ_TEST_CMD https://github.com/pcapriotti/optparse-applicative
    $HOZ_TEST_CMD https://github.com/Gabriel439/Haskell-Foldl-Library

    HOZ_OPTS=doJailbreak
    $HOZ_TEST_CMD -g Gabriel439/Haskell-Turtle-Library
    $HOZ_TEST_CMD -g Gabriel439/Haskell-Managed-Library

    popd > /dev/null
}

setup_test
