#!/bin/bash
set -euo pipefail

setup_test() {
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    pushd $this_dir > /dev/null

    $HOZ_TEST_CMD --flag-override DontCheck cabal://optparse-applicative-0.14.2.0

    $HOZ_TEST_CMD --flag-override DontHaddock cabal://foldl-1.4.5

    $HOZ_TEST_CMD --flag-override DontCheck --flag-override DontHaddock --flag-override DoJailbreak -g Gabriel439/Haskell-Turtle-Library

    $HOZ_TEST_CMD -g Gabriel439/Haskell-Managed-Library

    popd > /dev/null
}

setup_test
