#!/bin/bash
set -euo pipefail

setup_test() {
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    pushd $this_dir > /dev/null

    export HOZ_OPTS=dontCheck:ignore
    $HOZ_TEST_CMD cabal://optparse-applicative-0.14.2.0

    export HOZ_OPTS=doJailbreak:dontHaddock
    $HOZ_TEST_CMD cabal://foldl-1.3.7

    export HOZ_OPTS=doJailbreak:dontCheck:dontHaddock
    $HOZ_TEST_CMD -g Gabriel439/Haskell-Turtle-Library

    export HOZ_OPTS=ignore:andIgnore:reallyIgnore
    $HOZ_TEST_CMD -g Gabriel439/Haskell-Managed-Library

    unset HOZ_OPTS
    popd > /dev/null
}

setup_test
