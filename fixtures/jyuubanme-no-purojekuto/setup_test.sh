#!/bin/bash
set -euo pipefail

setup_test() {
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    pushd $this_dir > /dev/null

    export HOZ_OPTS=dontCheck:ignore
    haskell-overridez cabal://optparse-applicative-0.14.2.0

    export HOZ_OPTS=doJailbreak:dontHaddock
    haskell-overridez cabal://foldl-1.3.7

    export HOZ_OPTS=doJailbreak:dontCheck:dontHaddock
    haskell-overridez -g Gabriel439/Haskell-Turtle-Library

    export HOZ_OPTS=ignore:andIgnore:reallyIgnore
    haskell-overridez -g Gabriel439/Haskell-Managed-Library

    unset HOZ_OPTS
    popd > /dev/null
}

setup_test
