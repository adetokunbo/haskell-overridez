#!/bin/bash
set -euo pipefail

setup_test() {
    set -x
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    pushd $this_dir > /dev/null

    HOZ_OPTS=dontCheck
    haskell-overridez -g pcapriotti/optparse-applicative
    HOZ_OPTS=doJailbreak
    haskell-overridez -g Gabriel439/Haskell-Turtle-Library
    haskell-overridez -g Gabriel439/Haskell-Foldl-Library

    popd > /dev/null
    set +x
}

setup_test
