#!/bin/bash
set -euo pipefail

setup_test() {
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    pushd $this_dir > /dev/null

    HOZ_OPTS=dontCheck
    haskell-overridez https://github.com/pcapriotti/optparse-applicative
    HOZ_OPTS=doJailbreak
    haskell-overridez https://github.com/Gabriel439/Haskell-Turtle-Library
    haskell-overridez https://github.com/Gabriel439/Haskell-Foldl-Library
    haskell-overridez https://github.com/Gabriel439/Haskell-Managed-Library

    popd > /dev/null
}

setup_test
