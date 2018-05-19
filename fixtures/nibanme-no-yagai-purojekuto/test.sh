#!/bin/bash

test() {
    local invalid_fetch=https://github.com/adetokunbo/haskell-overridez
    haskell-overridez fetch $invalid_fetch || return 0
    test_desc="$(cat DESC)"
    echo
    echo "FAILED: '$test_desc'"
    echo
    return 1
}

test
